#!/usr/bin/env python3
"""Offline tests: no upgrades or model requests are executed."""

import contextlib
import importlib.util
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("review", ROOT / "bin/system-upgrade-review.py")
review = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(review)


class ReviewTests(unittest.TestCase):
    def test_redaction(self):
        cases = [
            "https://user:private-value@example.invalid/repo",
            "Authorization: Bearer private-value",
            "password=private-value",
            '"api_key": "private-value"',
            "https://example.invalid?access_token=private-value&ok=1",
            "-----BEGIN OPENSSH PRIVATE KEY-----\nprivate-value\n-----END OPENSSH PRIVATE KEY-----",
            "ghp_abcdefghijklmnopqrstuvwxyz",
            "sk-proj-abcdefghijklmnopqrstuvwxyz",
            "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiIxIn0.signature",
        ]
        for value in cases:
            with self.subTest(value=value):
                result = review.redact(value)
                self.assertNotIn("private-value", result)
                self.assertIn("REDACTED", result)
        with patch.dict(os.environ, {"EXAMPLE_API_KEY": "unusual-private-value"}):
            self.assertNotIn("unusual-private-value", review.redact("error unusual-private-value"))
        with patch.dict(os.environ, {"GITHUB_MCP_PAT": "opaque-value-without-token-prefix"}):
            self.assertNotIn("opaque-value-without-token-prefix", review.redact("error opaque-value-without-token-prefix"))
        self.assertEqual(review.redact("\x1b[31mfailed\x1b[0m\u202e"), "failed")

    def test_news_filters_read_and_irrelevant_without_marking_read(self):
        with tempfile.TemporaryDirectory() as tmp:
            read_file = Path(tmp) / "home-manager/news-read-ids"
            read_file.parent.mkdir()
            read_file.write_text("read-id\n")
            entries = [
                dict(id="read-id", time="2025-01-01", message="already read"),
                dict(id="new-id", time="2026-01-01", message="new news"),
            ]
            result = subprocess.CompletedProcess([], 0, json.dumps(entries))
            with patch.dict(os.environ, {"XDG_DATA_HOME": tmp}), patch.object(review.subprocess, "run", return_value=result) as run:
                news = review.unread_news(Path("/example"), "ista-dotfiles")
            self.assertIn("new news", news)
            self.assertNotIn("already read", news)
            self.assertIn("e.condition", run.call_args.args[0][-1])
            self.assertEqual(read_file.read_text(), "read-id\n")

    def test_isolated_tool_denied_run_and_exact_input(self):
        def check_run(command, **kwargs):
            self.assertIn("--standalone", command)
            self.assertNotIn("--auto", command)
            self.assertEqual(kwargs["input"], "approved report")
            env = kwargs["env"]
            self.assertEqual(env["OPENCODE_DISABLE_PROJECT_CONFIG"], "true")
            self.assertNotIn("OPENCODE_CONFIG_CONTENT", env)
            config = json.loads((Path(env["OPENCODE_CONFIG_DIR"]) / "opencode.json").read_text())
            self.assertEqual(config["permissions"], [{"action": "*", "resource": "*", "effect": "deny"}])
            self.assertEqual(config["mcp"], {"servers": {}})
            self.assertEqual(config["plugins"], [])
            self.assertEqual(config["share"], "disabled")
            self.assertEqual(config["model"], "openai/test-model")
            return subprocess.CompletedProcess(command, 0)
        with patch.object(review.subprocess, "run", side_effect=check_run):
            review.run_review("approved report", "openai/test-model")

    def test_no_confirmation_no_upload(self):
        with tempfile.TemporaryDirectory() as tmp:
            errors = Path(tmp) / "errors"
            errors.write_text("password=private-value\nfailed")
            argv = ["review", tmp, "ista-dotfiles", str(errors)]
            tty = unittest.mock.mock_open(read_data="n\n")
            with patch("sys.argv", argv), patch.dict(os.environ, {"SYSTEM_UPGRADE_REVIEW_MODEL": "openai/test"}), patch.object(review.shutil, "which", return_value="opencode"), patch.object(review, "unread_news", return_value="news"), patch("builtins.open", tty), patch.object(review, "run_review") as run, contextlib.redirect_stdout(io.StringIO()) as out:
                review.main()
            run.assert_not_called()
            self.assertNotIn("private-value", out.getvalue())
            self.assertIn("nothing was sent", out.getvalue())


class CaptureTests(unittest.TestCase):
    def run_upgrade(self, enabled=True, fail=True, reexec=False):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "flake.nix").write_text("{}")
            scripts = root / "bin"
            scripts.mkdir()
            stubs = root / "stubs"
            stubs.mkdir()
            home = root / "home"
            home.mkdir()
            shutil.copy(ROOT / "bin/system-upgrade.sh", scripts / "system-upgrade.sh")
            (scripts / "system-upgrade.sh").chmod(0o755)
            (scripts / "apply.sh").write_text("#!/bin/sh\nexit 0\n")
            (scripts / "apply.sh").chmod(0o755)
            (scripts / "system-upgrade-review.py").write_text(
                "import os,sys\nfrom pathlib import Path\n"
                "Path(os.environ['CAPTURE']).write_text(Path(sys.argv[3]).read_text())\n"
                "assert sys.argv[2] == 'ista-dotfiles'\n"
                "sys.exit(5) # reviewer failure must not alter upgrade result\n"
            )
            stub = '''#!/bin/bash
case "$(basename "$0")" in
  git)
    if [ "$1" = rev-parse ]; then
      if [ -e "$COUNTER" ]; then echo new; else echo old; touch "$COUNTER"; fi
    elif [ "$1" = pull ]; then echo pull-stderr >&2; exit 3
    fi ;;
  brew)
    echo stdout-never-upload
    if [ "$1" = upgrade ] && [ "$FAIL" = yes ]; then echo failed-stderr >&2; exit 7; fi
    echo successful-stderr-never-upload >&2 ;;
  *) exit 0 ;;
esac
'''
            for name in ("softwareupdate", "brew", "pipx", "npm", "opencode", "git", "doom"):
                p = stubs / name
                p.write_text(stub)
                p.chmod(0o755)
            env = dict(os.environ, PATH=str(stubs) + os.pathsep + os.environ["PATH"],
                       HOME=str(home), TMPDIR=str(root), DOTFILES_HOSTNAME="deess1mac",
                       DOTFILES_DIR=str(root),
                       SYSTEM_UPGRADE_REEXEC="" if reexec else "1", SYSTEM_UPGRADE_FAILED="",
                       SYSTEM_UPGRADE_REVIEW_DIR="", CAPTURE=str(root / "captured"),
                       COUNTER=str(root / "counter"), FAIL="yes" if fail else "no")
            command = ["/bin/bash", str(scripts / "system-upgrade.sh")]
            if enabled:
                command.append("--review")
            result = subprocess.run(command, env=env, capture_output=True, text=True)
            captured = (root / "captured").read_text() if (root / "captured").exists() else None
            self.assertFalse(list(root.glob("dotfiles-upgrade-review.*")))
            return result, captured

    def test_only_failed_stderr_is_retained(self):
        result, captured = self.run_upgrade()
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertIn("failed-stderr", captured)
        self.assertIn("exit 7", captured)
        self.assertNotIn("stdout-never-upload", captured)
        self.assertNotIn("successful-stderr-never-upload", captured)
        self.assertIn("stdout-never-upload", result.stdout)
        self.assertIn("successful-stderr-never-upload", result.stderr)

    def test_disabled_by_default(self):
        result, captured = self.run_upgrade(enabled=False)
        self.assertEqual(result.returncode, 1)
        self.assertIsNone(captured)

    def test_reviewer_failure_does_not_fail_successful_upgrade(self):
        result, captured = self.run_upgrade(fail=False)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(captured, "")

    def test_capture_survives_reexec(self):
        result, captured = self.run_upgrade(reexec=True)
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertIn("pull-stderr", captured)
        self.assertEqual(captured.count("failed-stderr"), 2)


if __name__ == "__main__":
    unittest.main()
