#!/usr/bin/env python3
"""Seed ista's OmO Native defaults without storing the API key."""

import json
import os
from pathlib import Path
import sys
import tempfile


def configure(agent_dir, key_command):
    agent_dir = Path(agent_dir)
    if (agent_dir / "settings.jsonc").exists():
        raise ValueError("settings.jsonc overrides settings.json; reconcile it first")

    updates = {
        "settings.json": {
            "defaultProvider": "openai",
            "defaultModel": "gpt-6.1-sol",
        },
        "auth.json": {
            "openai": {"type": "api_key", "key": key_command},
        },
    }
    pending = []
    # Validate both files before modifying either.
    for name, values in updates.items():
        path = agent_dir / name
        current = json.loads(path.read_text()) if path.exists() else {}
        if not isinstance(current, dict):
            raise ValueError(f"{path} must contain a JSON object")
        merged = {**current, **values}
        if merged != current:
            pending.append((path, merged))

    agent_dir.mkdir(parents=True, exist_ok=True, mode=0o700)
    for path, values in pending:
        fd, temporary = tempfile.mkstemp(prefix=f".{path.name}-", dir=agent_dir)
        try:
            with os.fdopen(fd, "w") as stream:
                json.dump(values, stream, indent=2)
                stream.write("\n")
            os.replace(temporary, path)
        finally:
            if os.path.exists(temporary):
                os.unlink(temporary)


if __name__ == "__main__":
    configure(*sys.argv[1:])
