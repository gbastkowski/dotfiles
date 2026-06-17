{ lib, pkgs, ... }:
let
  remote = "ssh-w00d23c2@bastkowski.name:git/password-store";
  gpgIssueUrl = "https://github.com/gbastkowski/dotfiles/issues/35";
in
{
  home.packages = with pkgs; [ pass gnupg ];

  home.activation.bootstrapPasswordStore = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    REMOTE="${remote}"
    STORE="$HOME/.password-store"
    GPG="${pkgs.gnupg}/bin/gpg"
    GIT="${pkgs.git}/bin/git"

    if ! "$GPG" --list-secret-keys --with-colons 2>/dev/null | grep -q '^sec:'; then
      echo "password-store: no GPG secret key found." >&2
      echo "password-store: import or generate a key before activation. See ${gpgIssueUrl}" >&2
      exit 1
    fi

    if [ ! -d "$STORE" ]; then
      echo "password-store: cloning $REMOTE into $STORE"
      "$GIT" clone "$REMOTE" "$STORE"
      exit 0
    fi

    if [ ! -d "$STORE/.git" ]; then
      echo "password-store: $STORE exists but is not a git repo; leaving untouched" >&2
      exit 1
    fi

    CURRENT="$("$GIT" -C "$STORE" remote get-url origin 2>/dev/null || true)"
    if [ -z "$CURRENT" ]; then
      echo "password-store: adding origin -> $REMOTE"
      "$GIT" -C "$STORE" remote add origin "$REMOTE"
    elif [ "$CURRENT" != "$REMOTE" ]; then
      echo "password-store: origin mismatch ($CURRENT != $REMOTE); aborting" >&2
      exit 1
    fi
  '';
}
