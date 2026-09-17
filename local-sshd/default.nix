{ pkgs, config, ... }:
let
  port = "2222";
  listen = "127.0.0.1";
  stateDir = "${config.home.homeDirectory}/.ssh/local-sshd";

  hostKey = "${stateDir}/ssh_host_ed25519_key";
  authKeys = "${stateDir}/authorized_keys";
  sshdConfig = "${stateDir}/sshd_config";

  # An unprivileged sshd cannot change uid, so it only ever accepts logins for
  # the user running it — which is the point here: sessions land in the real
  # account with the real home. Remote Login (the system service) is disabled
  # by an MDM profile on this host; this is a plain user process instead.
  #
  # StrictModes is off because sshd cannot verify ownership of files it does
  # not own as root. Safe only while bound to localhost, as it is here.
  startScript = pkgs.writeShellScript "local-sshd" ''
    set -euo pipefail

    mkdir -p ${stateDir}
    chmod 0700 ${stateDir}

    [ -f ${hostKey} ] || ${pkgs.openssh}/bin/ssh-keygen -q -t ed25519 -N "" -f ${hostKey}

    if [ ! -f ${authKeys} ]; then
      cp "$HOME/.ssh/id_ed25519.pub" ${authKeys}
    fi
    chmod 0600 ${authKeys}

    cat > ${sshdConfig} <<EOF
    Port ${port}
    ListenAddress ${listen}
    HostKey ${hostKey}
    AuthorizedKeysFile ${authKeys}
    PidFile ${stateDir}/sshd.pid
    PasswordAuthentication no
    KbdInteractiveAuthentication no
    PermitEmptyPasswords no
    PubkeyAuthentication yes
    UsePAM no
    StrictModes no
    X11Forwarding no
    PrintMotd no
    EOF

    # OpenSSH 9.8+ splits per-connection work into helper binaries; point the
    # system sshd at the system helpers explicitly.
    [ -x /usr/libexec/sshd-session ] && echo "SshdSessionPath /usr/libexec/sshd-session" >> ${sshdConfig}
    [ -x /usr/libexec/sshd-auth ] && echo "SshdAuthPath /usr/libexec/sshd-auth" >> ${sshdConfig}

    chmod 0600 ${sshdConfig}

    # Use the system sshd: it is signed and entitled for macOS auth APIs, and
    # the config above pins every path it needs.
    exec /usr/sbin/sshd -D -e -f ${sshdConfig}
  '';
in
{
  launchd.agents.local-sshd = {
    enable = true;
    config = {
      ProgramArguments = [ "/bin/sh" "-lc" "${startScript}" ];
      RunAtLoad = true;
      KeepAlive = true;
      StandardOutPath = "/tmp/local-sshd.log";
      StandardErrorPath = "/tmp/local-sshd.err";
    };
  };
}
