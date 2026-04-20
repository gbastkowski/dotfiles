{ ... }:
{
  programs.home-manager.enable = true;

  home.stateVersion = "25.11";

  home.file.".p10k.zsh".source = ../../zsh/.p10k.zsh;

  programs.git = {
    enable = true;
    lfs.enable = true;
    settings = {
      user.name = "Gunnar Bastkowski";
      user.email = "gunnar.bastkowski@ista.com";
      pull.rebase = true;
      merge.conflictstyle = "diff3";
      push.followTags = true;
      github.user = "gbastkowski";
      advice.detachedHead = false;
      init.defaultBranch = "main";
    };
    ignores = [
      "*.elc"
      "auto-save-list"
      "tramp"
      ".#*"
      ".bloop/"
      ".classpath"
      "/GPATH"
      "/GRTAGS"
      "/GTAGS"
      ".idea/"
      "/keybase/"
      "*.log"
      ".metals/"
      ".project"
      "/.scala_history"
      ".settings/"

      # Org-mode
      ".org-id-locations"
      "*_archive"

      # flymake-mode
      "*_flymake.*"

      # eshell files
      "/eshell/history"
      "/eshell/lastdir"

      # elpa packages
      "/elpa/"

      # reftex files
      "*.rel"

      "*.7z"
      "*.dmg"
      "*.gz"
      "*.iso"
      "*.jar"
      "!gradle-wrapper.jar"
      "*.rar"
      "*.tar"

      "*.log"
      "*.sqlite"

      # OS generated files
      ".DS_Store"
      ".DS_Store?"
      "._*"
      ".Spotlight-V100"
      ".Trashes"
      "ehthumbs.db"
      "Thumbs.db"

      ".*.swp"

      "TODOs.org"
      "/local.sbt"
      "/project/metals.sbt"
      "**/project/metals.sbt"
      "/.bsp/"
      "/TAGS"
      "*.~undo-tree~"
      "/.dir-locals.el"

      # Local overrides
      "/*.local.org"
      "/*.local.md"

      # Claude runtime state
      "**/.claude/settings.local.json"
      "/.claude/"
      "/claude/debug/"
      "/claude/file-history/"
      "/claude/history.jsonl"
      "/claude/projects/"
      "/claude/shell-snapshots/"
      "/claude/statsig/"
      "/claude/todos/"
    ];
  };
}
