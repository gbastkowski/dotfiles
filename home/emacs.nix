{ lib, inputs, ... }:
{
  home.file.".local/bin/emacs-capture.sh" = { source = ../bin/emacs-capture.sh; executable = true; };
  home.file.".local/bin/emacs-upgrade.sh" = { source = ../bin/emacs-upgrade.sh; executable = true; };
  home.file.".local/bin/metals-emacs"     = { source = ../bin/metals-emacs;     executable = true; };

  home.file.".doom.d/cheatsheet.css".source = ../emacs/.doom.d/cheatsheet.css;
  home.file.".doom.d/cheatsheet.html".source = ../emacs/.doom.d/cheatsheet.html;
  home.file.".doom.d/cheatsheet.org".source = ../emacs/.doom.d/cheatsheet.org;
  home.file.".doom.d/config.org".source = ../emacs/.doom.d/config.org;
  home.file.".doom.d/init.el".source = ../emacs/.doom.d/init.el;
  home.file.".doom.d/ispell.pws".source = ../emacs/.doom.d/ispell.pws;
  home.file.".doom.d/packages.el".source = ../emacs/.doom.d/packages.el;
  home.file.".doom.d/modules/tools/ai/config.el".source = ../emacs/.doom.d/modules/tools/ai/config.el;
  home.file.".doom.d/modules/tools/ai/packages.el".source = ../emacs/.doom.d/modules/tools/ai/packages.el;
  home.file.".doom.d/modules/tools/claude-code/config.el".source = ../emacs/.doom.d/modules/tools/claude-code/config.el;
  home.file.".doom.d/modules/tools/claude-code/functions.el".source = ../emacs/.doom.d/modules/tools/claude-code/functions.el;
  home.file.".doom.d/modules/tools/claude-code/packages.el".source = ../emacs/.doom.d/modules/tools/claude-code/packages.el;
  home.file.".doom.d/modules/tools/codex/config.el".source = ../emacs/.doom.d/modules/tools/codex/config.el;
  home.file.".doom.d/modules/tools/codex/packages.el".source = ../emacs/.doom.d/modules/tools/codex/packages.el;
  home.file.".doom.d/openai/playground/openai-playground.el".source = ../emacs/.doom.d/openai/playground/openai-playground.el;
  home.file.".doom.d/openspec/changes/find-or-create-task-refile-function/.openspec.yaml".source = ../emacs/.doom.d/openspec/changes/find-or-create-task-refile-function/.openspec.yaml;
  home.file.".doom.d/openspec/changes/find-or-create-task-refile-function/design.md".source = ../emacs/.doom.d/openspec/changes/find-or-create-task-refile-function/design.md;
  home.file.".doom.d/openspec/changes/find-or-create-task-refile-function/proposal.md".source = ../emacs/.doom.d/openspec/changes/find-or-create-task-refile-function/proposal.md;
  home.file.".doom.d/openspec/changes/find-or-create-task-refile-function/specs/task-refile-to-today/spec.md".source = ../emacs/.doom.d/openspec/changes/find-or-create-task-refile-function/specs/task-refile-to-today/spec.md;
  home.file.".doom.d/openspec/changes/find-or-create-task-refile-function/tasks.md".source = ../emacs/.doom.d/openspec/changes/find-or-create-task-refile-function/tasks.md;
  home.file.".doom.d/scala/customs/scala-customs.el".source = ../emacs/.doom.d/scala/customs/scala-customs.el;
  home.file.".doom.d/snippets/openai/openai.http".source = ../emacs/.doom.d/snippets/openai/openai.http;
  home.file.".doom.d/snippets/openai/openai.org".source = ../emacs/.doom.d/snippets/openai/openai.org;
  home.file.".doom.d/themes/gunnar-theme.el".source = ../emacs/.doom.d/themes/gunnar-theme.el;
  home.file.".emacs-profile".source = ../emacs/.emacs-profile;
  home.file.".emacs-profiles.el".source = ../emacs/.emacs-profiles.el;

  home.file.".emacs.d/init.el".source = "${inputs.chemacs2}/init.el";
  home.file.".emacs.d/early-init.el".source = "${inputs.chemacs2}/early-init.el";
  home.file.".emacs.d/chemacs.el".source = "${inputs.chemacs2}/chemacs.el";

  programs.zsh = {
    sessionVariables = {
      EDITOR = "emacsclient -t";
    };
    initContent = lib.mkAfter ''
      [[ -d $HOME/.emacs.doom/bin ]] && path_append "$HOME/.emacs.doom/bin"
    '';
  };
}
