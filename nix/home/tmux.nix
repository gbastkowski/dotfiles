{ ... }:
{
  programs.tmux = {
    enable = true;
    prefix = "C-a";
    baseIndex = 1;
    escapeTime = 1;
    historyLimit = 10000;
    mouse = true;
    keyMode = "vi";
    terminal = "xterm-256color";

    extraConfig = ''
      bind C-a send-prefix
      unbind C-b

      bind-key r source-file ~/.tmux.conf \; display-message "tmux config reloaded."

      setw -g pane-base-index 1
      set -g allow-passthrough on

      setw -g monitor-activity on
      set -g visual-activity on

      bind Escape copy-mode
      unbind p
      bind p paste-buffer

      bind-key -T copy-mode-vi 'v' send -X begin-selection
      bind-key -T copy-mode-vi 'y' send -X copy-selection
      bind-key -T copy-mode-vi 'Space' send -X halfpage-down
      bind-key -T copy-mode-vi 'BSpace' send -X halfpage-up

      bind C-c run "tmux safe-buffer - | xclip -i sel clipboard"
      bind C-v run "tmux set-buffer \"$(xclip -o -sel clipboard)\"; tmux paste-buffer"

      bind / split-pane -h
      bind - split-pane -v
      unbind '"'
      unbind %

      bind h select-pane -L
      bind j select-pane -D
      bind k select-pane -U
      bind l select-pane -R

      bind -r C-h select-window -t :-
      bind -r C-l select-window -t :+

      bind -r H resize-pane -L 5
      bind -r J resize-pane -D 5
      bind -r K resize-pane -U 5
      bind -r L resize-pane -R 5
    '';
  };
}
