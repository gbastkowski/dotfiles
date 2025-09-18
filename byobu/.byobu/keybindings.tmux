set -g prefix C-b
set -g prefix2 C-a
unbind-key -n F12
bind C-b send-prefix
bind C-a send-prefix

set -g xterm-keys on

# Make F1 mirror the standard tmux help prompt
unbind-key -n F1
bind -n F1 list-keys

# Remove reliance on Byobu's function key defaults
unbind-key -n F2
unbind-key -n F3
unbind-key -n F4
unbind-key -n F5
unbind-key -n F6
unbind-key -n F7
unbind-key -n F8
unbind-key -n F9

# Vim-like behavior and ergonomics
setw -g mode-keys vi
set -g status-keys vi
set -s escape-time 0

# Pane navigation with prefix + h/j/k/l
bind -r h select-pane -L
bind -r j select-pane -D
bind -r k select-pane -U
bind -r l select-pane -R

# Pane navigation without prefix using Alt + h/j/k/l
bind -n M-h select-pane -L
bind -n M-j select-pane -D
bind -n M-k select-pane -U
bind -n M-l select-pane -R

# Resize panes with prefix + H/J/K/L
bind -r H resize-pane -L 5
bind -r J resize-pane -D 5
bind -r K resize-pane -U 5
bind -r L resize-pane -R 5

# Window navigation similar to Vim tabs
bind -r '<' previous-window
bind -r '>' next-window
bind ` last-window

# Splits using Vim-inspired keys
bind s split-window -v
bind v split-window -h

# Doom Emacs-style leader key (prefix + Space)
bind Space switch-client -T space-leader
bind -T space-leader Space switch-client -T root
bind -T space-leader c new-window \; switch-client -T root
bind -T space-leader w choose-window \; switch-client -T root
bind -T space-leader s split-window -v \; switch-client -T root
bind -T space-leader v split-window -h \; switch-client -T root
bind -T space-leader x kill-pane \; switch-client -T root
bind -T space-leader d detach-client \; switch-client -T root
bind -T space-leader ? list-keys -T space-leader \; switch-client -T root

# Copy-mode (vi table)
bind -T copy-mode-vi v send -X begin-selection
bind -T copy-mode-vi V send -X select-line
bind -T copy-mode-vi C-v send -X rectangle-toggle
bind -T copy-mode-vi y send -X copy-selection-and-cancel

# Paste and clipboard integration
bind p paste-buffer
set -g set-clipboard on

# Indexing quality-of-life
set -g base-index 1
setw -g pane-base-index 1

# Allow quick detach without function keys
bind -n M-d detach-client
