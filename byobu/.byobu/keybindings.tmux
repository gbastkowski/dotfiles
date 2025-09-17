set -g prefix F12
unbind-key -n C-a

# Vim-like behavior and ergonomics
setw -g mode-keys vi
set -g status-keys vi
set -s escape-time 0

# Pane navigation with prefix + h/j/k/l
bind -r h select-pane -L
bind -r j select-pane -D
bind -r k select-pane -U
bind -r l select-pane -R

# Resize panes with prefix + H/J/K/L
bind -r H resize-pane -L 5
bind -r J resize-pane -D 5
bind -r K resize-pane -U 5
bind -r L resize-pane -R 5

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
