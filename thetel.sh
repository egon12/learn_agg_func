#!/bin/sh
tmux new-session -d 'nvim theslide.md'
tmux split-window -h 'sqlite3 mydb.db'
tmux split-window -v 'node'
tmux split-window -v 'ipython'
tmux -2 attach-session -d
