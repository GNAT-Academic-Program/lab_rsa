#!/bin/bash

SESSION_NAME="RSA"

# Start a new session with a single window
tmux new-session -d -s $SESSION_NAME

# Split the window into three panes
tmux split-window -v -t $SESSION_NAME:0.0
tmux split-window -v -t $SESSION_NAME:0.1

# Apply the even-vertical layout
tmux select-layout -t $SESSION_NAME:0 even-vertical

# Send commands to each pane
tmux send-keys -t $SESSION_NAME:0.0 './server_main' C-m
tmux send-keys -t $SESSION_NAME:0.1 './client_main' C-m
tmux send-keys -t $SESSION_NAME:0.2 './client_main' C-m

# Attach to the session
tmux attach -t $SESSION_NAME


