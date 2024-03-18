#!/bin/bash

# Define session name
SESSION_NAME="Run_RSA_Lab"

# Start a new tmux session
tmux has-session -t $SESSION_NAME 2>/dev/null

if [ $? != 0 ]; then
    tmux new-session -d -s $SESSION_NAME

    # Split the window into three panes
    tmux split-window -v -t $SESSION_NAME
    tmux split-window -v -t $SESSION_NAME
    tmux select-layout -t $SESSION_NAME even-vertical

    # Optionally, send commands to each pane
    tmux send-keys -t $SESSION_NAME:0.1 './server_main' C-m
    tmux send-keys -t $SESSION_NAME:0.0 './client_main' C-m
    tmux send-keys -t $SESSION_NAME:0.2 './client_main' C-m
fi

# Attach to the session
tmux attach -t $SESSION_NAME