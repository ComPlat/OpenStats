#!/bin/bash

sudo systemctl stop shiny-server

# Stop any running openstats-app container
var=$(docker container ls | grep 'openstats' | awk '{print $1}')
if [ -n "$var" ]; then
  docker stop "$var"
fi

# Build the Docker image
docker build -t openstats .

# Run the container
docker run --rm -p 127.0.0.1:3838:3838 openstats
