#!/bin/bash

sudo systemctl stop shiny-server

# Stop any running openstats-app container
var=$(docker container ls | grep 'openstats-app' | awk '{print $1}')
if [ -n "$var" ]; then
  docker stop "$var"
fi

# Build the Docker image
docker build -t openstats-app .

# Run the container
docker run --rm -p 3838:3838 openstats-app
