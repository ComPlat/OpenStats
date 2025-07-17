#!/bin/bash

docker build -t konradkraemer/openstats:latest .
# docker build --no-cache -t konradkraemer/openstats:latest .

docker push konradkraemer/openstats:latest
