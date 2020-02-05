#!/bin/bash

# Generate self signed certificate
# openssl req -new -newkey rsa:4096 -x509 -sha256 -days 365 -nodes -out cert.pem -keyout key.pem

# Copy certificate into docker container
# docker cp ./key.pem dkurilo/library-api:/var/build/key.pem
# docker cp ./cert.pem dkurilo/library-api:/var/build/cert.pem

# Run container
docker run -t -i -e POSTGRES_CONN="postgresql://library@host.docker.internal/library" --name LibraryAPI dkurilo/library-api
