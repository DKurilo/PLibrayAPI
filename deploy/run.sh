#!/bin/bash

# Generate self signed certificate
# openssl req -new -newkey rsa:4096 -x509 -sha256 -days 365 -nodes -out cert.pem -keyout key.pem

# Copy certificate into docker container
# docker cp ./key.pem dkurilo/library-api:/var/build/key.pem
# docker cp ./cert.pem dkurilo/library-api:/var/build/cert.pem

# Run container
docker run -t -i -d -e POSTGRES_CONN="postgresql://library@host.docker.internal/library" -p 8080:8080 dkurilo/library-api
