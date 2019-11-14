FROM node:latest

EXPOSE 1234
EXPOSE 12345
WORKDIR /home/node/factor

USER 1000

COPY "./docker.bash" "/usr/local/bin"
ENTRYPOINT ["/usr/local/bin/docker.bash"]
