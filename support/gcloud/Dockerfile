FROM google/cloud-sdk:slim@sha256:0b5e8d676210c484217afa10f69e30b9d7250f52bbc09ae164fea1766a377cb3

# docker dependencies
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    apt-transport-https \
    ca-certificates \
    curl \
    gnupg2 \
    software-properties-common \
 && rm -rf /var/lib/apt/lists/* /var/tmp/*

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# delete existing old docker which is copied in from the base image https://github.com/GoogleCloudPlatform/cloud-sdk-docker/issues/178
# cloud-sdk gets bumped often, so it should be fine to install latest docker, rather than pin and bump
RUN rm /usr/local/bin/docker \
 && curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add - \
 && add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/debian \
   $(lsb_release -cs) \
   stable" \
 && apt-get update \
 && apt-get install -y --no-install-recommends docker-ce docker-ce-cli containerd.io \
 && rm -rf /var/lib/apt/lists/* /var/tmp/*
