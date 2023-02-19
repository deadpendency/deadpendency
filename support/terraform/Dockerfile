FROM hashicorp/terraform:light@sha256:98821caea83ac03b917feb04b842d7e83d45b425cc560bc3f32e838bf904aa3e

RUN apk add --no-cache \
  bash

# copied from https://github.com/GoogleCloudPlatform/cloud-sdk-docker/blob/master/alpine/Dockerfile
# not sure how to do this without using a self referring env variable
# hadolint ignore=DL3044
ENV PATH /google-cloud-sdk/bin:$PATH

RUN apk --no-cache add \
        curl \
        python3 \
        py3-crcmod \
        py3-openssl \
        bash \
        libc6-compat \
        openssh-client \
        git \
        gnupg \
    && curl -O https://dl.google.com/dl/cloudsdk/release/google-cloud-sdk.tar.gz && \
    tar xzf google-cloud-sdk.tar.gz && \
    rm google-cloud-sdk.tar.gz && \
    gcloud config set core/disable_usage_reporting true && \
    gcloud config set component_manager/disable_update_check true && \
    gcloud config set metrics/environment github_docker_image && \
    gcloud --version
RUN git config --system credential.'https://source.developers.google.com'.helper gcloud.sh
VOLUME ["/root/.config"]

# A hack so I can validate terraform without pulling the secrets
RUN touch /tmp/credentials.json
