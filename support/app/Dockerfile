FROM debian:buster-slim@sha256:5dbce817ee72802025a38a388237b0ea576aa164bc90b7102b73aa42fef4d713

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
    ca-certificates \
    libgmp10 \
    libnuma1 \
 && rm -rf /var/lib/apt/lists/*

RUN groupadd -r appuser && useradd -r -m -g appuser appuser

COPY apps/resource /app/resource

# above is pre cached

ARG APP_EXE_ARG

COPY "dist/$APP_EXE_ARG" /app/exe/app-exe

# ci artifacts won't be executable, so need this to ensure it can be run
RUN chmod a+x /app/exe/app-exe

RUN chown -R appuser:appuser /app

USER appuser

WORKDIR /app/exe

ARG BUILD_VERSION_ARG
ENV BUILD_VERSION=$BUILD_VERSION_ARG

ENTRYPOINT [ "/app/exe/app-exe" ]
