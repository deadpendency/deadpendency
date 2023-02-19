#!/bin/sh

export PORT
export APP_LIVE_HOST

envsubst '$PORT $APP_LIVE_HOST' < /etc/nginx/conf.d/configfile.template > /etc/nginx/conf.d/default.conf

exec "$@"
