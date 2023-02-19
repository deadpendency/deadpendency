FROM nginx:1@sha256:e209ac2f37c70c1e0e9873a5f7231e91dcd83fdf1178d8ed36c2ec09974210ba

COPY nginx.conf /etc/nginx/conf.d/configfile.template
COPY build_conf.sh /scripts/build_conf.sh

ENTRYPOINT [ "/scripts/build_conf.sh" ]
CMD ["nginx", "-g", "daemon off;"]
