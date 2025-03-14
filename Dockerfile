FROM caddy
COPY Caddyfile /etc/caddy/Caddyfile
COPY dist /srv