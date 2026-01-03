#!/bin/bash
# Wrapper for logcli with Grafana Cloud credentials
# Usage: ./logcli.sh query '{service_name="tidepool"}' --limit=10

export LOKI_ADDR="https://logs-prod-021.grafana.net"
export LOKI_USERNAME="1432480"
export LOKI_PASSWORD="$(cat "$(dirname "$0")/.grafana-cloud-token")"

exec logcli "$@"
