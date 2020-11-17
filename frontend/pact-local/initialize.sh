#!/bin/sh
set -e

JSON="Content-Type: application/json"
NETWORK=http://localhost:$1/api/v1/send

if [ -z "$1" ];
then
  echo 'Server port must be supplied; ex: ./initialize-todos.sh 9001'
  exit 1
fi

pact -a pact-local/yaml/load-fungible.yaml | curl -H "$JSON" -d @- $NETWORK
pact -a pact-local/yaml/load-coin.yaml | curl -H "$JSON" -d @- $NETWORK
pact -a pact-local/yaml/load-ns.yaml | curl -H "$JSON" -d @- $NETWORK
pact -a pact-local/yaml/load-tokens.yaml | curl -H "$JSON" -d @- $NETWORK
pact -a pact-local/yaml/load-swap.yaml | curl -H "$JSON" -d @- $NETWORK
pact -a pact-local/yaml/load-exchange.yaml | curl -H "$JSON" -d @- $NETWORK
pact -a pact-local/yaml/create-abc.yaml | curl -H "$JSON" -d @- $NETWORK
pact -a pact-local/yaml/create-xyz.yaml | curl -H "$JSON" -d @- $NETWORK
pact -a pact-local/yaml/create-account.yaml | curl -H "$JSON" -d @- $NETWORK
