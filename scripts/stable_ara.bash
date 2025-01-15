#!/bin/bash

until python aranet_server.py 5004; do
    echo "The script crashed with exit code $?. Restarting..." >&2
    sleep 1
done
