#!/usr/bin/env bash

# e.g. ./env.bash /Users/micseydel/obsidian_vaults/deliberate_knowledge_accretion
python -m woke_notes $1 --scripts `realpath scripts/`
