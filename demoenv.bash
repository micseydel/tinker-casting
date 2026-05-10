#!/usr/bin/env bash

# needs to be an absolute path
export vaultRoot="/Users/micseydel/obsidian_vaults/litterdemovault/litterdemovault"
export tinkerbrainPort=5103

#thanks to https://stackoverflow.com/a/21433585/1157440 - for the `; set javaOptions` mandatory kludge
cmd_str="sbt '; set javaOptions += "-Dlogfolder=litter_demo_logs" ; runMain me.micseydel.actor.kitties.LitterBoxesHelperDemo'"
echo "Running: $cmd_str"
eval "$cmd_str"
