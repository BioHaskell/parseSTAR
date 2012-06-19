#!/bin/bash

for i in bmrb_plus_pdb/*.str; do
  echo test/TestConverter $i bmrb_conv/`basename $i .str`.test "+RTS -H2G -A800K";
done  > cmds_all_bmrb.sh

time nice ionice -c3 -- ~/Projects/rfr_new/src/queue_commands.py 4 cmds_all_bmrb.sh  > queued.out 2> queued.err
