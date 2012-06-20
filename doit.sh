#!/bin/bash

OUTDIR=bmrb_conv
NPROC=4

# Prepare flawed inputs
sed --in-place "s/'Linge, O'Donoghue and Nilges'/\"Linge, O'Donoghue and Nilges\"/" bmrb_plus_pdb/*.str

mkdir $OUTDIR
for i in bmrb_plus_pdb/*.str; do
  OUT=$OUTDIR/`basename $i .str`.cs;	 
  #OUT=/tmp/`basename $i .str`.test;	 
  #echo "test/TestConverter $i $OUT +RTS -H2G -A800K -RTS; rm $OUT";
  echo "test/TestChemShifts $i $OUT +RTS -H2G -A800K -RTS";
done  > cmds_all_bmrb.sh

#time nice ionice -c3 -- ~/Projects/rfr_new/src/queue_commands.py 4 cmds_all_bmrb.sh  > queued.out 2> queued.err
#for i in bmrb_plus_pdb/*.str; do
#  #OUT=bmrb_conv/`basename $i .str`.test;	 
#  OUT=/tmp/`basename $i .str`.test;	 
#  echo "test/TestConverter $i $OUT +RTS -H2G -A800K -RTS; rm $OUT";
#done  > cmds_all_bmrb.sh

time nice ionice -c3 -- ~/Projects/rfr_new/src/queue_commands.py $NPROC cmds_all_bmrb.sh  > queued.out 2> queued.err
