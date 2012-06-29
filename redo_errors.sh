#!/bin/bash

for i in `grep 'exit stats' queued.out |grep -v ' 0 '|cut -f2 '-d '`; do
  grep $i queued.out ;
done | grep Starting |cut -f2 "-d'" | sed 's/\\n/\&/' #| bash
