#!/bin/bash

#rm Rscripts_to_run.txt

# Example Usage: sh Array_Scripts_Creator.sh ML_Training_Array.csv 'Rscript ML_Model_Run_Array.R' Rscripts_to_run.txt

lines=$(wc -l $1 | cut -d " " -f 1) # Count number of lines in the file
lines=$((lines-1)) # Reduce number of lines since we won't index the header

for(( c=1; c<=$lines; c++ )); do
	echo $2 $c >> $3
done