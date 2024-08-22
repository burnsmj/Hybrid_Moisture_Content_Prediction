#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --mem=100gb
#SBATCH --time=01:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=burns756@umn.edu
#SBATCH -o /home/hirschc1/burns756/Hybrid_NMC/%j.out
#SBATCH -e /home/hirschc1/burns756/Hybrid_NMC/%j.err

# This script is to create a dataset that is a subset of 10 files.
# First the header will need to be collected from file 1, then the data from the other 9 files will be appended to the header.
less analysis/ld_decay/inbreds_geno_Chr_1.ld.gz | head -n 1 > analysis/ld_decay/inbreds_geno_sample.ld

# Regardless of sample size, the sampling time seems to take about 1 min and 45 seconds.

# Sample 500000 lines from each file and append to the header.
for i in {1..10}; do
    echo "Sampling Chromosome ${i}"
    less analysis/ld_decay/inbreds_geno_Chr_${i}.ld.gz | shuf -n 500000 >> analysis/ld_decay/inbreds_geno_sample.ld
done

# Check how many lines are in the new file.
wc -l analysis/ld_decay/inbreds_geno_sample.ld

# Check to make sure no additional headers were added to the file.
grep "CHR_A" analysis/ld_decay/inbreds_geno_sample.ld