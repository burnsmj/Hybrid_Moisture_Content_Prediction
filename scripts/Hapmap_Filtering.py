#!/usr/bin/env python3
### Hapmap Filtering
### Michael Burns
### 10/24/23

# This will be a python script to filter a hapmap dataset (line-by-line) for minor allele frequency, heterozygosity, and missing dataset

# Read in the hapmap file line by line
# For each line, check the following:
#	1) Minor allele frequency
#	2) Heterozygosity
#	3) Missing data
# If any of these are above the threshold, continue to the next line
# Those that are below the threshold will be written to a file
# Initiate a counter to keep track of the number of lines
tracker = 0

# Open connection to output file to write to
with open("/Users/michael/Desktop/Grad_School/Research/Hybrid_NMC/Data/snp_subset_filtered.hmp.txt", "w") as output_file:  
    # Open connection to file to read in
    with open("/Users/michael/Desktop/Grad_School/Research/Hybrid_NMC/Data/snp_subset.hmp.txt", "r") as hapmap_file:
        # Read in the file line by line
        for line in hapmap_file:
            #print(line) # Debugging
            # Add to the tracker with each line
            tracker += 1
            # Print the tracker to the screen if it is divisible by 50000
            if tracker % 50000 == 0:
                print('Working on line: ' + str(tracker))
            # Write the header to the output file
            if line.startswith("rs"):
                output_file.write(line)
                continue
            # Split line and extract only the genotype values
            genotypes = line.strip('\n').split('\t')[11:]
            # Determine Missing/Heterozygous Frequency (values that are not AA, TT, GG, or CC)
            if sum([1 for genotype in genotypes if genotype not in ['AA', 'TT', 'GG', 'CC']]) / len(genotypes) > 0.05:
                continue
            # Extract the alleles
            alleles = line.strip('\n').split('\t')[1].split("/")
            # Calculate the minor allele frequency of the first allele
            first_allele_freq = "".join(genotypes).count(alleles[0]) / (len(genotypes)*2)
            # Determine if the allele frequency is less than 0.05 or greater than 0.95
            if first_allele_freq < 0.05 or first_allele_freq > 0.95:
                continue
            # Print the alleles
            # print(alleles[0], str(first_allele_freq)) # Debugging
            # Write the line to the output file
            output_file.write(line)

# Close the connection to the files
hapmap_file.close()
output_file.close()

