#!/bin/bash

# CREATED  13 August 2018
# MODIFIED 14 August 2018

# PURPOSE simulated data x times using a different recruitment value each time
#         estimate recruitment using CASAL2
#         Compare the results

# Create a file to store the results
file="Results/SimVsEst.csv"
if [ -f "$file" ]
then
	rm $file; touch $file;
else
	touch $file;
fi

# Loop over x times simulation and estimation process
for i in `seq 1 100`; do
# Simulated data, generate CASAL2 input files
Rscript SimulationStudy.R >> $file

# Estimate parameters with CASAL2
#casal2_adolc -e
casal2_adolc -e -i Recruitment_startingValue.txt | grep -A 3 "*estimate_value" | tail -1 >> $file
done

# Plot results
Rscript PlotResults.R

