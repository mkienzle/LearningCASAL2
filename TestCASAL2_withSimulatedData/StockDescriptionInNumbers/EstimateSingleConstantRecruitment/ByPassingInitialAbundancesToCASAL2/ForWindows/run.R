# CREATED 12 Oct 2018

# PURPOSE a R script to repeat the simulation study X times

# Number of simulations
n <- 100

# Initialise the file that holds the results
cat("", file = "Results/SimVsEst.csv")

for(i in 1:n){
  cat(paste("Performing simulation", i,"\r"))
  source("SimulationStudy.R")
}

# Produce plots
source("PlotResults.R")