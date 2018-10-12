# CREATED   1 Aug 2018
# MODIFIED 12 Oct 2018

# PURPOSE simulate a pulse fishery (i.e. catches taken instantaneously)
#         in order to test CASAL2

# Project path
# ProjDir = "/home/mkienzle/mystuff/Work/NIWA/LearningCASAL2/TestCASAL2_withSimulatedData"

# Useful functions
source("../ForLinux/SimulationFunctions.R")

###############################
# PARAMETERS OF THE SIMULATION
###############################

# Dimensions of the problem
max.age = 12
nb.of.preexploitation.years = 50
nb.of.years.of.fishing = 20

# Parameters of the model
param <- c("Simulated.recruitment" = round(runif(1, min = 1e5, max = 1e7),0))
cat(paste(as.numeric(param["Simulated.recruitment"]), ",", sep=""), file = "Results/SimVsEst.csv", append = TRUE)

########################################################################
# Simulate a pulse removal fishery: catches are removed instantaneously
########################################################################

sim <-  Simulate.pulsefishing.Fishery(max.age = max.age,  nb.of.preExploitation.years = nb.of.preexploitation.years, nb.of.exploitation.years = nb.of.years.of.fishing, exploitation.rate = c(0.2, 0.8), nat.mort.range = c(0.4, 0.4), recruitment.range=c(param["Simulated.recruitment"], param["Simulated.recruitment"]), verbose = FALSE)

# Plot simulated dynamics of population
source("../ForLinux/PlotDynamic.R")

# Output synthetic data into a CASAL2 model
source(paste("../..", "Rlibrary", "OutputModelInCasal2Format.R", sep = "/"))
source(paste("../..", "Rlibrary", "OutputObservationsInCasal2Format.R", sep = "/"))

########################################################################
# DUMMY OUTPUT in place of survival analysis
########################################################################
cat(paste(NA, ",", sep=""), file = "Results/SimVsEst.csv", append = TRUE)

########################################################################
## What is the estimate using R (and simply a regression to the survey)
########################################################################
source("../ForLinux/EstimateRecruitmentInR.R")
cat(paste(round(Roptimize.res$minimum,0), ",", sep=""), file = "Results/SimVsEst.csv", append = TRUE)

########################################################################
# Estimate the parameter with CASAL2
########################################################################
system2(command = "casal2", args = "-e -i Recruitment_startingValue.txt", stdout = "tmpfile.txt")
casal2.est <- as.numeric(readLines("tmpfile.txt")[grep('estimate_value', readLines("tmpfile.txt"))+3])
cat(paste(casal2.est, "\n", sep=""), file = "Results/SimVsEst.csv", append = TRUE)

