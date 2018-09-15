# CREATED   1 Aug 2018
# MODIFIED  7 Sep 2018

# PURPOSE simulate a pulse fishery (i.e. catches taken instantaneously)
#         in order to test CASAL2

# Useful functions
source("SimulationFunctions.R")

###############################
# PARAMETERS OF THE SIMULATION
###############################

# Dimensions of the problem
max.age = 12
nb.of.preexploitation.years = 50
nb.of.years.of.fishing = 20

# Parameters of the model
param <- c("Simulated.recruitment" = round(runif(1, min = 1e5, max = 1e7),0))
cat(paste(as.numeric(param["Simulated.recruitment"]), ",", sep=""))

########################################################################
# Simulate a pulse removal fishery: catches are removed instantaneously
########################################################################

sim <-  Simulate.pulsefishing.Fishery(max.age = max.age,  nb.of.preExploitation.years = nb.of.preexploitation.years, nb.of.exploitation.years = nb.of.years.of.fishing, exploitation.rate = c(0.2, 0.8), nat.mort.range = c(0.4, 0.4), recruitment.range=c(param["Simulated.recruitment"], param["Simulated.recruitment"]), verbose = FALSE)

# Plot simulated dynamics of population
source("PlotDynamic.R")

# Output synthetic data into a CASAL2 model
source("OutputModelInCasal2Format.R")
source("OutputObservationsInCasal2Format.R")

############
# run CASAL2
############
system("casal2_adolc -r > /tmp/output.txt")

####################################################
# Compare simulated data with CASAL2 computed values
####################################################

### Load CASAL2 outputs into a matrix
source("UsefulFunction.R")
library(berryFunctions)

data.at.t1 <- ReadYearCrossAgeMat("year_age_matrix[annual_age_1st4Months]", "/tmp/output.txt")
#all(almost.equal(c(as.matrix(data.at.t1)), c(round(sim$DuringExploitationPop$nb.after.FirstHalfNatMort,0)), tolerance = 1))

data.at.t2 <- ReadYearCrossAgeMat("year_age_matrix[annual_age_2nd4Months]", "/tmp/output.txt")
#all(almost.equal(c(as.matrix(data.at.t2)), c(round(sim$DuringExploitationPop$nb.after.catch,0)), tolerance = 1))

data.at.t3 <- ReadYearCrossAgeMat("year_age_matrix[annual_age_3rd4Months]", "/tmp/output.txt")
#all(almost.equal(c(as.matrix(data.at.t3)), c(round(sim$DuringExploitationPop$nb.after.catch,0)), tolerance = 1))

#system("grep year_age_matrix -A 23 /tmp/output.txt | tail -22 > /tmp/tmp.txt")
#data.tab <- read.table("/tmp/tmp.txt", row.names = 1)

#######################################################################
# Estimates mortality rates from simulated data using survival analysis
#######################################################################
# note that the exploitation rate is converted into fishing mortality using the relation U = 1 - exp(-F) because taking a proportion U of the population is the same than saying 1 - exp(-F) died; U = 1 - exp(-F) <=> F = -log(1-U)

res <- optim(c(1,0.2), local.llfunc3, catch = sim$DuringExploitationPop$catch/10, effort = outer(-log(1-sim$DuringExploitationPop$yearly.exploitation.rate), rep(1,max.age)), selectivity.at.age = rep(1, max.age), catchability.scaling.factor = 1, hessian = TRUE)
errors <- sqrt(diag(solve(res$hessian)))

#print(res)
x <- seq(0,1, length = 100)
y <- rep(NA, length(x))
for(j in 1:length(x)) y[j] <- llfunc3(c(res$par[1], x[j]), catch = sim$DuringExploitationPop$catch / 10, effort = outer(-log(1-sim$DuringExploitationPop$yearly.exploitation.rate), rep(1,max.age)), selectivity.at.age = rep(1, max.age), catchability.scaling.factor = 1)

#pdf(file = "Results/Graphics/SurvivalAnalysis-NatMortLikelihoodProfile.pdf")
plot(x,y, type = "l")
#abline(v = sim$M, lty = 3)
#dev.off()

## Calculate the probabilities associated with each observation using model 2
prob <- prob.llfunc3(res$par, effort = outer(-log(1-sim$DuringExploitationPop$yearly.exploitation.rate), rep(1,max.age)), selectivity.at.age = rep(1, max.age), catchability.scaling.factor = 1)

## Estimate recruitment for each cohort using the ratio of sum of catch over sum of probability of catching (Dupont, 1983)
#est.nb.at.age.in.catch <- outer(estimated.nb.in.catch, rep(1, 6)) * nb.at.age / outer(rowSums(nb.at.age), rep(1, 6))

est.rec <- rowSums(Caaa2Coaa(sim$DuringExploitationPop$catch), na.rm = T) / rowSums(prob, na.rm = T)

#plot(est.rec, col = c(rep(1,length(est.rec)-20), rep(2,20)), pch = 19); abline(h = sim.rec)

# Ouput the average estimate of recruitment
cat(paste(round(mean(est.rec[seq(length(est.rec)-nb.of.years.of.fishing+1, length(est.rec))]),0), ",", sep=""))

## What is the estimate using R (and simply a regression to the survey)
source("EstimateRecruitmentInR.R")
cat(paste(round(Roptimize.res$minimum,0), ",", sep=""))
