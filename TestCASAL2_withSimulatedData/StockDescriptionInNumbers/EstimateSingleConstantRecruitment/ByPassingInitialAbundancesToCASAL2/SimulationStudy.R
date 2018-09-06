# CREATED  13 November 2014 (approach originally written for survival analysis)
# MODIFIED  5 Sep 2018

# PURPOSE simulate a pulse fishery (i.e. catches taken instantaneously)
#         in order to test CASAL2

# Useful functions
source("SimulationFunctions.R")

###############################
# PARAMETERS OF THE SIMULATION
###############################

# Dimension of the problem
max.age = 12
nb.of.preexploitation.years = 50
nb.of.years.of.fishing = 20

# Parameters of the model
sim.rec <- round(runif(1, min = 1e5, max = 1e7),0)
cat(paste(sim.rec, ",", sep=""))

sim <-  Simulate.pulsefishing.Fishery2(max.age = max.age,  nb.of.preExploitation.years = nb.of.preexploitation.years, nb.of.exploitation.years = nb.of.years.of.fishing, exploitation.rate = c(0.2, 0.8), nat.mort.range = c(0.4, 0.4), recruitment.range=c(sim.rec, sim.rec), log.para.range = c(8,12), log.parb.range = c(1,3), verbose = FALSE)

# Output a model in CASAL2 format
source("OutputModelInCasalFormat.R")
source("OutputObservationsInCasalFormat.R")

# run CASAL2
system("casal2_adolc -r > /tmp/output.txt")

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

# Estimates with survival analysis
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

# ######################################################################################################
# ## Calculate the uncertainties on recruitment

# # CREATED  13 Dec 2016
# # MODIFIED 20 Apr 2017

# # PURPOSE estimate abundance and recruitment point estimates and uncertainties

# # METHOD according to S. Brandt, Data Analysis, 1999 Springer-Verlag
# #        The x% confidence region of the log-likelihood is contains with the minimum log-likelihood + chi-square (0.95, 4 df)
# #        Calculating the recruitment from as many points as possible in this region, we use the extreme values to give the 95% confidence
# #        interval of the recruitment estimate

# x <- 95 # 95% CI

# # Use the best fitted model
# #source("FitModels.R")

# # define some variables

# n.year <- nrow(sim$DuringExploitationPop$catch)
# n.cohort <- nrow(sim$DuringExploitationPop$catch) + ncol(sim$DuringExploitationPop$catch) - 1
# n.par <- length(res$par) # Number of parameters in the best model

# # Create a data.frame to hold the resample results
# n.resample <- 1e2

# resample.results <- as.data.frame(matrix(nrow = n.resample, ncol = n.par + 1 + n.cohort + n.year))
# dimnames(resample.results)[[2]] <- c(paste("par", 1:n.par, sep=""), "log.lik", paste("rec", 1:n.cohort, sep = ""),
#                                      paste("Biomass", 1:n.year, sep = ""))

# # Fix number of define the range of values, in unit of sd, to look around the mean of each parameters
# n.sigma <- 2

# for(resample in 1:n.resample){
# print(paste(resample,"/",n.resample,sep=""))

# # Create a set of random parameters
# rand.par <- rep(NA,n.par)
# for(i in 1:n.par) rand.par[i] <- runif(1, min = res$par[i] -n.sigma * errors[i], max = res$par[i] + n.sigma * errors[i])

# resample.results[resample, 1:n.par] <- rand.par
# resample.results[resample, "log.lik"] <- local.llfunc3(rand.par, catch = sim$DuringExploitationPop$catch/10, effort = outer(-log(1-sim$DuringExploitationPop$yearly.exploitation.rate), rep(1,max.age)), selectivity.at.age = rep(1, max.age), catchability.scaling.factor = 1)

# #ll.model2(rand.par, catch = nb.at.age.wgt, effort = effort, catchability.scaling.factor = csf)

# ### Estimate recruitment
# #P <- prob.for.ll.model2(rand.par, effort = effort, catchability.scaling.factor = csf)
# #prob <- P 
# prob <- prob.llfunc3(rand.par, effort = outer(-log(1-sim$DuringExploitationPop$yearly.exploitation.rate), rep(1,max.age)), selectivity.at.age = rep(1, max.age), catchability.scaling.factor = 1)

# #print(Coaa2Caaa(prob))

# #est.nb.at.age.in.catch <- outer(estimated.nb.in.catch, rep(1, 6)) * nb.at.age / outer(rowSums(nb.at.age), rep(1, 6))
# #est.rec <- rowSums(Caaa2Coaa(est.nb.at.age.in.catch), na.rm = T) / rowSums(P, na.rm = T)
# est.rec <- rowSums(Caaa2Coaa(sim$DuringExploitationPop$catch), na.rm = T) / rowSums(prob, na.rm = T)

# resample.results[resample, grep("rec", names(resample.results))] <- est.rec

# }

# # Select the subset of simulated data that fall within the chi-square (x/100, 4df) distance from the minimum log-likelihood
# #resample.results.x <- subset(resample.results, (log.lik - result2$value) <= (0.5 * qchisq(x/100, df = 11 * 6 - 4 )))
# resample.results.x <- subset(resample.results, (log.lik - res$value) <= (0.5 * qchisq(x/100, df = 2 )))

# # Save the results
# #write.csv(resample.results.x, file = paste("Results/Data/ProfileLikelihoodOfRecruitmentEstimates-", format(Sys.time(), "%b%d%Y-%H-%M-%S"), ".csv", sep=""))

