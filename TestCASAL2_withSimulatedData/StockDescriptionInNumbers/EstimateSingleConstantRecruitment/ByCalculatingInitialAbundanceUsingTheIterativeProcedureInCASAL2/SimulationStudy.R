# CREATED  13 November 2014 (approach originally written for survival analysis)
# MODIFIED 24 Sep 2018

# PURPOSE simulate a pulse fishery (i.e. catches taken instantaneously)
#         in order to test CASAL2

# Useful functions
source("SimulationFunctions.R")

# Keep all simulations
all.simulations <- list()
counter <- 1

# Combination of paramaters
#combination.of.par <- expand.grid(max.age = c(8,12,16), nb.of.cohort = seq(20,40,10), n.sample.per.year = rep(seq(1e3,1e4,1e3), each = 1e2))
combination.of.par <- expand.grid(max.age = 20, nb.of.cohort = 50, n.sample.per.year = 2e3)

# How many simulations ?
n.simulations <- nrow(combination.of.par)

# track simulation using
rand.seeds <- runif(n.simulations, min = 1, max = 1e6)

for( i in 1:n.simulations){

#print(paste("Simulation ", i,"of", dim(combination.of.par)[1]))

sim.rec <- round(runif(1, min = 1e5, max = 1e7),0)
cat(paste(sim.rec, ",", sep=""))

set.seed(rand.seeds[i])
sim <-  Simulate.pulsefishing.Fishery(max.age = combination.of.par$max.age[i], nb.of.cohort = combination.of.par$nb.of.cohort[i], exploitation.rate = c(0.05, 0.5), nat.mort.range = c(0.1,0.5), recruitment.range=c(sim.rec, sim.rec), log.para.range = c(-1e3,-1e3), log.parb.range = c(1e3, 1e3))

all.simulations[[counter]] <- sim
counter <- counter + 1

}

# Output a model in CASAL2 format

# **** WARNING **** : REPLACED THIS
#source("OutputModelInCasalFormat.R")
#source("OutputObservationsInCasalFormat.R")
# BY
source(paste(ProjDir, "StockDescriptionInNumbers/EstimateSingleConstantRecruitment", "Rlibrary", "OutputModelInCasal2Format.R", sep = "/"))
source(paste(ProjDir, "StockDescriptionInNumbers/EstimateSingleConstantRecruitment", "Rlibrary", "OutputObservationsInCasal2Format.R", sep = "/"))

# Estimates with survival analysis
# note that the exploitation rate is converted into fishing mortality using the relation U = 1 - exp(-F) because taking a proportion U of the population is the same than saying 1 - exp(-F) died; U = 1 - exp(-F) <=> F = -log(1-U)
res <- optim(c(1,0.2), llfunc3, catch = sim$catch / 10, effort = outer(-log(1-sim$yearly.exploitation.rate), rep(1,combination.of.par$max.age[i])), selectivity.at.age = rep(1, combination.of.par$max.age[i]), catchability.scaling.factor = 1, hessian = TRUE)

#print(res)
x <- seq(0,1, length = 100)
y <- rep(NA, length(x))
for(j in 1:length(x)) y[j] <- llfunc3(c(res$par[1], x[j]), catch = sim$catch / 10, effort = outer(-log(1-sim$yearly.exploitation.rate), rep(1,combination.of.par$max.age[i])), selectivity.at.age = rep(1, combination.of.par$max.age[i]), catchability.scaling.factor = 1)

#pdf(file = "Results/Graphics/SurvivalAnalysis-NatMortLikelihoodProfile.pdf")
#plot(x,y, type = "l")
#abline(v = sim$M, lty = 3)
#dev.off()

