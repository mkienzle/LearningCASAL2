# CREATED  14 Sep 2018
# MODIFIED 14 Sep 2018

# PURPOSE using the same data passed to CASAL2, estimate recruitment

# Useful function to compute the population dynamic
# source("SimulationFunctions.R")

#
## Simulate a pulse removal fishery in numbers
# source("SimulationStudy.R")

# format the synthetic dataset to use in the "assessment" (same as those passed to CASAL2)

catch <- round(rowSums(sim$DuringExploitationPop$catch),5)
prop.at.age <- round(sim$DuringExploitationPop$catch / outer(rowSums(sim$DuringExploitationPop$catch), rep(1, ncol(sim$DuringExploitationPop$catch))),6)

init.pop <- round(c(0,sim$preExploitationPop$nb.after.SecondHalfNatMort[nb.of.years.of.fishing,-max.age]),3)

biomass.survey <- round(runif(nrow(sim$DuringExploitationPop$nb.after.SecondHalfNatMort), min = 1, max = 1) * rowSums(sim$DuringExploitationPop$nb.after.SecondHalfNatMort),3)


# The likelihood function of a linear regression between abundance survey and abundance from the model
ll.fct <- function(par){

# Compute the dynamics of pop
tmp <- estimated.nb.in.pop(Rec = par, catch = catch, prop.catch.at.age = prop.at.age, Nat.Mort = sim$DuringExploitationPop$M, InitialAbundance = init.pop)

model.biomass <- rowSums(tmp$pop.after2ndNatMort)

plot(model.biomass, biomass.survey)

ll <- -sum(dnorm(model.biomass, mean = biomass.survey, sd = 1, log = TRUE))
return(ll)
}

####
Roptimize.res <- optimize(ll.fct, lower = 1e5, upper = 1e7)
#print(Roptimize.res$minimum)
