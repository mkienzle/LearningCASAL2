# CREATED   1 August 2018
# MODIFIED  3 September 2018

# PURPOSE function for the simulation study

# Useful library
library(SAFR)

#
# This function simulate numbers at age in population over multiple year
# Every year, the number of individuals decrease because
#             1. 1/2 natural mortality (continuous process)
#             2. fishing (a pulse removal of catches)
#             3. 1/2 natural mortality (continuous process)

Simulate.pulsefishing.Fishery <- function(max.age = 10, nb.of.cohort = 20, exploitation.rate = c(0.2, 0.2), nat.mort.range = c(0.1, 0.8), recruitment.range=c(1e6, 1e7), log.para.range = c(8,12), log.parb.range = c(1,3), verbose = FALSE){

# Some parameters
#max.age <- 10; nb.of.cohort <- 20 # be aware that nb.of.cohort / max.age >= 3
                               # for some algorithm below to work
#age <- matrix(seq(0, max.age), nrow = nb.of.cohort, ncol = max.age+1, byrow=T)

# Exploitation rate
#yearly.exploitation.rate <- runif(nb.of.cohort, min = exploitation.rate[1], max = exploitation.rate[2])
#ExplRate.faced.by.each.cohort <- matrix(nrow = nb.of.cohort, ncol = max.age)
#for(i in 1:nb.of.cohort) ExplRate.faced.by.each.cohort[i,] <- yearly.exploitation.rate[seq(i, i + max.age - 1)]

### Exploitation rate
# here we simulate as many exploitation rates as number of cohorts, so that we can simulate
# the dynamics of all cohorts
simulated.exploitation.rate <- runif(nb.of.cohort, min = exploitation.rate[1], max = exploitation.rate[2])
ExplRate.faced.by.each.cohort <- matrix(nrow = nb.of.cohort, ncol = max.age)
for(i in 1:nb.of.cohort) ExplRate.faced.by.each.cohort[i,] <- simulated.exploitation.rate[seq(i, i + max.age - 1)]

# check that exploitation rates are the same for each rows
if(all(duplicated(Coaa2Caaa(ExplRate.faced.by.each.cohort)[,1:2], MARGIN = 2)[-1])){
		yearly.exploitation.rate <- Coaa2Caaa(ExplRate.faced.by.each.cohort)[,1]}
		else {
		print("There a problem with yearly.exploitation.rate <- ")
		stop();}

n <- max.age-1
#selectivity.at.age <- c(0,0,seq(1/(max.age - 3), (max.age - 4)/(max.age - 3), length = max.age - 4), 1,1)
#selectivity.at.age <- c(0,0,seq(1/(max.age - 5), (max.age - 6)/(max.age - 5), length = max.age - 8), rep(1,6))
log.para <- runif(1, min = log.para.range[1], max=log.para.range[2])
if(verbose) print(paste("Logistic parameter a is", log.para))
log.parb <- runif(1, min = log.parb.range[1], max=log.parb.range[2])
if(verbose) print(paste("Logistic parameter b is", log.parb))

selectivity.at.age <- logistic(log.para,log.parb,seq(1,max.age))

# Natural mortality
M <- runif(1, min = nat.mort.range[1], max = nat.mort.range[2])
if(verbose) print(paste("Simulated natural mortality is", round(M,3)))

# Recruitment: N(0)
Recruitment <- runif(nb.of.cohort, min = recruitment.range[1], max = recruitment.range[2])

# Nb at age in cohorts
cohort.nb.at.beginningOfYearInterval <- matrix(nrow = nb.of.cohort, ncol = max.age)
cohort.nb.after.FirstHalfNatMort <- matrix(nrow = nb.of.cohort, ncol = max.age)
cohort.nb.after.catch <- matrix(nrow = nb.of.cohort, ncol = max.age)
cohort.nb.after.SecondHalfNatMort <- matrix(nrow = nb.of.cohort, ncol = max.age)
cohort.catch <- matrix(nrow = nb.of.cohort, ncol = max.age)

cohort.nb.at.beginningOfYearInterval[,1] <- Recruitment

for(i in 1:nrow(cohort.nb.at.beginningOfYearInterval)){
      for(j in 1:ncol(cohort.nb.at.beginningOfYearInterval)){
      
      # Apply first 1/2 natural mortality
      cohort.nb.after.FirstHalfNatMort[i,j] <- cohort.nb.at.beginningOfYearInterval[i,j] * exp(-M/2)

      ## Remove catch

      # Catch is proportional to exploitation rate
      cohort.catch[i,j] <- ExplRate.faced.by.each.cohort[i,j] * cohort.nb.after.FirstHalfNatMort[i,j]
      cohort.nb.after.catch[i,j] <- cohort.nb.after.FirstHalfNatMort[i,j] - cohort.catch[i,j]

      # Apply second 1/2 natural mortality
      cohort.nb.after.SecondHalfNatMort[i,j] <- cohort.nb.after.catch[i,j] * exp(-M/2)

      # Ageing
      if(j < ncol(cohort.nb.at.beginningOfYearInterval))
      	   cohort.nb.at.beginningOfYearInterval[i,j+1] <- cohort.nb.after.SecondHalfNatMort[i,j]
}
}

return(list(nb.at.beginningOfYearInterval = Coaa2Caaa(cohort.nb.at.beginningOfYearInterval),
            nb.after.FirstHalfNatMort = Coaa2Caaa(cohort.nb.after.FirstHalfNatMort),
	    nb.after.catch = Coaa2Caaa(cohort.nb.after.catch),
            nb.after.SecondHalfNatMort = Coaa2Caaa(cohort.nb.after.SecondHalfNatMort),
	    catch = Coaa2Caaa(cohort.catch),
	    M = M,
	    yearly.exploitation.rate = yearly.exploitation.rate))
	    
}

### Need to introduce the idea of pre-exploited fishery

# This function simulated the dynamics of a fishery targeting fish with age ranging between 0 and max.age (function argument: max.age), grouped into p age categories spanning 1 years (p is equal to max.age)
# We'll simulate a fish population that was not exploited for n1 years (function argument: nb.of.preExploitation.years)
# n1 + p - 1 cohorts existed during those years
# The exploitation rate during those years was 0

# Exploitation started 1 year after n1 and lasted for n2 years (function argument: nb.of.exploitation.years)
#
#
# This function simulate numbers at age in population over multiple year
# Every year, the number of individuals decrease because
#             1. 1/2 natural mortality (continuous process)
#             2. fishing (a pulse removal of catches)
#             3. 1/2 natural mortality (continuous process)

Simulate.pulsefishing.Fishery2 <- function(max.age = 10, nb.of.preExploitation.years = 50, nb.of.exploitation.years = 20, exploitation.rate = c(0.2, 0.2), nat.mort.range = c(0.4, 0.4), recruitment.range=c(1e6, 1e6), log.para.range = c(8,12), log.parb.range = c(1,3), verbose = FALSE){


## Some parameters

# Number of cohorts
nb.of.cohort <- nb.of.preExploitation.years + nb.of.exploitation.years + max.age - 1

#max.age <- 10; nb.of.cohort <- 20 # be aware that nb.of.cohort / max.age >= 3
                               # for some algorithm below to work
#age <- matrix(seq(0, max.age), nrow = nb.of.cohort, ncol = max.age+1, byrow=T)

## Exploitation rate. The exploitation rate is 0 for the nb of pre-exploitation years
# For ease of simulation, we perform the dynamic all the cohorts in the dataset since they were age=0
exploitation.rate.by.years.since.the.first.cohort.was.born <- c(rep(0, nb.of.preExploitation.years + max.age - 1), runif(nb.of.exploitation.years, min = exploitation.rate[1], max = exploitation.rate[2]))

ExplRate.faced.by.each.cohort <- matrix(nrow = nb.of.cohort, ncol = max.age)
for(i in 1:nb.of.cohort) ExplRate.faced.by.each.cohort[i,] <- exploitation.rate.by.years.since.the.first.cohort.was.born[seq(i, i + max.age - 1)]

#yearly.exploitation.rate <- runif(nb.of.cohort, min = exploitation.rate[1], max = exploitation.rate[2])
#ExplRate.faced.by.each.cohort <- matrix(nrow = nb.of.cohort, ncol = max.age)
#for(i in 1:nb.of.cohort) ExplRate.faced.by.each.cohort[i,] <- yearly.exploitation.rate[seq(i, i + max.age - 1)]

### Exploitation rate
# here we simulate as many exploitation rates as number of cohorts, so that we can simulate
# the dynamics of all cohorts
#simulated.exploitation.rate <- runif(nb.of.cohort, min = exploitation.rate[1], max = exploitation.rate[2])
#ExplRate.faced.by.each.cohort <- matrix(nrow = nb.of.cohort, ncol = max.age)
#for(i in 1:nb.of.cohort) ExplRate.faced.by.each.cohort[i,] <- simulated.exploitation.rate[seq(i, i + max.age - 1)]

# check that exploitation rates are the same for each rows
#if(all(duplicated(Coaa2Caaa(ExplRate.faced.by.each.cohort)[,1:2], MARGIN = 2)[-1])){
#		yearly.exploitation.rate <- Coaa2Caaa(ExplRate.faced.by.each.cohort)[,1]}
#		else {
#		print("There a problem with yearly.exploitation.rate <- ")
#		stop();}

n <- max.age-1
#selectivity.at.age <- c(0,0,seq(1/(max.age - 3), (max.age - 4)/(max.age - 3), length = max.age - 4), 1,1)
#selectivity.at.age <- c(0,0,seq(1/(max.age - 5), (max.age - 6)/(max.age - 5), length = max.age - 8), rep(1,6))
log.para <- runif(1, min = log.para.range[1], max=log.para.range[2])
if(verbose) print(paste("Logistic parameter a is", log.para))
log.parb <- runif(1, min = log.parb.range[1], max=log.parb.range[2])
if(verbose) print(paste("Logistic parameter b is", log.parb))

selectivity.at.age <- logistic(log.para,log.parb,seq(1,max.age))

# Natural mortality
M <- runif(1, min = nat.mort.range[1], max = nat.mort.range[2])
if(verbose) print(paste("Simulated natural mortality is", round(M,3)))

# Recruitment: N(0)
Recruitment <- runif(nb.of.cohort, min = recruitment.range[1], max = recruitment.range[2])

# Nb at age in cohorts
cohort.nb.at.beginningOfYearInterval <- matrix(nrow = nb.of.cohort, ncol = max.age)
cohort.nb.after.FirstHalfNatMort <- matrix(nrow = nb.of.cohort, ncol = max.age)
cohort.nb.after.catch <- matrix(nrow = nb.of.cohort, ncol = max.age)
cohort.nb.after.SecondHalfNatMort <- matrix(nrow = nb.of.cohort, ncol = max.age)
cohort.catch <- matrix(nrow = nb.of.cohort, ncol = max.age)

cohort.nb.at.beginningOfYearInterval[,1] <- Recruitment

for(i in 1:nrow(cohort.nb.at.beginningOfYearInterval)){
      for(j in 1:ncol(cohort.nb.at.beginningOfYearInterval)){
      
      # Apply first 1/2 natural mortality
      cohort.nb.after.FirstHalfNatMort[i,j] <- cohort.nb.at.beginningOfYearInterval[i,j] * exp(-M/2)

      ## Remove catch

      # Catch is proportional to exploitation rate
      cohort.catch[i,j] <- ExplRate.faced.by.each.cohort[i,j] * cohort.nb.after.FirstHalfNatMort[i,j]
      cohort.nb.after.catch[i,j] <- cohort.nb.after.FirstHalfNatMort[i,j] - cohort.catch[i,j]

      # Apply second 1/2 natural mortality
      cohort.nb.after.SecondHalfNatMort[i,j] <- cohort.nb.after.catch[i,j] * exp(-M/2)

      # Ageing
      if(j < ncol(cohort.nb.at.beginningOfYearInterval))
      	   cohort.nb.at.beginningOfYearInterval[i,j+1] <- cohort.nb.after.SecondHalfNatMort[i,j]
}
}

return(list(preExploitationPop = list(
			       nb.at.beginningOfYearInterval = Coaa2Caaa(cohort.nb.at.beginningOfYearInterval)[seq(1, nb.of.preExploitation.years),],
            		       nb.after.FirstHalfNatMort = Coaa2Caaa(cohort.nb.after.FirstHalfNatMort)[seq(1, nb.of.preExploitation.years),],
	    		       nb.after.catch = Coaa2Caaa(cohort.nb.after.catch)[seq(1, nb.of.preExploitation.years),],
            		       nb.after.SecondHalfNatMort = Coaa2Caaa(cohort.nb.after.SecondHalfNatMort)[seq(1, nb.of.preExploitation.years),],
	    		       catch = Coaa2Caaa(cohort.catch)[seq(1, nb.of.preExploitation.years),],
	    		       M = M),
	  DuringExploitationPop = list(
			       nb.at.beginningOfYearInterval = Coaa2Caaa(cohort.nb.at.beginningOfYearInterval)[-seq(1,nb.of.preExploitation.years),],
            		       nb.after.FirstHalfNatMort = Coaa2Caaa(cohort.nb.after.FirstHalfNatMort)[-seq(1,nb.of.preExploitation.years),],
	    		       nb.after.catch = Coaa2Caaa(cohort.nb.after.catch)[-seq(1,nb.of.preExploitation.years),],
            		       nb.after.SecondHalfNatMort = Coaa2Caaa(cohort.nb.after.SecondHalfNatMort)[-seq(1,nb.of.preExploitation.years),],
	    		       catch = Coaa2Caaa(cohort.catch)[-seq(1,nb.of.preExploitation.years),],
	    		       M = M,
			       yearly.exploitation.rate = exploitation.rate.by.years.since.the.first.cohort.was.born[-seq(1,nb.of.preExploitation.years + max.age - 1)]
)
))	    
}



