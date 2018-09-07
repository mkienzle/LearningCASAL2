# CREATED   1 August 2018
# MODIFIED  7 September 2018

# PURPOSE function for the simulation study

# Useful library
library(SAFR)

# This function simulates the dynamics of a fish population, in number at age, during a pre-exploitation period follow by an fishery exploitation period.

# The age of fishes in the simulation ranges from 0 to max.age (function argument: max.age). Age is grouped into p age categories spanning 1 years (p is equal to max.age)
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

# Recruitment is constant from year to year (from cohort to cohort)
# Selectivity is constant equal to 1 for all age-groups

Simulate.pulsefishing.Fishery <- function(max.age = 10, nb.of.preExploitation.years = 50, nb.of.exploitation.years = 20, exploitation.rate = c(0.2, 0.2), nat.mort.range = c(0.4, 0.4), recruitment.range=c(1e6, 1e6), verbose = FALSE){

## Some parameters
n <- max.age-1

# Number of cohorts
nb.of.cohort <- nb.of.preExploitation.years + nb.of.exploitation.years + max.age - 1

## Exploitation rate: the exploitation rate is a fraction, varying from 0 to 1, determining the proportion of fish alive at time t that are caught by the fishery
# The exploitation rate is 0 for the nb of pre-exploitation years
# Since we perform the dynamics of fish number along cohorts, we simulate simulate an exploitation rate for as many years as there were cohorts (n1 + n2 + p - 1)

exploitation.rate.by.years.since.the.first.cohort.was.born <- c(rep(0, nb.of.preExploitation.years + max.age - 1), runif(nb.of.exploitation.years, min = exploitation.rate[1], max = exploitation.rate[2]))

ExplRate.faced.by.each.cohort <- matrix(nrow = nb.of.cohort, ncol = max.age)
for(i in 1:nb.of.cohort) ExplRate.faced.by.each.cohort[i,] <- exploitation.rate.by.years.since.the.first.cohort.was.born[seq(i, i + max.age - 1)]

# Selectivity is assumed to be constant, equal to 1 for all age-groups

# Natural mortality
M <- runif(1, min = nat.mort.range[1], max = nat.mort.range[2])
if(verbose) print(paste("Simulated natural mortality is", round(M,3)))

# Recruitment: N(0)
Recruitment <- runif(nb.of.cohort, min = recruitment.range[1], max = recruitment.range[2])

#
# Created matrices that will hold number of fish alive at various intervals within each years
#
cohort.nb.at.beginningOfYearInterval <- matrix(nrow = nb.of.cohort, ncol = max.age)
cohort.nb.after.FirstHalfNatMort <- matrix(nrow = nb.of.cohort, ncol = max.age)
cohort.nb.after.catch <- matrix(nrow = nb.of.cohort, ncol = max.age)
cohort.nb.after.SecondHalfNatMort <- matrix(nrow = nb.of.cohort, ncol = max.age)
cohort.catch <- matrix(nrow = nb.of.cohort, ncol = max.age)

# Allocate recruitment at time t=0
cohort.nb.at.beginningOfYearInterval[,1] <- Recruitment

# Perform the dynamics for each timesteps, for all years
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

#
# On returning the values convert from data organised by cohorts to data organised by year
#
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



