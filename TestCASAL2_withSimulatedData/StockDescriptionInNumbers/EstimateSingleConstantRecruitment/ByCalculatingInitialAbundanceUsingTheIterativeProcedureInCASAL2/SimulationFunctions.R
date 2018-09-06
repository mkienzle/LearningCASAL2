# CREATED   1 August 2018
# MODIFIED 14 August 2018

# PURPOSE function for the simulation study

# Useful library
library(SAFR)

# CHANGES
#
# 14/8/2018: following the switch to using CASAL2 iterative procedure to find initial conditions,
#            I had to modify the present code to simulate a population without fishing harvest before the 1st year of data
#            (i.e. a population in its natural state, virgnin state)
#            This was achieved by applying fisheries removals (catches) only to those cohort's age-groups that are present in the matrix of catch at age 


#
# This function simulate numbers at age in population over multiple year
# Every year, the number of individuals decrease because
#             1. 1/2 natural mortality (continuous process)
#             2. fishing (a pulse removal of catches)
#             3. 1/2 natural mortality (continuous process)

Simulate.pulsefishing.Fishery <- function(max.age = 10, nb.of.cohort = 20, exploitation.rate = c(0.2, 0.2), nat.mort.range = c(0.1, 0.8), recruitment.range=c(1e6, 1e7), log.para.range = c(8,12), log.parb.range = c(1,3), verbose = FALSE){

## Some parameters

# Exploitation rate
n.year <- nb.of.cohort - max.age + 1
yearly.exploitation.rate <- runif(n.year, min = exploitation.rate[1], max = exploitation.rate[2])
#ExplRate.faced.by.each.cohort <- matrix(nrow = nb.of.cohort, ncol = max.age)
#for(i in 1:nb.of.cohort) ExplRate.faced.by.each.cohort[i,] <- yearly.exploitation.rate[seq(i, i + max.age - 1)]
ExplRate.faced.by.each.cohort <- Caaa2Coaa(outer(yearly.exploitation.rate, rep(1,max.age)))

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

# Identify which age-group of each cohort will be present in the catch at age matrix
cohorts.IdentifyCohorts <- outer(seq(1,nb.of.cohort), rep(1,max.age))
PresentInCatchAtAge <- Coaa2Caaa(cohorts.IdentifyCohorts)
PresentInCatchAtAge <- Caaa2Coaa(Coaa2Caaa(cohorts.IdentifyCohorts) > 0)
PresentInCatchAtAge <- replace(PresentInCatchAtAge, is.na(PresentInCatchAtAge), FALSE)

for(i in 1:nrow(cohort.nb.at.beginningOfYearInterval)){
      for(j in 1:ncol(cohort.nb.at.beginningOfYearInterval)){
      
      ## Apply first 1/2 natural mortality
      cohort.nb.after.FirstHalfNatMort[i,j] <- cohort.nb.at.beginningOfYearInterval[i,j] * exp(-M/2)

      ## Remove catch
      # Apply catch only to cohort'age group that will appear in the cath i.e
      # prior to the first year, the stock was not exploited (N.B.: Catch is proportional to exploitation rate)
      if(PresentInCatchAtAge[i,j])
      {
      cohort.catch[i,j] <- ExplRate.faced.by.each.cohort[i,j] * cohort.nb.after.FirstHalfNatMort[i,j]
      }
      # For the cohort's age-groups that are not in the catch, do not apply catches
      else {
      cohort.catch[i,j] <- 0
      }
      cohort.nb.after.catch[i,j] <- cohort.nb.after.FirstHalfNatMort[i,j] - cohort.catch[i,j]
      
      ## Apply second 1/2 natural mortality
      cohort.nb.after.SecondHalfNatMort[i,j] <- cohort.nb.after.catch[i,j] * exp(-M/2)

      ## Ageing
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



