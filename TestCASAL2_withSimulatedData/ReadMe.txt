# 13 August 2018
# 14 August 2018

# AUTHOR Marco.Kienzle@gmail.com

# Background

I order to learn how casal works, synthetic data were first simulated using the Baranov equation in the same way that the survival analysis model and the delay difference model were tested with simulated data. Such approach did not work since CASAL2, did not implemented the Baranov equation (as of July 2018), implemented a pulse fishery where catches are assumed to be removed from the population instantaneously (see pulse removals in Quinn and Deriso, 1999). To be more specific, this discrepancy between the population dynamic model used in the simulation and the estimation (CASAL2) models resulted in the inability for CASAL2 to estimate the exact value of the parameters used in the simulation.

Therefore, following the advice of Sophie Mormede, I modified the population dynamic model to assume that the number of individual at age decrease during the years (1) first, using the exponential mortality model with constant rate equal to M/2; (2) then by the number caught at age, calculated as a proportion of the stock - an proportion exploited in CASAL2 language - applied equally to all number at age in the simulation; and (3) a second natural mortality of magnitude M/2, exactely the same as the first natural mortality event. This approach (in sub-directory EstimateSingleConstantRecruitment/ByPassingInitialAbundancesToCASAL2) allowed to estimate a single constant recruitment.

Then, I removed passing initial abundances to CASAL2 and used the iterative procedure to calculate these initial abundances: CASAL2 failed to estimate recruitment correctly, it underestimated it, under-estimation that was proportional to the magnitude of simulated recruitment. Ian Doonan, explained that the iterative procedure applied by CASAL2 to calculate the initial abundances in the population does so by taking the stable distribution of number at age in the population before the start assuming the model is wild, un-exploited, virgin (no fishing exploitation is taking place before the record of landing started). Such approach might apply to new zealand fisheries that were not exploited before modern time recording of fisheries catches but would certainly not work for either Europe or Australian fishery where the beginning of the time series of catch start (much) after people started to fish the stocks. In other words, if you are to use the iterative methods, it will have to be on a CASAL2 model of a stock which state before the time series of catches starts is virgin (un-exploited; only natural mortality did reduce the number of fish through time).
Modifying the simulation (SimulationFunctions.) to create a dataset where the stock is virgin before the first year of data passed to CASAL2 did allow to estimate a constant recruitment (EstimateSingleConstantRecruitment/ByCalculatingInitialAbundanceUsingTheIterativeProcedureInCASAL2).
Using these simulated data, we can estimate natural and fishing mortality using survival analysis: optim(c(1,0.2), llfunc3, catch = sim$catch / 10, effort = outer(-log(1-sim$yearly.exploitation.rate), rep(1,20)), selectivity.at.age = rep(1, 20), catchability.scaling.factor = 1). Note that CASAL2 exploitation rate (U) is transformed here in fishing effort using F = -log(1-U), because in such a pulse removal fishery removing a proportion U of the population (number at age) is the same as saying you took 1 - exp(-F)



# References
Quinn and Deriso, 1999 - Quantitative Fish Dynamics
