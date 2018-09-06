# CREATED  23 May 2018
# MODIFIED  3 Sep 2018

# PURPOSE create the input for CASAL2 that describes the population structure

filename = "./myfirstmodel.csl2"

cat(file = filename, "## CREATED  23 Jul 2018\n")
cat(file = filename, "## MODIFIED  3 Sep 2018\n", append = TRUE)

cat(file = filename, "\n", append = TRUE)

cat(file = filename, "## an age-structured model\n", append = TRUE)

cat(file = filename, "@model\n", append = TRUE)
cat(file = filename, "type age\n", append = TRUE)
cat(file = filename, "min_age 1\n", append = TRUE)
cat(file = filename, paste("max_age", max.age,"\n"), append = TRUE)
cat(file = filename, "age_plus false\n", append = TRUE)
cat(file = filename, "start_year 1970\n", append = TRUE)
cat(file = filename, paste("final_year", 1970 + nb.of.years.of.fishing - 1,"\n"), append = TRUE)
cat(file = filename, "base_weight_units kgs\n", append = TRUE)
cat(file = filename, "time_steps January-April May-August September-December\n", append = TRUE)
#cat(file = filename, "initialisation_phases Equilibrium_state\n", append = TRUE)
cat(file = filename, "initialisation_phases Fixed\n", append = TRUE)


cat(file = filename, "\n", append = TRUE)

cat(file = filename, "@initialisation_phase Fixed\n", append = TRUE)
cat(file = filename, "type state_category_by_age\n", append = TRUE)
cat(file = filename, "categories some_species\n", append = TRUE)
cat(file = filename, "min_age 1\n", append = TRUE)
cat(file = filename, paste("max_age", max.age,"\n"), append = TRUE)
cat(file = filename, "table n\n", append = TRUE)
cat(file = filename, "some_species ", append = TRUE)
cat(file = filename, round(c(0,sim$preExploitationPop$nb.after.SecondHalfNatMort[nb.of.years.of.fishing,-max.age]),3), append = TRUE)
cat(file = filename, "\n", append = TRUE)

cat(file = filename, "\n", append = TRUE)

cat(file = filename, "@categories\n", append = TRUE)
cat(file = filename, "format Stock\n", append = TRUE)
cat(file = filename, "names some_species\n", append = TRUE)
#cat(file = filename, "age_lengths age_size\n", append = TRUE)

cat(file = filename, "\n", append = TRUE)

#cat(file = filename, "@initialisation_phase Equilibrium_state\n", append = TRUE)
#cat(file = filename, "type derived\n", append = TRUE)

cat(file = filename, "\n", append = TRUE)

cat(file = filename, "# Sequence of processes affecting the stock\n", append = TRUE)
cat(file = filename, "@time_step January-April\n", append = TRUE)
cat(file = filename, "processes Recruitment NaturalMortality\n", append = TRUE)
cat(file = filename, "@time_step May-August\n", append = TRUE)
cat(file = filename, "processes Fishery\n", append = TRUE)
cat(file = filename, "@time_step September-December\n", append = TRUE)
cat(file = filename, "processes NaturalMortality Ageing\n", append = TRUE)

cat(file = filename, "\n", append = TRUE)

cat(file = filename, "## Recruitment\n", append = TRUE)
cat(file = filename, "@process Recruitment\n", append = TRUE)
cat(file = filename, "type recruitment_constant\n", append = TRUE)
cat(file = filename, "categories some_species\n", append = TRUE)
cat(file = filename, "proportions 1\n", append = TRUE)
cat(file = filename, paste("r0", sim.rec, "## R0 is numbers, you could also specify B0\n"), append = TRUE)
cat(file = filename, "age 1\n", append = TRUE)

cat(file = filename, "\n", append = TRUE)

cat(file = filename, "@process Ageing\n", append = TRUE)
cat(file = filename, "type ageing\n", append = TRUE)
cat(file = filename, "categories some_species\n", append = TRUE)

cat(file = filename, "\n", append = TRUE)

### here we will output yields
vbgf <- function(t, Linf, k, t0) Linf * (1 - exp(-k*(t-t0)))
par <- c(100, 0.5, 0)
length.at.age <- vbgf(seq(1,max.age), Linf = par[1], k = par[2], t0 = par[3])

# Length-Weight relationship
#par <- c(par, 1e-6, 3)
#weight.at.age <- par[4] * length.at.age ^ par[5] # in kilograms
#yield <- rowSums(sim$catch * outer(rep(1, dim(sim$catch)[1]),weight.at.age))

cat(file = filename, "@process NaturalMortality\n", append = TRUE)
cat(file = filename, "type mortality_constant_rate\n", append = TRUE)
cat(file = filename, "categories some_species\n", append = TRUE)
cat(file = filename, "selectivities One\n", append = TRUE)
cat(file = filename, paste("m ", sim$DuringExploitationPop$M/2, "\n", sep=""), append = TRUE) # M/2 because applied in 2 time-steps

cat(file = filename, "\n", append = TRUE)

cat(file = filename, "@process Fishery\n", append = TRUE)
cat(file = filename, "type mortality_event\n", append = TRUE)
cat(file = filename, "categories some_species\n", append = TRUE)
cat(file = filename, paste("years   1970:", 1970 + nb.of.years.of.fishing - 1, "\n", sep=""), append=TRUE)
cat(file = filename, "catches ", append = TRUE)
cat(file = filename, round(rowSums(sim$DuringExploitationPop$catch),5), append = TRUE)
cat(file = filename, "\n", append = TRUE)
cat(file = filename, "U_max 0.9\n", append = TRUE)
cat(file = filename, "selectivities One\n", append = TRUE)

cat(file = filename, "\n", append = TRUE)

cat(file = filename, "@selectivity One\n", append = TRUE)
cat(file = filename, "type constant\n", append = TRUE)
cat(file = filename, "c 1\n", append = TRUE)

cat(file = filename, "\n", append = TRUE)


