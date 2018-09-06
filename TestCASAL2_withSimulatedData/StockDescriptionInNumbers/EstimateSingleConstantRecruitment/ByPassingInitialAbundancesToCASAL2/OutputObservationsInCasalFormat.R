# CREATED  23 May 2018
# MODIFIED  3 Sep 2018

# PURPOSE create the input for CASAL2 that describes the data collected on the stock

filename = "./observations.csl2"

cat(file = filename, "@catchability SimAbundanceNumberSurvey_q\n")
cat(file = filename, "type free\n", append = TRUE)
cat(file = filename, "q 1\n", append = TRUE)

cat(file = filename, "\n", append = TRUE)

cat(file = filename, "@observation SimulatedAbundanceNumber\n", append = TRUE)
cat(file = filename, "type abundance\n", append = TRUE)
cat(file = filename, "categories some_species\n", append = TRUE)
cat(file = filename, "time_step September-December\n", append = TRUE)
cat(file = filename, "catchability SimAbundanceNumberSurvey_q\n", append = TRUE)
cat(file = filename, "selectivities One\n", append = TRUE)
cat(file = filename, "likelihood normal\n", append = TRUE)
cat(file = filename, paste("years 1970:", 1970 + nb.of.years.of.fishing - 1, "\n", sep = ""), append = TRUE)

cat(file = filename, "obs ", append = TRUE)
cat(file = filename, paste(round(runif(nrow(sim$DuringExploitationPop$nb.after.SecondHalfNatMort), min = 1, max = 1) * rowSums(sim$DuringExploitationPop$nb.after.SecondHalfNatMort),3), collapse = " "), append = TRUE)
cat(file = filename, "\n", append = TRUE)

cat(file = filename, "error_value ", append = TRUE)
cat(file = filename, paste(rep(0, nrow(sim$DuringExploitationPop$catch)), collapse = " "), append = TRUE)
cat(file = filename, "\n", append = TRUE)

cat(file = filename, "time_step_proportion 1.0\n", append = TRUE)
cat(file = filename, "process_error 1e-3\n", append = TRUE)

cat(file = filename, "\n", append = TRUE)

cat(file = filename, "@observation some_species_agefreq\n", append = TRUE)
cat(file = filename, "type proportions_at_age\n", append = TRUE)
cat(file = filename, "categories some_species\n", append = TRUE)
cat(file = filename, "min_age 1\n", append = TRUE)
cat(file = filename, paste("max_age", max.age,"\n"), append = TRUE)
cat(file = filename, "selectivities One\n", append = TRUE)
cat(file = filename, paste("years 1970:", 1970 + nb.of.years.of.fishing - 1, "\n", sep = ""), append = TRUE)
cat(file = filename, "plus_group false\n", append = TRUE)
cat(file = filename, "time_step May-August\n", append = TRUE)
cat(file = filename, "likelihood multinomial\n", append = TRUE)

cat(file = filename, "table obs\n", append = TRUE)
write.table(file = filename, cbind(seq(1970, 1970 + nb.of.years.of.fishing - 1), round(sim$DuringExploitationPop$catch / outer(rowSums(sim$DuringExploitationPop$catch), rep(1, ncol(sim$DuringExploitationPop$catch))),6)), row.names = FALSE, col.names = FALSE, append = TRUE)
cat(file = filename, "end_table\n", append = TRUE)

cat(file = filename, "\n", append = TRUE)

cat(file = filename, "table error_values\n", append = TRUE)
write.table(file = filename, cbind(seq(1970, 1970 + nb.of.years.of.fishing - 1), 0), row.names = FALSE, col.names = FALSE, append = TRUE)
cat(file = filename, "end_table\n", append = TRUE)




