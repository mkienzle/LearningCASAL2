tmp <- read.csv("Results/SimVsEst.csv", header = FALSE, col.names = c("Simulated", "Survival.Analysis", "CASAL2"))

pdf(file = "Results/Graphics/SimVsEst.pdf")
with(tmp, plot(Simulated, CASAL2, pch = 3, xlab = "Simulated", cex = 2, ylab = "Estimated", las = 1))
#with(tmp, points(Simulated, Survival.Analysis, pch = 19, cex = 0.5, col = "lightgrey"))
#legend(0.65 * max(tmp$Simulated), 0.2 * max(tmp$Simulated), pch = c(3,19), legend = c("CASAL2", "Survival analysis"),
#col = c("black", "lightgrey"))
abline(0,1, col = "blue")
dev.off()
