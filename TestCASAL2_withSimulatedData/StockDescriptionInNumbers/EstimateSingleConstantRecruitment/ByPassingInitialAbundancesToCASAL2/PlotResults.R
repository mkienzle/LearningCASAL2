tmp <- read.csv("Results/SimVsEst.csv", header = FALSE, col.names = c("Simulated", "Survival.Analysis", "Roptimize", "CASAL2"))

postscript(file = "Results/Graphics/SimVsEst.ps")
# parameters for a beamer presentation
par(mgp = c(4,1,0), cex.lab = 1.5, cex = 1.2, mai = c(1.4, 1.5, 0.82, 0.42))

with(tmp, plot(Simulated, CASAL2, pch = 3, xlab = "Simulated", cex = 2, ylab = "Estimated", las = 1))
#with(tmp, points(Simulated, Survival.Analysis, pch = 19, cex = 0.5, col = "lightgrey"))
with(tmp, points(Simulated, Roptimize, pch = 5))
legend(0.65 * max(tmp$Simulated), 0.3 * max(tmp$Simulated), pch = c(3,5), legend = c("CASAL2", "Roptimize"))
abline(0,1, col = "blue")
dev.off()
