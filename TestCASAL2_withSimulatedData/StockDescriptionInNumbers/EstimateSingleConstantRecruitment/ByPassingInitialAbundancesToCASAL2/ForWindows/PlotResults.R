# MODIFIED 12 Oct 2018

# PURPOSE compare estimated recruitment with simulated

# Load the data to be plotted
tmp <- read.csv("Results/SimVsEst.csv", header = FALSE, col.names = c("Simulated", "Survival.Analysis", "Roptimize", "CASAL2"))

# Function to plot CASAL2 results
fct1 <- function(){
with(tmp, plot(Simulated, CASAL2, pch = 3, xlab = "Simulated", cex = 2, ylab = "Estimated", las = 1))
#with(tmp, points(Simulated, Survival.Analysis, pch = 19, cex = 0.5, col = "lightgrey"))
#with(tmp, points(Simulated, Roptimize, pch = 5))
legend(0.65 * max(tmp$Simulated), 0.3 * max(tmp$Simulated), pch = 3, legend = "CASAL2")
abline(0,1, col = "blue")
}

## Plot into PDF
pdf(file = "Results/Graphics/SimVsEst.pdf")
# parameters for a beamer presentation
par(mgp = c(4,1,0), cex.lab = 1.5, cex = 1.2, mai = c(1.4, 1.5, 0.82, 0.42))
fct1()
dev.off()

## Plot in png
#png(file = "Results/Graphics/SimVsEst.png")
# parameters for a beamer presentation
#par(mgp = c(4,1,0), cex.lab = 1.5, cex = 1.2, mai = c(1.4, 1.5, 0.82, 0.42))
#fct1()
#dev.off()

# Function to plot results obtained with fitting the model with R
fct2 <- function(){
with(tmp, plot(Simulated, CASAL2, pch = 3, xlab = "Simulated", cex = 2, ylab = "Estimated", las = 1))
#with(tmp, points(Simulated, Survival.Analysis, pch = 19, cex = 0.5, col = "lightgrey"))
with(tmp, points(Simulated, Roptimize, pch = 5, col = "red"))
legend(0.65 * max(tmp$Simulated), 0.3 * max(tmp$Simulated), pch = c(3,5), legend = c("CASAL2", "Roptimize"), col = c("black","red"))
abline(0,1, col = "blue")
}

## Plot into PDF
pdf(file = "Results/Graphics/SimVsEst2.pdf")
# parameters for a beamer presentation
par(mgp = c(4,1,0), cex.lab = 1.5, cex = 1.2, mai = c(1.4, 1.5, 0.82, 0.42))
fct2()
dev.off()

# ## Plot into png
# png(file = "Results/Graphics/SimVsEst2.png")
# # parameters for a beamer presentation
# par(mgp = c(4,1,0), cex.lab = 1.5, cex = 1.2, mai = c(1.4, 1.5, 0.82, 0.42))
# fct2()
# dev.off()
