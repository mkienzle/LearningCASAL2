# CREATED  10 Sep 2018
# MODIFIED 11 Sep 2018

# PURPOSE give a visual representation of the dynamic of number

### Load the data
# pre-exploitatioin
y0 <- rowSums(sim$preExploitationPop$nb.at.beginningOfYearInterval)
y1 <- rowSums(sim$preExploitationPop$nb.after.FirstHalfNatMort)
y2 <- rowSums(sim$preExploitationPop$nb.after.catch)
y3 <- rowSums(sim$preExploitationPop$nb.after.SecondHalfNatMort)

x <- seq(1940, length = length(y0))

# During exploitation
Y0 <- rowSums(sim$DuringExploitationPop$nb.at.beginningOfYearInterval)
Y1 <- rowSums(sim$DuringExploitationPop$nb.after.FirstHalfNatMort)
Y2 <- rowSums(sim$DuringExploitationPop$nb.after.catch)
Y3 <- rowSums(sim$DuringExploitationPop$nb.after.SecondHalfNatMort)

X <- seq(x[length(x)] + 1, length = length(Y0))

### Plot the data
postscript(file = "Results/Graphics/PopDynamics.ps")
pchar <- 19

# parameters for a beamer presentation
par(mgp = c(4,1,0), cex.lab = 1.5, cex = 1.2, mai = c(1.02, 1.5, 0.82, 0.42))
plot(x, y0, xlab = "", ylab = "Abundance (nb)", las = 1,
xlim = c(1985, 2010), ylim = c(min(Y3), max(y0)), pch = pchar)

points(x + 0.5, y1, pch = pchar)
points(x + 0.5, y2, pch = pchar)
points(x + 1, y3, pch = pchar)
for(i in 1:length(x)){
      #print(x[i])
      curve(y0[i] * exp(-sim$preExploitationPop$M * (t-x[i])), from = x[i], to = x[i]+1, xname = "t", add = TRUE, col = "blue", lwd = 2.0)
      }
segments(x[-1], y3[-length(y3)], x[-1], y0[-1])


points(X, Y0, pch = pchar)
points(X+0.5, Y1, pch = pchar)
points(X+0.5, Y2, pch = pchar)
points(X+1, Y3, pch = pchar)

for(i in 1:length(X)){
      curve(Y0[i] * exp(-sim$DuringExploitationPop$M * (t-X[i])), from = X[i], to = X[i]+0.5, xname = "t", add = TRUE, col = "blue", lwd = 2.0)
      curve(Y2[i] * exp(-sim$DuringExploitationPop$M * (t-X[i]-0.5)), from = X[i] + 0.5, to = X[i]+1, xname = "t", add = TRUE, col = "blue", lwd = 2.0)
      }

segments(X+0.5, Y1, X+0.5, Y2, col = "red", lwd = 2.0)
segments(X[-1], Y3[-length(Y3)], X[-1], Y0[-1])

dev.off()