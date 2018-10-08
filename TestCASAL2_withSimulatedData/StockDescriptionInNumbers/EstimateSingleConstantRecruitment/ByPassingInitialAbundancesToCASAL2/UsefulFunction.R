# CREATED  15 Aug 2018
# MODIFIED  8 Oct 2018
# Read matrices at age from CASAL2 reports

ReadYearCrossAgeMat <- function(Identifier = '', ReportFileName = '/tmp/tmp.txt'){

# Which line does the Identifier appears at
line.start <- grep(Identifier ,readLines(ReportFileName), fixed = TRUE)

tmp.mat <- matrix(scan(ReportFileName, skip = line.start + 4, nlines = nb.of.years.of.fishing), ncol = max.age + 1, byrow=T)
data.tab <- as.data.frame(tmp.mat[,-1]);
dimnames(data.tab)[[1]] <- tmp.mat[,1]; dimnames(data.tab)[[2]] <- scan(ReportFileName, what = "character", skip = line.start + 3, nlines =1)[-1]

return(data.tab)
}

### For survival analysis

prob.llfunc3 <- function (par, effort, selectivity.at.age, catchability.scaling.factor) {

    effort.by.cohort <- Caaa2Coaa(effort)
    q <- par[1] * catchability.scaling.factor
    M <- par[2]
    F <- q * effort.by.cohort * outer(rep(1, nrow(effort.by.cohort)), 
        selectivity.at.age)
    Z <- M + F
    cum.Z <- my.cumsum(Z)
    prob1 <- F/Z * exp(-(cum.Z - Z))
    prob2 <- F/Z * exp(-cum.Z)
    P <- prob1 - prob2

return(P)
}

local.llfunc3 <- function (par, catch, effort, selectivity.at.age, catchability.scaling.factor) 
{


# Probability model
P <- prob.llfunc3(par, effort, selectivity.at.age, catchability.scaling.factor)

# likelihood function
catch.by.cohort <- Caaa2Coaa(catch)
index <- which(!is.na(catch.by.cohort) & P != 0)
-sum(catch.by.cohort[index] * log(P[index]/total.over.lines(P)[index]))
}




