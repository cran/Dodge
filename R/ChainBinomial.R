ChainBinomial <-
function (N, n, i, p = seq(0, 0.2, 0.001), Plots = TRUE) 
{
    OC = (1 - p)^n + n * p * (1 - p)^(n + n * i - 1)
    AOQ = (N - n) * p * OC/N
    ATI = n * OC + N * (1 - OC)
    results = cbind(p, OC, AOQ, ATI)
    if (Plots) {
        par(mfrow = c(2, 2))
        plot(OC ~ p, type = "l", ylab = "Probability of Acceptance", 
            xlab = "Fraction Nonconforming p")
        plot(rep(n, length(p)) ~ p, type = "l", ylab = "Uncurtailed sample size", 
            xlab = "Fraction Nonconforming p")
        plot(AOQ ~ p, type = "l", ylab = "AOQ", xlab = "Fraction Nonconforming p")
        title(paste("AOQL = ", formatC(max(AOQ))))
        plot(ATI ~ p, type = "l", ylab = "ATI", xlab = "Fraction Nonconforming p")
        par(mfrow = c(1, 1))
    }
    results
}
