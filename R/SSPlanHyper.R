SSPlanHyper <-
function (N, n, Ac, p = seq(0, 0.3, 0.001), Plots = TRUE) 
{
    OC = phyper(Ac, N * p, N * (1 - p), n)
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
}
