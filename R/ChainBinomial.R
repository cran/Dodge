ChainBinomial <-
function (N, n, i, p = seq(0, 0.2, 0.001), Plots = TRUE) 
{
    OC = (1 - p)^n + n * p * (1 - p)^(n + n * i - 1)
    AOQ = (N - n) * p * OC/N
    ATI = n * OC + N * (1 - OC)
    results = list(p = p, OC = OC, n = rep(n, length(p)), AOQ = AOQ, 
        ATI = ATI)
    class(results) = "AccSampPlan"
    if (Plots) {
        par(mfrow = c(2, 2))
        plot(results)
    }
    return(results)
}
