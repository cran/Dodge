VSPUnknown <-
function (N, n, k, p = seq(0, 1, 0.001), Plots = TRUE) 
{
    Pa = p
    zpa = qnorm(Pa)
    k1 = sqrt(1 + (k * k)/2)
    zp = k + (k1 * zpa/sqrt(n))
    p = 1 - pnorm(zp)
    OC = Pa
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
