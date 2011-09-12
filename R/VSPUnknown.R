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
