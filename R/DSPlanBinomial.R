DSPlanBinomial <-
function (N, n1, n2, Ac1, Re1, Ac2, p = seq(0, 0.25, 0.005), 
    Plots = TRUE) 
{
    Pa1 = pbinom(Ac1, n1, p)
    limits = (Ac1 + 1):(Re1 - 1)
    i = 0
    Pa2 = matrix(0, ncol = length(limits), nrow = length(p))
    for (d1 in limits) {
        i = i + 1
        pc = dbinom(d1, n1, p) * pbinom((Ac2 - d1), n2, p)
        Pa2[, i] = pc
        Pa2
    }
    Pa2 = rowSums(Pa2)
    OC = Pa1 + Pa2
    ASN = n1 + n2 * (pbinom((Re1 - 1), n1, p) - pbinom(Ac1, n1, 
        p))
    AOQ = (p * Pa1 * (N - n1) + p * Pa2 * (N - n1 - n2))/N
    ATI = n1 * Pa1 + (n1 + n2) * Pa2 + (1 - OC) * N
    results = list(p = p, OC = OC, ASN = ASN, AOQ = AOQ, ATI = ATI, 
        Pa1 = Pa1, Pa2 = Pa2)
    class(results) = "AccSampPlan"
    if (Plots) {
        par(mfrow = c(2, 2))
        plot(results)
    }
    return(results)
}
