Sequential <-
function (AQL, alpha, LQL, beta, Plots = TRUE) 
{
    a = log((1 - beta)/alpha)
    b = log((1 - alpha)/beta)
    g1 = log(LQL/AQL)
    g2 = log((1 - AQL)/(1 - LQL))
    G = g1 + g2
    h1 = b/G
    h2 = a/G
    s = g2/G
    h = seq(-4 * h1, 5 * h2, 0.01)
    p = (1 - ((1 - LQL)/(1 - AQL))^h)/(((LQL/AQL)^h) - (((1 - 
        LQL)/(1 - AQL))^h))
    k1 = ((1 - beta)/alpha)^h
    k2 = (beta/(1 - alpha))^h
    OC = (k1 - 1)/(k1 - k2)
    AOQ = p * OC
    k3 = OC * log(beta/(1 - alpha)) + (1 - OC) * log((1 - beta)/alpha)
    k4 = p * log(LQL/AQL) + (1 - p) * log((1 - LQL)/(1 - AQL))
    ASN = k3/k4
    L = round(2 * h1/s)
    k = seq(1, L, 1)
    accept = s * k - h1
    reject = s * k + h2
    if (Plots) {
        par(mfrow = c(2, 2))
        plot(accept ~ k, type = "l", ylab = expression(d[k]), 
            xlab = "k", ylim = c(min(accept), max(reject)))
        par(new = TRUE)
        plot(reject ~ k, type = "l", ylab = "", xlab = "", ylim = c(min(accept), 
            max(reject)))
        title("Sequential Acceptance Chart")
        axis(1, tck = 1, col = "grey", lty = "dotted")
        axis(2, tck = 1, col = "grey", lty = "dotted")
        text(median(k), min(accept), "ACCEPT")
        text(median(k), max(reject), "REJECT")
        text(median(k), max(accept), "CONTINUE")
        plot(OC ~ p, type = "l", ylab = "Probability of Acceptance", 
            xlab = "Fraction Nonconforming p")
        title("OC Curve")
        plot(ASN ~ p, type = "l", ylab = "Average sample size", 
            xlab = "Fraction Nonconforming p")
        title(paste("maximum ASN = ", formatC(max(ASN))))
        plot(AOQ ~ p, type = "l", ylab = "AOQ", xlab = "Fraction Nonconforming p")
        title(paste("AOQL = ", formatC(max(AOQ))))
        par(mfrow = c(1, 1))
    }
    return(rbind(h1, h2, s))
}
