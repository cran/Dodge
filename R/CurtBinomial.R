CurtBinomial <-
function (n, Ac, p = seq(0, 0.5, 0.01), Plots = TRUE) 
{
    q = 1 - p
    ASN.full = pbinom(Ac, n + 1, p) * ((n - Ac)/(n * q)) + (1 - 
        pbinom(Ac + 1, n + 1, p)) * ((Ac + 1)/(n * p))
    ASN.full = n * ASN.full
    ASN.semi = pbinom(Ac, n, p) + (1 - pbinom(Ac + 1, n + 1, 
        p)) * ((Ac + 1)/(n * p))
    ASN.semi = n * ASN.semi
    if (any(p == 0)) {
        ASN.semi[p == 0] = n
        ASN.full[p == 0] = n - Ac
    }
    if (Plots) {
        plot(ASN.full ~ p, type = "l", ylim = c(1, n), ylab = "ASN", 
            col = "red", lty = 2)
        par(new = TRUE)
        plot(ASN.semi ~ p, type = "l", ylim = c(1, n), ylab = "", 
            col = "blue", lty = 1)
        legend("topright", legend = c("Fully Curtailed ASN", 
            "Semi-curtailed ASN"), lty = 2:1, col = c("red", 
            "blue"))
    }
    return(data.frame(p, ASN.semi, ASN.full))
}
