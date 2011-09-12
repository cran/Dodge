VSPDesign <-
function (AQL, alpha, LQL, beta) 
{
    zp1 = qnorm(1 - AQL)
    zp2 = qnorm(1 - LQL)
    zpa1 = qnorm(1 - alpha)
    zpa2 = qnorm(1 - beta)
    k = (zp2 * zpa1 + zp1 * zpa2)/(zpa1 + zpa2)
    n = (zpa1 + zpa2)/(zp1 - zp2)
    n = n * n
    n = round(n)
    n.unknown = n * (1 + (k * k/2))
    return(c(k, n, n.unknown))
}
