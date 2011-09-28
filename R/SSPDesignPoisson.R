SSPDesignPoisson <-
function (AQL, alpha, LQL, beta) 
{
    nl = function(Ac, LQL, beta) {
        n = 1
        while (ppois(Ac, n * LQL) >= beta) {
            n = n + 1
        }
        n
    }
    nu = function(Ac, AQL, alpha) {
        n = 1
        while (ppois(Ac, n * AQL) >= 1 - alpha) {
            n = n + 1
        }
        n
    }
    Ac = 0
    while (nl(Ac, LQL, beta) > nu(Ac, AQL, alpha)) {
        Ac = Ac + 1
    }
    n = nl(Ac, LQL, beta)
    return(data.frame(n, Ac))
}
