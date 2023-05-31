library(minpack.lm)

fit_qy <- function(data) {
    fit <- nlsLM(
        QY ~ a * PAR^b,
        data = data,
        start = list(a = 2, b = -0.2), # suggested start values the make sense with my data
        control = nls.control(maxiter = 500) # tries 500 values to find the best fit
    )

    return(fit)
}


get_qy_ek <- function(data, ek) {
    fit <- fit_qy(data)
    co <- coefficients(fit)
    a <- co[["a"]]
    b <- co[["b"]]

    return(a * ek^b)
}
