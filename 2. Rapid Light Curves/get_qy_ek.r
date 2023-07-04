library(minpack.lm) # for nlsLM
library(broom) # for augment
library(dplyr) # for filter
library(ggplot2) # for filter
source("utils/christineTheme.r")

fit_qy <- function(data, plot = FALSE) {
    #
    # Initial Fit
    #############

    if (plot) {
        qy_plot <- ggplot() +
            geom_point(data = data, mapping = aes(x = PAR, y = QY)) +
            ggtitle(data[1, "SampleName"]) +
            christineTheme
    }

    fit1 <- nlsLM(
        QY ~ a * PAR^b,
        data = data,
        start = list(a = 2, b = -0.2),
        control = nls.control(maxiter = 500)
    )

    # Remove outliers
    #################

    fitted <- augment(fit1)
    outliers <- boxplot.stats(fitted$.resid)$out
    outliers <- fitted[fitted$.resid %in% outliers, ]

    if (length(outliers) == 0) {
        # No outliers, return this fit
        return(fit1)
    }

    # Fit without outliers
    ######################

    fit2 <- nlsLM(
        QY ~ a * PAR^b,
        data = filter(data, !PAR %in% outliers$PAR),
        start = list(a = 2, b = -0.2), # suggested start values the make sense with my data
        control = nls.control(maxiter = 500) # tries 500 values to find the best fit
    )


    if (plot) {
        new_data <- data.frame(PAR = seq(min(data$PAR), max(data$PAR), 1))
        new_data$QY <- predict(fit1, newdata = new_data)

        qy_plot <- qy_plot +
            geom_line(data = new_data, mapping = aes(x = PAR, y = QY), alpha = 0.1)

        new_data <- data.frame(PAR = seq(min(data$PAR), max(data$PAR), 1))
        new_data$QY <- predict(fit2, newdata = new_data)

        qy_plot <- qy_plot +
            geom_line(data = new_data, mapping = aes(x = PAR, y = QY))

        # plot outliers to see what's being removed
        if (length(outliers)) {
            qy_plot <- qy_plot +
                geom_point(data = outliers, aes(x = PAR, y = QY), colour = "red", pch = 4, size = 5)
        }

        print(qy_plot)
    }

    return(fit2)
}


# well_data <- filter(rlc_data, Well == 1, SampleName == "LI F2")
# fit_qy(well_data, plot = TRUE)

get_qy_ek <- function(data, ek) {
    fit <- fit_qy(data)
    co <- coefficients(fit)
    a <- co[["a"]]
    b <- co[["b"]]

    return(a * ek^b)
}
