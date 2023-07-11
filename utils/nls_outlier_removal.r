library(minpack.lm) # for nlsLM
library(broom) # for augment
library(dplyr) # for filter

nls_outlier_removal <- function(data, fit_data, handle_outliers = identity) {
    # fit all data
    fit <- fit_data(data)

    if (remove_outliers == FALSE) {
        return(list(fit = fit, outliers = data.frame()))
    }

    # Remove outliers

    fitted <- augment(fit)
    # Find outliers using boxplot's internals
    outliers <- boxplot.stats(fitted$.resid)$out
    outliers <- filter(data, fitted$.resid %in% outliers)

    # no outliers, return fit
    if (nrow(outliers) == 0) {
        return(list(fit = fit, outliers = outliers))
    }

    # allow outliers to be acted on, manipulated, etc
    outliers <- handle_outliers(outliers)

    # filter outliers
    cols <- colnames(fitted) # get the names of the columns used in the fit
    # check if each row in the data is in the outlier list
    x_in_outliers <- data[, cols[1]] %in% outliers[, cols[1]]
    y_in_outliers <- data[, cols[2]] %in% outliers[, cols[2]]
    # filter, keeping those rows that are not in the outlier list
    data <- filter(data, !x_in_outliers | !y_in_outliers)

    # update fit
    fit <- fit_data(data)

    return(list(fit = fit, outliers = outliers))
}
