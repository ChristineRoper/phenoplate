input <- "1. Fluorcam data/outputs/fluorcam_data.csv"
output <- "3. Thermal Curves/outputs/NPQ_residual/exponential raw output.csv"
plots <- "3. Thermal Curves/outputs/NPQ_residual/plots/"

library(dplyr)
library(broom)
source("utils/default_theme.r")

# Load Data
data <- read.csv(input, header = TRUE)

data$NPQ_residual <- pmax(0, (data$T1_Fm - data$T3_Fm) / data$T3_Fm)

# NPQ Residual
library(minpack.lm) # for nlsLM
library(ggplot2) # for nlsLM
source("utils/nls_outlier_removal.r")


printOnTop <- function(...values) {
    cat("\33[2K\r", ...values)
}

# samples <- list("BH 13") # ,"BH 13", "LI F5", "MO S3", "TB 2", "W7") # trial representative samples
samples <- levels(as.factor(data$SampleID)) # all samples
outputs <- NULL
for (sample_name in samples) {
    printOnTop(sample_name)
    sample_data <- filter(data, SampleID == sample_name)

    # Create the plot
    #################
    plot <- ggplot(sample_data, aes(Temperature, NPQ_residual)) +
        geom_point() +
        default_theme +
        coord_cartesian(
            xlim = c(min(sample_data$Temperature), max(sample_data$Temperature))
        ) +
        labs(
            x = "Temperature (ºC)", y = "NPQ Residual",
            title = sample_name
        )

    #######
    # Fit #
    #######

    initial <- sample_data[1, "NPQ_residual"]

    fit_data <- function(data) {
        nlsLM(
            NPQ_residual ~ a * exp(r * Temperature),
            data = data,
            start = list(a = 0.01, r = 0.1),
            control = nls.lm.control(maxiter = 500)
        )
    }

    fit <- nls_outlier_removal(
        sample_data,
        fit_data = fit_data
    )

    outliers <- fit$outliers
    fit <- fit$fit

    # plot them to see what's being removed
    if (length(outliers)) {
        plot <- plot +
            geom_point(data = outliers, aes(x = Temperature, y = NPQ_residual), colour = "red", pch = 1, size = 3)
    }

    #################
    # Output Params #
    #################

    a <- coef(fit)[["a"]]
    r <- coef(fit)[["r"]]
    output_for_this_sample <- data.frame(
        SampleID = sample_name,
        a = a,
        r = r
        # AIC = AIC(fit),
        # RSS = deviance(fit)
    )

    # add extra cols
    output_for_this_sample <- cbind(output_for_this_sample, sample_data[1, -seq(1, 9)])

    outputs <- rbind(outputs, output_for_this_sample)

    ########
    # Plot #
    ########

    new_data <- data.frame(Temperature = seq(min(sample_data$Temperature) - 10, max(sample_data$Temperature) + 10, 0.2))
    predictions <- augment(fit, newdata = new_data)

    # Add fitted data to plot
    plot <- plot +
        geom_line(aes(Temperature, .fitted), predictions, col = "blue") +
        labs(
            # subtitle = paste("AIC", AIC(fit)),
            caption = paste("a:", a, "r:", r)
        ) +
        coord_cartesian(
            ylim = c(min(data$NPQ_residual), max(data$NPQ_residual)),
            xlim = c(min(sample_data$Temperature), max(sample_data$Temperature))
        )

    # print(plot)
    suppressMessages(ggsave(paste0(plots, sample_name, "-exp.png"), plot))
}


write.csv(outputs, output)
