input <- "2. Rapid Light Curves/outputs/rlc_qy_metrics_webb.csv"
plots <- "3. Thermal Curves/outputs/Ek/plots/"
output <- "3. Thermal Curves/outputs/Ek/exponential raw output.csv"

remove_outliers <- FALSE # When fitting curve, should we remove outlier residuals and re-fit


library(minpack.lm) # for nlsLM
library(dplyr) # for filter
library(ggplot2)
library(dplyr)
library(broom)
source("utils/default_theme.r")
source("utils/nls_outlier_removal.r")

# Load Data
rlc_metrics <- read.csv(input, header = TRUE)

# Have a look
ggplot(rlc_metrics, aes(x = as.numeric(Area), y = Ek, col = SampleID)) +
    # scale_y_log10() +
    # coord_cartesian(ylim = c(0, 0.8)) +
    geom_line(alpha = 0.6) +
    geom_point() +
    theme(legend.position = "none") +
    default_theme

samples <- levels(as.factor(rlc_metrics$SampleID)) # all samples
outputs <- NULL
for (sample_name in samples) {
    print(sample_name)
    sample_data <- filter(rlc_metrics, SampleID == sample_name)

    # Create the plot
    #################
    plot <- ggplot(sample_data, aes(Temperature, Ek)) +
        geom_point() +
        default_theme +
        coord_cartesian(
            xlim = c(min(sample_data$Temperature), max(sample_data$Temperature)),
            ylim = c(min(sample_data$Ek), max(sample_data$Ek))
        ) +
        labs(
            x = "Temperature (ºC)", y = "Ek",
            title = sample_name
        )

    #######
    # Fit #
    #######

    initial <- sample_data[1, "Ek"]

    fit_data <- function(data) {
        nlsLM(
            Ek ~ I - exp(-S + k * Temperature),
            data = data,
            start = list(I = initial, S = 15, k = 0.7),
            control = nls.lm.control(maxiter = 500)
        )
    }

    fit <- tryCatch(
        {
            nls_outlier_removal(
                sample_data,
                fit_data = fit_data
            )
        },
        error = message
    )

    # skip the rest if we couldn't fit
    if (is.null(fit)) {
        suppressMessages(ggsave(paste0(plots, "failed/", sample_name, ".png"), plot))
        next # go on to the next sample
    }

    outliers <- fit$outliers
    fit <- fit$fit

    # plot them to see what's being removed
    if (length(outliers)) {
        plot <- plot +
            geom_point(data = outliers, aes(x = Temperature, y = Ek), colour = "red", pch = 1, size = 3)
    }



    #################
    # Output Params #
    #################

    S <- coef(fit)[["S"]]
    k <- coef(fit)[["k"]]
    I <- coef(fit)[["I"]]
    output_for_this_sample <- data.frame(
        SampleID = sample_name,
        S = S,
        k = k,
        I = I,
        AIC = AIC(fit),
        RSS = deviance(fit)
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
        geom_line(aes(Temperature, .fitted), predictions, col = "blue")

    # print(plot)
    suppressMessages(ggsave(paste0(plots, sample_name, "-exp.png"), plot))
}


write.csv(outputs, output)
