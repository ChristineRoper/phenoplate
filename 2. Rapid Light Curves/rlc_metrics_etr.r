# Processes the RLC part of the Fluorcam data with PI curves to extract Pmax, alpha, ek

input <- "2. Rapid Light Curves/outputs/fluorcam_RLC_data.csv"
output <- "2. Rapid Light Curves/outputs/rlc_etr_metrics_webb.csv"
plots <- "2. Rapid Light Curves/outputs/ETR/plots/"

remove_outliers <- FALSE # When fitting curve, should we remove outlier residuals and re-fit

##

rlc_data <- read.csv(input, header = TRUE)
rlc_data$SampleID <- as.factor(rlc_data$SampleID)
rlc_data$Area <- as.factor(rlc_data$Area)

library(ggplot2)
library(minpack.lm) # for nlsLM
library(broom) # for augment
source("utils/default_theme.r")
source("utils/nls_outlier_removal.r")


webb_pi <- function(Pmax, alpha, I) {
    Ek <- Pmax / alpha
    return(Pmax * (1 - exp(-I / Ek)))
}

eilers_pi <- function(Pmax, Iopt, a, I) {
    pi <- (Pmax * I) / ((Pmax / (a * Iopt^2)) * I^2 + ((1 - (2 * Pmax) / (a * Iopt)) * I) + (Pmax / a))
    return(pi)
}

sample_ids <- levels(rlc_data$SampleID) # all samples

output_data <- NULL

print_on_top <- function(...values) {
    cat("\33[2K\r", ...values)
}


cat("\n")
for (sample in sample_ids) {
    print_on_top(sample)

    plot <- ggplot() + default_theme
    fit_curves <- NULL

    sample_data <- filter(rlc_data, SampleID == sample)

    # Ensure no ETR is negative
    sample_data$ETR <- pmax(sample_data$ETR, 0)

    for (area in 1:6) {
        area_data <- filter(sample_data, Area == area)

        simple_max <- max(area_data$ETR)

        # Fit
        ###########

        # First par is never an outlier
        handle_outliers <- function(outliers) filter(outliers, PAR > 100)

        fit_data <- function(data) {
            nlsLM(
                ETR ~ webb_pi(Pmax, a, PAR),
                data = data,
                start = list(Pmax = simple_max, a = 0.2),
                # lower = c(0, 0),
                # upper = c(simple_max * 1.2, 3),
                control = nls.lm.control(maxiter = 500)
            )
        }


        fit <- tryCatch(
            {
                nls_outlier_removal(
                    area_data,
                    fit_data = fit_data,
                    handle_outliers = handle_outliers
                )
            },
            error = message
        )

        # skip the rest if we couldn't fit
        if (is.null(fit)) {
            fail_plot <- plot +
                geom_line(data = sample_data, mapping = aes(x = PAR, y = ETR, col = Area), alpha = 0.2) +
                geom_point(data = sample_data, mapping = aes(x = PAR, y = ETR, col = Area))
            suppressMessages(ggsave(paste0(plots, "failed/", sample, "Webb.png"), fail_plot))
            next # go on to the next sample
        }

        outliers <- fit$outliers
        fit <- fit$fit

        # plot them to see what's being removed
        if (length(outliers)) {
            plot <- plot +
                geom_point(data = outliers, aes(x = PAR, y = ETR), colour = "red", pch = 1, size = 3)
        }

        ETR_max <- coef(fit)[["Pmax"]]
        alpha <- coef(fit)[["a"]]
        Ek <- ETR_max / alpha

        new_row <- data.frame(
            ETR_max = ETR_max,
            Ek = Ek,
            alpha = alpha,
            AIC = AIC(fit),
            outliersRemoved = nrow(outliers)
        )
        # add extra columns
        new_row <- cbind(new_row, area_data[1, -c(1, 5, 6, 7)])

        output_data <- rbind(output_data, new_row)

        plot <- plot +
            geom_hline(yintercept = ETR_max, color = "grey", alpha = 0.4) +
            geom_segment(data = data.frame(ETR_max = ETR_max, Ek = Ek), mapping = aes(x = Ek, y = 0, xend = Ek, yend = ETR_max), color = "grey", alpha = 0.5) +
            geom_point(data = data.frame(ETR_max = ETR_max, Ek = Ek), mapping = aes(x = Ek, y = ETR_max), pch = 4, size = 2.5) +
            geom_segment(data = data.frame(ETR_max = ETR_max, Ek = Ek), mapping = aes(x = 0, y = 0, xend = Ek, yend = ETR_max), color = "grey", linetype = "dotted", alpha = 0.4)

        # Save the curve for plotting later
        pars <- seq(0, 1800, 1) # get pars from 0 to 1800 to plot the curve against
        curve <- data.frame(PAR = pars, ETR = webb_pi(ETR_max, alpha, pars), Area = area)
        fit_curves <- rbind(fit_curves, curve)
    }

    fit_curves$Area <- as.factor(fit_curves$Area)


    # plot
    plot <- plot +
        # Data
        geom_line(data = sample_data, mapping = aes(x = PAR, y = ETR, col = Area), alpha = 0.2) +
        geom_point(data = sample_data, mapping = aes(x = PAR, y = ETR, col = Area)) +
        # Fit 1
        geom_line(data = fit_curves, mapping = aes(x = PAR, y = ETR, col = Area), linetype = 1) +

        # coord_cartesian(ylim = c(0, 1), xlim = c(0, max(sample_data$PAR))) +
        ggtitle(sample)

    # print(plot)
    suppressMessages(ggsave(paste0(plots, sample, "Webb.png"), plot))
}

cat("\n")

write.csv(output_data, output)
