# Processes the RLC part of the Fluorcam data with PI curves to extract Pmax, alpha, ek

input <- "2. Rapid Light Curves/outputs/fluorcam_RLC_data.csv"
output <- "2. Rapid Light Curves/outputs/rlc_qy_metrics_eilers.csv"
plots <- "2. Rapid Light Curves/outputs/QY/plots/"

remove_outliers <- FALSE # When fitting curve, should we remove outlier residuals and re-fit

##

rlc_data <- read.csv(input, header = TRUE)
rlc_data$SampleID <- as.factor(rlc_data$SampleID)
rlc_data$Area <- as.factor(rlc_data$Area)

library(ggplot2)
library(nls.multstart)
library(minpack.lm) # for nlsLM
library(broom) # for augment
source("utils/default_theme.r")
source("utils/nls_outlier_removal.r")


webb_pi <- function(I, Pmax, alpha) {
    Ek <- Pmax / alpha
    return(Pmax * (1 - exp(-I / Ek)))
}

webb_qy_pi <- function(QY_max, Ek, I) { # modified for QY as per Emma's Excel sheet
    Pmax <- QY_max * Ek
    alpha <- Pmax / Ek
    return(webb_pi(I, Pmax, alpha) / I)
}

eilers_pi <- function(Pmax, Iopt, a, I) {
    pi <- (Pmax * I) / ((Pmax / (a * Iopt^2)) * I^2 + ((1 - (2 * Pmax) / (a * Iopt)) * I) + (Pmax / a))
    return(pi)
}

eilers_qy_pi <- function(QY_max, IoptExtra, Ek, I) {
    Iopt <- Ek + IoptExtra
    Pmax <- QY_max * Ek
    alpha <- Pmax / Ek
    return(eilers_pi(Pmax, Iopt, alpha, I) / I)
}

sample_ids <- levels(as.factor(rlc_data$SampleID)) # all samples

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

    # Ensure no QY is negative
    sample_data$QY <- pmax(sample_data$QY, 0)

    for (area in 1:6) {
        area_data <- filter(sample_data, Area == area)

        simple_max <- max(area_data$QY)

        # Fit
        ###########

        # First par is never an outlier
        handle_outliers <- function(outliers) filter(outliers, PAR > 100)

        fit_data <- function(data) {
            tryCatch(
                {
                    fit <- nls_multstart(
                        QY ~ eilers_qy_pi(QY_max, IoptExtra, Ek, PAR),
                        data = data,
                        iter = c(1, 3, 5),
                        start_lower = list(QY_max = simple_max, IoptExtra = 0, Ek = 20),
                        start_upper = list(QY_max = simple_max, IoptExtra = 200, Ek = 1000),
                        lower = c(0, 0, 0),
                        upper = c(0.8, 1700, 1700),
                        supp_errors = "Y",
                        convergence_count = FALSE
                    )
                    # nlsLM(
                    #     QY ~ eilers_qy_pi(QY_max, IoptExtra, Ek, PAR),
                    #     data = data,
                    #     start = list(QY_max = simple_max, IoptExtra = 100, Ek = 500),
                    #     lower = c(0, 0, 0),
                    #     upper = c(0.8, 1700, 1700),
                    #     control = nls.lm.control(maxiter = 500)
                    # )
                },
                error = function(e) {
                    cat("error in ", sample, " area ", area, " fit 1\n")
                    stop(e)
                }
            )
        }

        fit <- nls_outlier_removal(
            area_data,
            fit_data = fit_data,
            handle_outliers = handle_outliers
        )

        outliers <- fit$outliers
        fit <- fit$fit

        # plot them to see what's being removed
        if (length(outliers)) {
            plot <- plot +
                geom_point(data = outliers, aes(x = PAR, y = QY), colour = "red", pch = 1, size = 3)
        }

        Ek <- coef(fit)[["Ek"]]
        QY_max <- coef(fit)[["QY_max"]]
        IoptExtra <- coef(fit)[["IoptExtra"]]
        Iopt <- Ek + IoptExtra

        new_row <- data.frame(
            QY_max = QY_max,
            Ek = Ek,
            qy_ek = eilers_qy_pi(QY_max, IoptExtra, Ek, Ek),
            Iopt = Iopt,
            AIC = AIC(fit),
            outliersRemoved = nrow(outliers)
        )
        # add extra columns
        new_row <- cbind(new_row, area_data[1, -c(1, 5, 6, 7)])

        output_data <- rbind(output_data, new_row)

        plot <- plot +
            geom_segment(data = data.frame(QY_max = QY_max, Ek = Ek, IoptExtra = IoptExtra), mapping = aes(x = Ek, y = 0, xend = Ek, yend = eilers_qy_pi(QY_max, IoptExtra, Ek, Ek)), color = "grey", alpha = 0.5) +
            geom_point(data = data.frame(QY_max = QY_max, Ek = Ek, IoptExtra = IoptExtra), mapping = aes(x = Ek, y = eilers_qy_pi(QY_max, IoptExtra, Ek, Ek)), pch = 4, size = 2.5) +
            geom_segment(data = data.frame(QY_max = QY_max, Ek = Ek, IoptExtra = IoptExtra), mapping = aes(x = 0, y = QY_max, xend = Ek, yend = 0), color = "grey", linetype = "dotted", alpha = 0.4)

        # Save the curve for plotting later
        pars <- seq(0, 1800, 1) # get pars from 0 to 1800 to plot the curve against
        curve <- data.frame(PAR = pars, QY = eilers_qy_pi(QY_max, IoptExtra, Ek, pars), Area = area)
        fit_curves <- rbind(fit_curves, curve)
    }

    fit_curves$Area <- as.factor(fit_curves$Area)


    # plot
    plot <- plot +
        geom_hline(yintercept = 0, color = "grey", alpha = 0.6) +
        # Data
        geom_line(data = sample_data, mapping = aes(x = PAR, y = QY, col = Area), alpha = 0.2) +
        geom_point(data = sample_data, mapping = aes(x = PAR, y = QY, col = Area)) +
        # Fit 1
        # geom_line(data = sample_data, mapping = aes(x = PAR, y = QY_fit, col = Area), linetype = 1) +
        geom_line(data = fit_curves, mapping = aes(x = PAR, y = QY, col = Area), linetype = 1) +

        coord_cartesian(ylim = c(0, 1), xlim = c(0, max(sample_data$PAR))) +
        ggtitle(sample)

    # print(plot)
    suppressMessages(ggsave(paste0(plots, sample, "Eilers.png"), plot))
}

cat("\n")

write.csv(output_data, output)
