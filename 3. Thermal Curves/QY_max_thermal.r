input <- "2. Rapid Light Curves/outputs/rlc_qy_metrics_webb.csv"
output <- "3. Thermal Curves/outputs/QY_max/inverted exponential.csv"
plots <- "3. Thermal Curves/outputs/QY_max/plots/"

library(dplyr)
library(broom)
source("utils/default_theme.r")

# Load Data
rlc_metrics <- read.csv(input, header = TRUE)
rlc_metrics$SampleID <- as.factor(rlc_metrics$SampleID)
rlc_metrics$Area <- as.factor(rlc_metrics$Area)

# have a look
ggplot(rlc_metrics, aes(x = Ek, y = QY_max, col = Area)) +
    scale_y_log10() +
    scale_x_log10() +
    geom_point() +
    default_theme

ggplot(rlc_metrics, aes(x = as.numeric(Area), y = QY_max, col = SampleID)) +
    # scale_y_log10() +
    coord_cartesian(ylim = c(0, 0.8)) +
    geom_line(alpha = 0.6) +
    geom_point() +
    theme(legend.position = "none") +
    default_theme

# Output QY_max
library(minpack.lm) # for nlsLM

# samples <- list("W7", "LI F5", "MO S3", "TB 2") # trial representative samples
samples <- levels(as.factor(rlc_metrics$SampleID)) # all samples
outputs <- NULL

printOnTop <- function(...values) {
    cat("\33[2K\r", ...values, " ")
}

for (sample_name in samples) {
    printOnTop(sample_name)
    sample_data <- filter(rlc_metrics, SampleID == sample_name)

    if (sample_data[sample_data$Area == 6, "QY_max"] > sample_data[sample_data$Area == 5, "QY_max"]) {
        sample_data[sample_data$Area == 6, "QY_max"] <- NA
    } else if (sample_data[sample_data$Area == 6, "QY_max"] > sample_data[sample_data$Area == 4, "QY_max"]) {
        sample_data[sample_data$Area == 6, "QY_max"] <- NA
    }
    if (sample_data[sample_data$Area == 5, "QY_max"] > sample_data[sample_data$Area == 4, "QY_max"]) {
        sample_data[sample_data$Area == 5, "QY_max"] <- NA
    }
    # a copy before setting some to NA below
    all_points <- sample_data

    # plot data and model fit
    plot <- ggplot(sample_data, aes(Temperature, QY_max)) +
        geom_point() +
        default_theme +
        labs(
            x = "Temperature (ºC)", y = "QY EK",
            title = sample_name
        )

    #######
    # Fit #
    #######

    initial <- sample_data[1, "QY_max"]

    fit <- tryCatch(
        {
            nlsLM(
                QY_max ~ I - exp(-S + k * Temperature),
                data = sample_data,
                start = list(I = initial, S = 10, k = 1), # suggested start values the make sense with my data
                control = nls.control(maxiter = 500) # tries 500 values to find the best fit
            )
        },
        error = message
    )

    # skip the rest if we couldn't fit
    if (is.null(fit)) {
        suppressMessages(ggsave(paste0(plots, "failed/", sample_name, ".png"), plot))
        next # go on to the next sample
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
        I = I
    )

    # add extra cols
    output_for_this_sample <- cbind(output_for_this_sample, sample_data[1, -seq(1, 9)])

    outputs <- rbind(outputs, output_for_this_sample)

    ########
    # Plot #
    ########

    new_data <- data.frame(Temperature = seq(1, 45, 0.2))
    predictions <- augment(fit, newdata = new_data)

    # plot data and model fit
    plot <- plot +
        geom_line(aes(Temperature, .fitted), predictions, col = "blue") +
        coord_cartesian(
            xlim = c(23, 40),
            ylim = c(0, 1)
        )
    # ylim = c(min(rlc_metrics$QY_max), max(rlc_metrics$QY_max)))

    suppressMessages(ggsave(paste0(plots, sample_name, ".png"), plot))
}


write.csv(outputs, output)
