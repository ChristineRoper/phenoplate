target <- "ETR_max"
input <- "2. Rapid Light Curves/outputs/rlc_etr_metrics_eilers.csv"
# target <- "QY_max"
# target <- "Ek"
# target <- "qy_ek"
# input <- "2. Rapid Light Curves/outputs/rlc_qy_metrics_eilers.csv"

output <- paste0("3. Thermal Curves/outputs/", target, "/ed50.csv")
plots <- paste0("3. Thermal Curves/outputs/", target, "/plots/")


library(dplyr)
library(drc)
library(broom)
source("utils/default_theme.r")

# Load Data
rlc_metrics <- read.csv(input, header = TRUE)
rlc_metrics$SampleID <- as.factor(rlc_metrics$SampleID)
rlc_metrics$Area <- as.factor(rlc_metrics$Area)

# samples <- list("W7", "LI F5", "MO S3", "TB 2") # trial representative samples
samples <- levels(as.factor(rlc_metrics$SampleID)) # all samples
outputs <- NULL

printOnTop <- function(...values) {
    cat("\33[2K\r", ...values, " ")
}

for (sample_name in samples) {
    printOnTop(sample_name)
    sample_data <- filter(rlc_metrics, SampleID == sample_name)

    # if (sample_data[sample_data$Area == 6, "ETR_max"] > sample_data[sample_data$Area == 5, "ETR_max"]) {
    #     sample_data[sample_data$Area == 6, "ETR_max"] <- NA
    # } else if (sample_data[sample_data$Area == 6, "ETR_max"] > sample_data[sample_data$Area == 4, "ETR_max"]) {
    #     sample_data[sample_data$Area == 6, "ETR_max"] <- NA
    # }
    # if (sample_data[sample_data$Area == 5, "ETR_max"] > sample_data[sample_data$Area == 4, "ETR_max"]) {
    #     sample_data[sample_data$Area == 5, "ETR_max"] <- NA
    # }

    # plot data and model fit
    sample_data$Target <- sample_data[[target]]
    plot <- ggplot(sample_data, aes(Temperature, Target)) +
        geom_point() +
        default_theme +
        labs(
            x = "Temperature (ºC)", y = target,
            title = sample_name
        )

    #######
    # Fit #
    #######

    formula_str <- paste(target, "~ Temperature")
    drc <- drm(as.formula(formula_str),
        data = sample_data,
        fct = LL.3()
    )

    # skip the rest if we couldn't fit
    if (is.null(drc)) {
        suppressMessages(ggsave(paste0(plots, "failed/", sample_name, ".png"), plot))
        next # go on to the next sample
    }

    #################
    # Output Params #
    #################

    max <- coef(drc)["d:(Intercept)"]
    ed50 <- coef(drc)["e:(Intercept)"]
    slope <- coef(drc)["b:(Intercept)"]
    output_for_this_sample <- data.frame(
        SampleID = sample_name,
        max = max,
        ed50 = ed50,
        slope = slope,
        ip = ed50 - max / slope / 2
    )

    # add extra cols
    output_for_this_sample <- cbind(output_for_this_sample, sample_data[1, -c(1, seq(5, 38))])

    outputs <- rbind(outputs, output_for_this_sample)

    ########
    # Plot #
    ########

    new_data <- data.frame(Temperature = seq(1, 45, 0.2))
    new_data$predicted <- predict(drc, newdata = new_data)

    # plot data and model fit
    plot <- plot +
        # ED50
        geom_segment(aes(x = ed50, xend = ed50, y = -10, yend = max / 2), output_for_this_sample, linetype = "dashed") +
        # ip
        geom_segment(aes(x = ip, xend = ip, y = -10, yend = max), output_for_this_sample, linetype = "dashed") +
        # Max
        geom_hline(aes(yintercept = max), output_for_this_sample, linetype = "dashed") +
        # Slope
        geom_abline(aes(slope = -slope, intercept = slope * ed50 + max / 2), output_for_this_sample, linetype = "dotted") +
        # Curve
        geom_line(aes(Temperature, predicted), new_data, col = "blue") +
        # ED50 point
        geom_point(aes(ed50, max / 2), output_for_this_sample, color = "blue", size = 4, pch = 4) +
        coord_cartesian(
            xlim = c(24, 38),
            ylim = c(0, max(sample_data[[target]]))
        )

    suppressMessages(ggsave(paste0(plots, sample_name, "-ll.png"), plot))
}

write.csv(outputs, output)

# summary(outputs)

# ggplot(outputs, aes(x = ip)) +
#     geom_density()
# ggplot(outputs, aes(x = ed50)) +
#     geom_density()
# ggplot(outputs, aes(x = max)) +
#     geom_density()
# ggplot(outputs, aes(x = slope)) +
#     geom_density()
