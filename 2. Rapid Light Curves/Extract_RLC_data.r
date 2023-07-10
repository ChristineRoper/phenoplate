# fluorcam_data from extract_fluorcam_data.rmd and creates a data frame with cols:
# SampleID, Area, Reef, Site, Habitat, Pale, Group, Temperature, PAR, ETR, QY

fluorcam_data <- "1. Fluorcam data/outputs/fluorcam_data.csv"
output <- "2. Rapid Light Curves/outputs/fluorcam_RLC_data.csv"

##

fluorcam_data <- read.csv(fluorcam_data)
etr_data <- NULL

for (i in seq_len(nrow(fluorcam_data))) {
    sample <- fluorcam_data[i, ]
    for (m in seq(1:15)) {
        new_row <- data.frame(
            SampleID = sample$SampleID,
            Area = sample$Area,
            Temperature = sample$Temperature,
            PAR = fluorcam_data[i, paste0("T2_PAR", m)],
            ETR = fluorcam_data[i, paste0("T2_ETR", m)],
            QY = fluorcam_data[i, paste0("T2_QY_Lss", m)]
        )
        new_row <- cbind(new_row, sample[, 69:ncol(fluorcam_data)])
        etr_data <- rbind(etr_data, new_row)
    }
}

write.csv(etr_data, output)
