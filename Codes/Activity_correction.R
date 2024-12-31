library(tidyverse)
library(activity)
library(astroFns)

flatfile=read.csv("Input/flatfile_1sec.csv")
Species = unique(flatfile$Species)

estimate_activity <- function(flatfile, species) {
  # Filter data for the given species
  activity_data <- flatfile %>%
    filter(Species == species) %>%
    filter(!is.na(DateTimeOriginal)) %>%
    filter(diff > 300 | diff == 0)
  
  # Check if there is enough data to estimate activity
  if (nrow(activity_data) == 0) {
    message(paste("No data available for species:", species))
    return(activity_matrix)  # Return the unchanged activity matrix
  }
  
  # Estimate activity pattern using fitact
  activity_estimate <- fitact(hms2rad(strftime(activity_data$DateTimeOriginal, format="%H:%M:%S")), sample="data")
  
  # Return the updated activity matrix
  return(activity_estimate)
}

###Example:
dfox = estimate_activity(flatfile, "DESERT_FOX")
plot(dfox)

activity_matrix = data.frame()
for (i in 1:length(Species)) {
  activity = estimate_activity(flatfile, Species[i])
  plot(activity)
  activity_matrix_temp <- data.frame(Species = Species[i]) %>%
    cbind(t(activity@act))
  
  activity_matrix <- activity_matrix %>%
    rbind(activity_matrix_temp) %>%
    unique.data.frame()
  rm(activity)
}

write_rds(activity_matrix,"Input/activity_matrix")
