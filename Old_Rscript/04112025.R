


library(dplyr)
library(readr)



getwd()
setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_CH/Seed_predation_CH_analysis_Desktop")
list.files()
list.files("Data/CameraData")


# Step 1 — list all files
path <- "Data/CameraData"
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Step 2 — create an empty data frame
CameraData_all <- data.frame()

# Step 3 — loop over files, read each, and add metadata
for (file in files) {
  
  # Read the file
  temp <- read_csv(file, show_col_types = FALSE, guess_max = 10000)
  
  # Force "Sighting Count" to numeric (if column exists)
  if ("Sighting Count" %in% names(temp)) {
    temp[["Sighting Count"]] <- suppressWarnings(as.numeric(temp[["Sighting Count"]]))
  }
  
  # Extract file name
  file_name <- basename(file)
  
  # Extract site (first 3 letters before the first "_")
  site <- sub("_.*", "", file_name)
  
  # Extract treatment ("FULL" or "EMPTY")
  treatment <- if (grepl("FULL", file_name)) "FULL" else "EMPTY"
  
  # Add metadata columns
  temp <- temp %>%
    mutate(
      file_name = file_name,
      site = site,
      treatment = treatment
    )
  
  # Bind to the main dataframe
  CameraData_all <- bind_rows(CameraData_all, temp)
}

# Step 4 — summary check
CameraData_all %>%
  count(site, treatment)


CameraData_all_clean <- CameraData_all%>%
  filter(`Sighting Count`!="0")

saveRDS(CameraData_all_clean, file= "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_CH/Seed_predation_CH_analysis_Desktop/Data/CameraData_clean")

sighting_summary<- CameraData_all_clean%>%
  group_by(Species, site, "site Name", treatment) %>%
  summarize(total_sightings= sum(`Sighting Count`, na.rm= TRUE),
  .groups= "drop")

CameraData_all_clean<-CameraData_all_clean%>%
  rename ("Micromys" = "Apodemus")
