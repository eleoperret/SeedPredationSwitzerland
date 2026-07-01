
#cODE TO MERGE ALL DATASHEET TOGETHER
#22.07.2025
#Code where I merged all datasheet for the environmental data, the shelter data and the station installation data. 


# install.packages("readxl")
# install.packages("dplyr")
# install.packages("purrr")
library(readxl)
library(dplyr)
library(purrr)

# 1. Define the folder path
setwd("C:/Users/eperret/Desktop/Seed_predation_CH_analysis/Data")
folder_path <- "Environmental_data"

# 2. List all Excel files in the folder
file_list <- list.files(folder_path, pattern = "\\.xlsx$", full.names = TRUE)

# Read all Excel files with all columns as text
env_data <- file_list %>%
  set_names(nm = basename(.)) %>%
  map_dfr(
    ~ read_excel(.x, col_types = "text") %>% 
      mutate(SourceFile = .x),
    .id = "FileName"
  )

# Extract site name from filename
env_data <- env_data %>%
  mutate(Site = sub("_.*", "", basename(FileName)))
saveRDS(env_data, file = "env_data.rds")

#Adding other datasets

folder_path2 <- "Shelters_experiment"

# 2. List all Excel files in the folder
file_list <- list.files(folder_path2, pattern = "\\.xlsx$", full.names = TRUE)

# Read all Excel files with all columns as text
shelter_data <- file_list %>%
  set_names(nm = basename(.)) %>%
  map_dfr(
    ~ read_excel(.x, col_types = "text") %>% 
      mutate(SourceFile = .x),
    .id = "FileName"
  )

# Extract site name from filename
shelter_data <- shelter_data %>%
  mutate(Site = sub("_.*", "", basename(FileName)))
saveRDS(shelter_data, file = "shelter_data.rds")

folder_path3 <- "Stations_installation"

# 2. List all Excel files in the folder
file_list <- list.files(folder_path3, pattern = "\\.xlsx$", full.names = TRUE)

# Read all Excel files with all columns as text
station_data <- file_list %>%
  set_names(nm = basename(.)) %>%
  map_dfr(
    ~ read_excel(.x, col_types = "text") %>% 
      mutate(SourceFile = .x),
    .id = "FileName"
  )

# Extract site name from filename
station_data <- station_data %>%
  mutate(Site = sub("_.*", "", basename(FileName)))
saveRDS(station_data, file = "station_data.rds")
