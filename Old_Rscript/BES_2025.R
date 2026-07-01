library(readxl)   # to read Excel files
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)


# List of filenames
env_files <- list.files("Data/Environmental_data", full.names = TRUE)

# Function to read one Excel file and add a Site column
read_env_file <- function(file) {
  # Extract site code from filename (e.g. BEA)
  site_code <- tools::file_path_sans_ext(basename(file)) %>%
    substr(1,3)
  
  read_excel(file) %>%
    mutate(Site = site_code)
}

# Read and combine all environmental data files
env_data <- lapply(env_files, read_env_file) %>%
  bind_rows()

# View first rows
head(env_data)


read_env_file <- function(file) {
  site_code <- tools::file_path_sans_ext(basename(file)) %>% substr(1, 3)
  
  read_excel(file, col_types = "text") %>%   # all columns as text
    mutate(Site = site_code)
}

env_data <- lapply(env_files, read_env_file) %>% bind_rows()

# Now convert numeric columns (like Veg_cover_%) explicitly:
env_data <- env_data %>%
  mutate(
    `Veg_cover_%` = as.numeric(`Veg_cover_%`),
    `Rock_cover_%` = as.numeric(`Rock_cover_%`),
    # ... add other columns you expect to be numeric
  )


head(env_data)

env_data_clean <- env_data %>% 
  rename(
    Canopy_N = `...12`,
    Canopy_S = `...13`,
    Canopy_E = `...14`
    # If you have a 4th direction, add here (e.g., Canopy_W)
  ) %>%
  mutate(
    `Veg_cover_%` = as.numeric(`Veg_cover_%`),
    `Branches_%` = as.numeric(`Branches_%`),
    `Rocks_%` = as.numeric(`Rocks_%`),
    `Nurselog_%` = as.numeric(`Nurselog_%`),
    Holes_nr_2m = as.numeric(Holes_nr_2m),
    Canopy_cover = as.numeric(Canopy_cover),
    Canopy_N = as.numeric(Canopy_N),
    Canopy_S = as.numeric(Canopy_S),
    Canopy_E = as.numeric(Canopy_E)
    # Convert Canopy_W here if exists
  ) %>%
  mutate(
    Canopy_Avg = rowMeans(select(., starts_with("Canopy_")), na.rm = TRUE)
  )


seed_data <- read_excel("Data/FinalCorrected_review_EP.xlsx")

head(seed_data)


combined_data <- seed_data %>%
  left_join(env_data, by = c("Site", ""))



# Example: plot seed removal by site and species
ggplot(combined_data, aes(x = SeedSpecies, y = RemovalRate, fill = Access)) +
  geom_boxplot() +
  facet_wrap(~Site) +
  theme_minimal()

# Example model (adjust column names to match your data)
model <- lmer(RemovalRate ~ SeedSpecies + Access + SomeEnvironmentalVariable + (1|Site), data = combined_data)
summary(model)

