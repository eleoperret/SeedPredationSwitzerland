


#TO DO.
# Check for the mistakes in the seed count
# Add the Bettlachstock data
# Add the seed data from the datasheet (mistakes and found seed)


install.packages("readxl")  
install.packages("dplyr")
install.packages("ggplot2")
library(readxl)
library(dplyr)
library(ggplot2)


getwd()
setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_CH/Seed_predation_CH_analysis/Data")
setwd("C:/Users/eperret/Desktop/Seed_predation_CH_analysis/Data")
list.files()


data<- read_excel( "FinalCorrected_review_EP.xlsx")

str(data)

data <- data %>%
  mutate(
    Week = as.character(Week),
    Station = as.character(Station),
    TreatmentNr = as.character(TreatmentNr),
    SpeciesTreatmentNr = as.character(SpeciesTreatmentNr)
  )

# Summarize the number of unique weeks and stations per site
site_summary <- data %>%
  group_by(Site) %>%
  summarize(
    unique_weeks = n_distinct(Week),
    unique_stations = n_distinct(Station),
    unique_treatment= n_distinct(TreatmentNr)
  )

site_summary_2 <- data %>%
  group_by(Site, Week) %>%
  summarize(
    unique_weeks = n_distinct(Week),
    unique_stations = n_distinct(Station),
    unique_treatment= n_distinct(TreatmentNr)
  )


# Ensure the key columns are numeric
data$NbSeedStart <- as.numeric(data$NbSeedStart)
data$SeedsCount <- as.numeric(data$SeedsCount)


str(data)

data <- data %>%
  mutate(SeedsLeft= NbSeedStart-SeedsCount)

negative_seed_rows <- data %>%
  filter(SeedsLeft < 0)

data_cleaned<-data %>%
  mutate(SeedsLeft = if_else(SeedsLeft < 0, 0, SeedsLeft))

data_cleaned<-data_cleaned %>%
  mutate(Removal = SeedsLeft/NbSeedStart)

saveRDS(data_cleaned, file = "data_cleaned_seeds.rds")

# Overall removal rates ---------------------------------------------------
# Compute overall removal rate
data<-data_seeds
total_seeds_start <- sum(data$NbSeedStart, na.rm = TRUE)
total_seeds_left <- sum(data$SeedsCount, na.rm = TRUE)
total_seeds_removed<-total_seeds_start-total_seeds_left

total_removal<-(total_seeds_start-total_seeds_left)/total_seeds_start
percent<-total_seeds_removed*100/total_seeds_start

#Week
# Compute total seeds started and total seeds left per week
weekly_removal <- data %>%
  group_by(Week) %>%
  summarise(
    total_seeds_start_week = sum(NbSeedStart, na.rm = TRUE),
    total_seeds_left_week = sum(SeedsCount, na.rm = TRUE)
  )
# Calculate the removal rate for each week
weekly_removal <- weekly_removal %>%
  mutate(RemovalRateWeek = 1- ((total_seeds_left_week) / total_seeds_start_week))
boxplot(RemovalRateWeek~Week, data= weekly_removal)

#Week/Site
week_removal_site <- data %>%
  group_by(Site, Week) %>%
  summarise(
    total_seeds_start_site = sum(NbSeedStart, na.rm = TRUE),
    total_seeds_left_site = sum(SeedsCount, na.rm = TRUE)
  )
# Calculate the removal rate for each week
week_removal_site <- week_removal_site %>%
  mutate(RemovalRateWeek = 1- ((total_seeds_left_site) / total_seeds_start_site))
ggplot(week_removal_site, aes(x = Week, y = RemovalRateWeek, color = Site, group = Site)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  labs(
    title = "Weekly Seed Removal Rate by Site",
    x = "Week",
    y = "Removal Rate (1 - Proportion of Seeds Remaining)",
    color = "Site"
  ) +
  theme_minimal(base_size = 14)

#Per site 
site_removal <- data %>%
  group_by(Site) %>%
  summarise(
    total_seeds_start_site = sum(NbSeedStart, na.rm = TRUE),
    total_seeds_left_site = sum(SeedsCount, na.rm = TRUE)
  )
site_removal <- site_removal %>%
  mutate(RemovalRateSite = 1-((total_seeds_left_site / total_seeds_start_site)))
boxplot(RemovalRateSite~Site, data= site_removal)
         

#Per species
species_removal <- data %>%
  group_by(Species) %>%
  summarise(
    total_seeds_start_Species = sum(NbSeedStart, na.rm = TRUE),
    total_seeds_left_Species = sum(SeedsCount, na.rm = TRUE)
  )
species_removal <- species_removal %>%
  mutate(RemovalRateSpecies = 100-((total_seeds_left_Species / total_seeds_start_Species)*100))
boxplot(RemovalRateSpecies~Species, data= species_removal)


# Per species and site
species_site_removal <- data %>%
  group_by(Species, Site) %>%
  summarise(
    total_seeds_start_SpeciesSite = sum(NbSeedStart, na.rm = TRUE),
    total_seeds_left_SpeciesSite = sum(SeedsCount, na.rm = TRUE) 
  )
species_site_removal<-species_site_removal%>%
  mutate(RemovalRateSpeciesSite = 1- ((total_seeds_left_SpeciesSite / total_seeds_start_SpeciesSite)))

# For average removal rate calculation (based on species and site)
species_site_removal_avg <- species_site_removal %>%
  mutate(RemovalDiff = total_seeds_start_SpeciesSite - total_seeds_left_SpeciesSite) %>%
  mutate(RemovalRateSpeciesSiteAvg = RemovalDiff / total_seeds_start_SpeciesSite)

# # ggplot to visualize the Removal Rate by Site/Species
# ggplot(species_site_removal, aes(x = interaction(Species, Site), y = RemovalRateSpeciesSite)) +
#   geom_boxplot() +
#   labs(title = "Seed Removal Rate by Species and Site", 
#        x = "Species and Site Combination", y = "Average Removal Rate") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# # ggplot to visualize the Removal Rate by Species/Site
# ggplot(species_site_removal, aes(x = interaction(Site, Species), y = RemovalRateSpeciesSite)) +
#   geom_boxplot() +
#   labs(title = "Seed Removal Rate by Species and Site", 
#        x = "Species and Site Combination", y = "Average Removal Rate") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ggplot to visualize the Removal Rate by Species, with faceting by Site
ggplot(species_site_removal, aes(x = Species, y = RemovalRateSpeciesSite)) +
  geom_boxplot() +
  labs(title = "Seed Removal Rate by Species and Site", 
       x = "Species", y = "Average Removal Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        panel.background = element_rect(fill = "white", color = "gray"),  # White background with gray border
        plot.background = element_rect(fill = "#f0f0f0"),  # Light gray background for the whole plot
        panel.grid.major = element_line(color = "gray", size = 0.5),  # Gray grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.title = element_text(face = "bold", size = 12),  # Bold and larger axis titles
        axis.text = element_text(size = 10),  # Larger axis text
        strip.text = element_text(size = 12, face = "bold"),  # Bold and larger facet labels
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +  # Centered and bold title
  scale_fill_brewer(palette = "Set2") +  # Improved color palette (Set2)
  facet_wrap(~ Site, scales = "free_y")  # Facet by Site

ggplot(species_site_removal, aes(x = Site, y = RemovalRateSpeciesSite)) +
  geom_boxplot() +
  labs(title = "Seed Removal Rate by Species and Site", 
       x = "Site", y = "Average Removal Rate") +
  facet_wrap(~ Species) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#For poster
ggplot(species_site_removal, aes(x = Site, y = RemovalRateSpeciesSite, fill = Site)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8) +
  geom_jitter(width = 0.2, size = 1, alpha = 0.5, color = "black") +
  facet_wrap(~ Species, nrow = 2) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Seed Removal by Site and Species",
    x = "Site",
    y = "Average Seed Removal Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 14)
  )
ggplot(species_site_removal, aes(x = Species, y = RemovalRateSpeciesSite, fill = Species)) +
  geom_col(position = position_dodge(), width = 0.7, alpha = 0.9) +
  facet_wrap(~ Site, nrow = 2) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Seed Removal by Site and Species",
    x = "Site",
    y = "Mean Seed Removal Rate ± SE"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 14)
  )
species_site_removal_sch<-species_site_removal%>%
  filter(Site=="SCH")
ggplot(species_site_removal_sch, aes(x = Species, y = RemovalRateSpeciesSite, fill = Species)) +
  geom_col(position = position_dodge(), width = 0.7, alpha = 0.9) +
  facet_wrap(~ Site, nrow = 2) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Seed Removal by Site and Species",
    x = "Site",
    y = "Mean Seed Removal Rate ± SE"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 14)
  )
ggplot(species_removal, aes(x = Species, y = RemovalRateSpecies, fill = Species)) +
  geom_col(position = position_dodge(), width = 0.7, alpha = 0.9) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black", size = 1) +  # Optional: show individual data points
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Seed Removal Rate by Species",
    x = "Species",
    y = "Seed Removal Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
ggplot(site_removal, aes(x = site, y = RemovalRateSpeciesSite, fill = site)) +
  geom_col(position = position_dodge(), width = 0.7, alpha = 0.9) +
  geom_jitter(width = 0.2, alpha = 0.4, color = "black", size = 1) +  # Optional: show individual data points
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Seed Removal Rate by Species",
    x = "Species",
    y = "Seed Removal Rate"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
