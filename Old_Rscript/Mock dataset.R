# Mock dataset creation in R: Seed removal + Environmental data

# Load required packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# Set seed for reproducibility
set.seed(42)

# Basic structure
stands <- paste0("Stand_", 1:8)
n_pairs <- 10
treatments <- c("PiceaBased", "FagusBased", "LarixBased")
weeks <- 1:3
access_types <- c("Open", "Restricted")

# Environmental cover categories to convert later
env_classes <- c("0-5%", "5-25%", "35-50%", "50-75%", "75-100%")

# Simulate environmental data per box pair
env_data <- expand.grid(
  Stand = stands,
  PairID = 1:n_pairs
) %>%
  mutate(
    PairID = paste(Stand, sprintf("Pair%02d", PairID), sep = "_"),
    VegCover = sample(env_classes, n(), replace = TRUE),
    BranchCover = sample(env_classes, n(), replace = TRUE),
    RockCover = sample(env_classes, n(), replace = TRUE),
    NurseLog = sample(env_classes, n(), replace = TRUE),
    HolesNearby = rpois(n(), lambda = 2),
    Canopy_N = sample(50:100, n(), replace = TRUE),
    Canopy_S = sample(50:100, n(), replace = TRUE),
    Canopy_E = sample(50:100, n(), replace = TRUE),
    Canopy_W = sample(50:100, n(), replace = TRUE),
    HidingDist_cm = sample(10:500, n(), replace = TRUE),
    HidingType = sample(c("Rock", "Branch"), n(), replace = TRUE)
  )

# Convert cover categories to mid-points
cover_levels <- c("0-5%" = 0.005, "5-25%" = 0.15, "35-50%" = 0.425, 
                  "50-75%" = 0.625, "75-100%" = 0.875)

env_data <- env_data %>%
  mutate(
    VegCover_pct = cover_levels[VegCover],
    BranchCover_pct = cover_levels[BranchCover],
    RockCover_pct = cover_levels[RockCover],
    NurseLog_pct = cover_levels[NurseLog],
    CanopyAvg = (Canopy_N + Canopy_S + Canopy_E + Canopy_W) / 4
  )

# Simulate seed removal data
removal_data <- expand.grid(
  Stand = stands,
  PairID = paste0("Pair", sprintf("%02d", 1:n_pairs)),
  Treatment = treatments,
  Access = access_types,
  Week = weeks
) %>%
  mutate(
    PairID = paste(Stand, PairID, sep = "_"),
    Seeds_Initial = 30,
    Seeds_Remaining = ifelse(Access == "Open", 
                             rpois(n(), lambda = 15), 
                             rpois(n(), lambda = 25))
  )

# Combine with environmental data
combined_data <- left_join(removal_data, env_data, by = c("Stand", "PairID"))

# Preview
head(combined_data)

# Boxplot of canopy cover by stand
ggplot(combined_data, aes(x = Stand, y = CanopyAvg)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Average Canopy Cover by Stand", y = "Canopy Cover (%)") +
  theme_minimal()

# Summary statistics of environmental conditions by stand
combined_data %>%
  group_by(Stand) %>%
  summarise(
    mean_canopy = mean(CanopyAvg),
    sd_canopy = sd(CanopyAvg),
    mean_hiding = mean(HidingDist_cm),
    mean_veg = mean(VegCover_pct),
    mean_rock = mean(RockCover_pct)
  )

#Does seed removal changes across stands?
# 1. ANOVAs: Do environmental variables differ across stands?
summary(aov(CanopyAvg ~ Stand, data = combined_data))
summary(aov(VegCover_pct ~ Stand, data = combined_data))
summary(aov(RockCover_pct ~ Stand, data = combined_data))
summary(aov(HidingDist_cm ~ Stand, data = combined_data))

combined_data %>%
  mutate(RemovalRate = (Seeds_Initial - Seeds_Remaining) / Seeds_Initial) %>%
  ggplot(aes(x = Stand, y = RemovalRate, fill = Access)) +
  geom_boxplot() +
  facet_wrap(~Treatment) +
  labs(title = "Seed Removal by Stand and Access", y = "Removal Proportion") +
  theme_minimal()

library(lme4)
combined_data <- combined_data %>%
  mutate(RemovalRate = (Seeds_Initial - Seeds_Remaining) / Seeds_Initial)

model1 <- lmer(RemovalRate ~ Access * Treatment + CanopyAvg + VegCover_pct +
                 RockCover_pct + HidingDist_cm + (1|Stand/PairID), data = combined_data)

summary(model1)

# Questions: yes 
# Is open access associated with higher removal?
# Are removal rates higher in certain forest types (stands)?
# Do vegetation cover or hiding spot distance correlate with removal?
# Is there an Access × Treatment interaction?


#PREDATOR DATA
# Simulate predator visit data (e.g., from camera traps)
set.seed(100)
predator_data <- env_data %>%
  select(Stand, PairID) %>%
  mutate(
    PredatorVisits = rpois(n(), lambda = 5),
    PredatorSpecies = sample(c("Mouse", "Squirrel", "Bird", "Unknown"), n(), replace = TRUE)
  )
combined_data <- left_join(combined_data, predator_data, by = c("Stand", "PairID"))
model2 <- lmer(RemovalRate ~ Access * Treatment + CanopyAvg + VegCover_pct +
                 RockCover_pct + HidingDist_cm + PredatorVisits +
                 (1|Stand/PairID), data = combined_data)
summary(model2)
#If interested in specific predator type
combined_data$PredatorSpecies <- as.factor(combined_data$PredatorSpecies)

model3 <- lmer(RemovalRate ~ Access * Treatment + CanopyAvg + VegCover_pct +
                 RockCover_pct + HidingDist_cm + PredatorSpecies +
                 (1|Stand/PairID), data = combined_data)
summary(model3)
AIC(model1, model2, model3)


# Community type
library(vegan)  # for diversity calculations

# Define species pool
predator_species <- c("Mouse", "Squirrel", "Bird", "Weasel", "Unknown")

# Simulate species counts per pair
predator_matrix <- matrix(
  rpois(length(env_data$PairID) * length(predator_species), lambda = 2),
  ncol = length(predator_species)
)
colnames(predator_matrix) <- predator_species

# Combine with environmental data
predator_df <- env_data %>%
  select(Stand, PairID) %>%
  bind_cols(as.data.frame(predator_matrix)) %>%
  rowwise() %>%
  mutate(
    PredatorRichness = sum(c_across(all_of(predator_species)) > 0),
    PredatorDiversity = diversity(c_across(all_of(predator_species)), index = "shannon")
  )

combined_data <- left_join(combined_data, predator_df, by = c("Stand", "PairID"))

model_rich <- lmer(RemovalRate ~ Access * Treatment + CanopyAvg + VegCover_pct +
                     RockCover_pct + HidingDist_cm + PredatorRichness +
                     (1|Stand/PairID), data = combined_data)

model_div <- lmer(RemovalRate ~ Access * Treatment + CanopyAvg + VegCover_pct +
                    RockCover_pct + HidingDist_cm + PredatorDiversity +
                    (1|Stand/PairID), data = combined_data)

summary(model_rich)
summary(model_div)


AIC(model1, model_rich, model_div)


ggplot(combined_data, aes(x = PredatorRichness, y = RemovalRate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Seed Removal vs. Predator Richness", y = "Removal Rate")

ggplot(combined_data, aes(x = PredatorDiversity, y = RemovalRate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Seed Removal vs. Predator Diversity", y = "Removal Rate")



# PCA ---------------------------------------------------------------------


#PCA of environmental conditions
# Select relevant environmental predictors
env_numeric <- env_data %>%
  select(PairID, Stand, CanopyAvg, VegCover_pct, RockCover_pct, HidingDist_cm)

# Scale them for PCA
env_scaled <- env_numeric %>%
  select(-PairID, -Stand) %>%
  scale()

pca_env <- prcomp(env_scaled, center = TRUE, scale. = TRUE)
summary(pca_env)  # View variance explained

# Get PC1 and PC2
pca_scores <- as.data.frame(pca_env$x[, 1:2])  # first 2 axes
colnames(pca_scores) <- c("PC1_env", "PC2_env")

# Add back IDs to match with seed data
pca_scores <- bind_cols(env_numeric %>% select(PairID, Stand), pca_scores)

# Merge with seed removal data
combined_data <- left_join(combined_data, pca_scores, by = c("Stand", "PairID"))

model_pca <- lmer(RemovalRate ~ Access * Treatment + PC1_env + PC2_env +
                    PredatorRichness + (1|Stand/PairID), data = combined_data)

summary(model_pca)

install.packages("ggbiplot")
library(ggbiplot)

ggbiplot(pca_env, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE) +
  theme_minimal() +
  ggtitle("PCA of Environmental Conditions")



# Seed preferneces --------------------------------------------------------


library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)

set.seed(123)

# Site species table, as before
site_species <- tibble(
  Site = c("Beatenberg", "Bettlachstock", "Celerina", "NationalPark",
           "Neunkirch", "Schanis", "Vordemwald", "Waldlabor"),
  Sp1 = c("Abies_alba", "Fagus_sylvatica", "Pinus_cembra", "Pinus_mugo",
          "Fagus_sylvatica", "Fagus_sylvatica", "Fagus_sylvatica", "Fagus_sylvatica"),
  Sp2 = c("Picea_abies", "Abies_alba", "Larix_decidua", NA,
          "Tilia_platyphyllos", "Abies_alba", "Abies_alba", "Abies_alba"),
  Sp3 = c(NA, "Picea_abies", NA, NA,
          "Acer_pseudoplatanus", "Acer_pseudoplatanus", "Picea_abies", "Picea_abies")
)

weeks <- 1:3
access_types <- c("Open", "Restricted")

# Convert to long format and drop NAs
site_species_long <- site_species %>%
  pivot_longer(cols = Sp1:Sp3, values_to = "SeedSpecies", values_drop_na = TRUE) %>%
  # Assign dominance rank based on position (Sp1=1, Sp2=2, Sp3=3)
  mutate(DominanceRank = case_when(
    name == "Sp1" ~ 1,
    name == "Sp2" ~ 2,
    name == "Sp3" ~ 3
  )) %>%
  select(-name)

# Seed amount per species (constant, e.g. 1g equivalent seeds = 30 seeds per box)
seeds_per_species <- 30

# Simulate data
sim_data <- site_species_long %>%
  crossing(Week = weeks, Access = access_types) %>%
  mutate(
    SeedsStart = seeds_per_species,
    # Removal probability increases with dominance rank (lower number = more dominant)
    RemovalProb = case_when(
      Access == "Open" ~ 0.3 + 0.1 * (4 - DominanceRank) + rnorm(n(), 0, 0.05),
      Access == "Restricted" ~ 0.1 + 0.05 * (4 - DominanceRank) + rnorm(n(), 0, 0.03)
    ),
    RemovalProb = pmin(pmax(RemovalProb, 0), 1),
    SeedsRemoved = rbinom(n(), SeedsStart, RemovalProb),
    SeedsRemaining = SeedsStart - SeedsRemoved,
    RemovalRate = SeedsRemoved / SeedsStart
  )

# Preview
head(sim_data)

# Plot example
ggplot(sim_data, aes(x = factor(DominanceRank), y = RemovalRate, fill = Access)) +
  geom_boxplot() +
  facet_wrap(~Site) +
  labs(x = "Dominance Rank (1=most dominant)", y = "Removal Rate",
       title = "Simulated Seed Removal by Dominance Rank, Access, and Site") +
  theme_minimal()

# Example model
model_sim <- lmer(RemovalRate ~ factor(DominanceRank) + Access + (1|Site), data = sim_data)
summary(model_sim)

