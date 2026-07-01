#Analysis

#To do : 
#Add the seeds that were found later
#check the percentage of mistakes to make an overall mistake percentage check

install.packages("FactoMineR")
install.packages("factoextra")
install.packages("DHARMa")
install.packages("ggeffects")
install.packages("broom.mixed")
library(dplyr)
library(tidyr)
library(FactoMineR)  # or prcomp (base R)
library(factoextra)  # for visualization
library(ggplot2)
library(tibble)
library(lme4)
library(DHARMa)
library(emmeans)
library(ggeffects)
library(purrr)
library(broom.mixed)

getwd()
setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_CH/Seed_predation_CH_analysis_Desktop/Data")

env_data <- readRDS("env_data.rds")
data_seeds <- readRDS("data_cleaned_seeds.rds")
shelter_data <- readRDS("shelter_data.rds")
station_data <- readRDS("station_data.rds")


str(env_data)
str(data_seeds)
str(shelter_data)
str(station_data)


# 1. Prepare Environmental Data ------------------------------------------------

# Rename columns for easier handling
env_data <- env_data %>%
  rename(
    Veg_cover = `Veg_cover_%`,
    Branches = `Branches_%`,
    Rocks = `Rocks_%`,
    Nurselog = `Nurselog_%`
  ) %>%
  mutate(across(c(`Canopy_cover`, `...12`, `...13`, `...14`), as.numeric)) %>%
  mutate(Canopy_Mean = rowMeans(select(., `...12`, `...13`, `...14`), na.rm = TRUE))

# Convert categorical cover scores to numeric percentages
convert_cover_score <- function(x) {
  case_when(
    x == "0"     ~ 0,
    x == "Trace" ~ 0.01,
    x == "1"     ~ 2.5,
    x == "2"     ~ 15,
    x == "3"     ~ 37.5,
    x == "4"     ~ 62.5,
    x == "5"     ~ 87.5,
    TRUE         ~ NA_real_
  )
}

env_data <- env_data %>%
  mutate(across(c(Veg_cover, Branches, Rocks, Nurselog),
                ~ convert_cover_score(as.character(.)))) %>%
  mutate(Holes_nr_2m = as.numeric(Holes_nr_2m))



# #Merging datasets -------------------------------------------------------

seed_env <- data_seeds %>%
  left_join(env_data, 
            by = c("Site" = "Site", "Station" = "Station_number"))


ggplot(seed_env, aes(x = Canopy_cover, y = Removal)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  facet_wrap(~ Species) +
  labs(title = "Seed removal vs canopy cover", x = "Canopy cover (%)") +
  theme_minimal()

# Summarise mean and confidence interval
summary_plot_data <- seed_env %>%
  group_by(Site, Species, TreatmentNr) %>%
  summarise(
    mean_removal = mean(Removal, na.rm = TRUE),
    sd_removal = sd(Removal, na.rm = TRUE),
    n = n(),
    se = sd_removal / sqrt(n),
    ci95 = 1.96 * se,
    .groups = "drop"
  )

# Plot with mean ± CI
ggplot(summary_plot_data, aes(x = Species, y = mean_removal, fill = TreatmentNr)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = mean_removal - ci95, ymax = mean_removal + ci95),
                width = 0.2, position = position_dodge(width = 0.8)) +
  facet_wrap(~ Site) +
  labs(title = "Mean seed removal by species and treatment (±95% CI)",
       y = "Mean proportion removed") +
  theme_minimal()

# 2. PCA on Environmental Variables -------------------------------------------

# Select environmental variables (drop rows with NAs)
env_vars <- env_data %>%
  select(Site, Station_number, Veg_cover, Branches, Rocks, Nurselog, Holes_nr_2m, Canopy_Mean) %>%
  drop_na()

# Scale environmental variables (except Site and Station_number)
env_scaled <- env_vars %>%
  select(-Site, -Station_number) %>%
  scale()

# Perform PCA
pca_result <- prcomp(env_scaled, center = TRUE, scale. = FALSE)

# Add PC1 scores back to env_vars
env_vars <- env_vars %>%
  mutate(PC1 = pca_result$x[,1])


# 3. Summarize Environmental Data per Station ---------------------------------

env_summary <- env_vars %>%
  group_by(Site, Station_number) %>%
  summarise(
    Veg_cover = mean(Veg_cover, na.rm = TRUE),
    Canopy_Mean = mean(Canopy_Mean, na.rm = TRUE),
    PC1 = mean(PC1, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(Station = as.character(Station_number)) %>%  # prepare for merging
  select(-Station_number)


# 4. Prepare Seed Data ---------------------------------------------------------

# Ensure keys are character type to match env_summary
data_seeds <- data_seeds %>%
  mutate(Site = as.character(Site),
         Station = as.character(Station))


# 5. Merge Environmental Summary with Seed Data -------------------------------

data_merged <- data_seeds %>%
  left_join(env_summary, by = c("Site", "Station"))

data_merged <- data_merged %>%
  mutate(SeedsNotRemoved = pmin(SeedsCount, NbSeedStart))

data_merged <- data_merged %>%
  mutate(SeedsRemoved = NbSeedStart - SeedsNotRemoved)

# Model number of seeds removed out of NbSeedStart (trials)
model_binom <- glmer(cbind(SeedsRemoved, SeedsNotRemoved) ~ TreatmentNr + Species + PC1^3 +(1 | Site/Station) + (1 | Week),
                     data = data_merged,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa"))

data_merged$ObsID <- factor(1:nrow(data_merged))
model_binom <- glmer(
  cbind(SeedsRemoved, SeedsNotRemoved) ~ TreatmentNr + Species + PC1^2 + (1 | Week) + (1 | ObsID),
  data = data_merged,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
)

# plot(model_binom)
# summary(model_binom)


# Simulate residuals
simulationOutput <- simulateResiduals(fittedModel = model_binom, n = 1000)

# Plot residual diagnostics
plot(simulationOutput)

# Test for overdispersion
testDispersion(simulationOutput)




# BEA MODEL-------------------------------------------------------------------------

data_BEA <- data_merged %>%
  filter(Site=="BEA")

# Model number of seeds removed out of NbSeedStart (trials)
Bea_model_binom <- glmer(cbind(SeedsRemoved, SeedsNotRemoved) ~ TreatmentNr + Species + PC1 + (1 | Week),
                     data = data_BEA,
                     family = binomial,
                     control = glmerControl(optimizer = "bobyqa"))
plot(Bea_model_binom)
summary(Bea_model_binom)


# Simulate residuals
simulationOutput <- simulateResiduals(fittedModel = Bea_model_binom, n = 1000)

# Plot residual diagnostics
plot(simulationOutput)

# Test for overdispersion
testDispersion(simulationOutput)

emmeans(Bea_model_binom,~Species)
emmeans(Bea_model_binom, pairwise ~ Species, type = "response")


mean_removal_per_species <- data_BEA %>%
  group_by(Species) %>%
  summarise(mean_removal = mean(Removal, na.rm = TRUE)) %>%
  arrange(desc(mean_removal))

ggplot(mean_removal_per_species, aes(x = Species, y = mean_removal)) +
  geom_boxplot(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Seed Removal by Species at Site BEA",
       x = "Species", y = "Seed Removal")+
  ylim(0, 1)  # fix y-axis from 0 to 1

# BET MODEL-------------------------------------------------------------------------

data_BET <- data_merged %>%
  filter(Site=="BET")

# Model number of seeds removed out of NbSeedStart (trials)
Bet_model_binom <- glmer(cbind(SeedsRemoved, SeedsNotRemoved) ~ TreatmentNr + Species + PC1 + (1 | Week),
                         data = data_BET,
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa"))
plot(Bet_model_binom)
summary(Bet_model_binom)


# Simulate residuals
simulationOutput <- simulateResiduals(fittedModel = Bet_model_binom, n = 1000)

# Plot residual diagnostics
plot(simulationOutput)

# Test for overdispersion
testDispersion(simulationOutput)

emmeans(Bet_model_binom,~Species)
emmeans(Bet_model_binom, pairwise ~ Species, type = "response")


mean_removal_per_species <- data_BET %>%
  group_by(Species) %>%
  summarise(mean_removal = mean(Removal, na.rm = TRUE)) %>%
  arrange(desc(mean_removal))

ggplot(mean_removal_per_species, aes(x = Species, y = mean_removal)) +
  geom_boxplot(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Seed Removal by Species at Site BEA",
       x = "Species", y = "Seed Removal")+
  ylim(0, 1)  # fix y-axis from 0 to 1

# WAL MODEL-------------------------------------------------------------------------

data_WAL <- data_merged %>%
  filter(Site=="WAL")

# Model number of seeds removed out of NbSeedStart (trials)
WAL_model_binom <- glmer(cbind(SeedsRemoved, SeedsNotRemoved) ~ TreatmentNr + Species + PC1 + (1 | Week),
                         data = data_WAL,
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa"))
plot(WAL_model_binom)
summary(WAL_model_binom)


# Simulate residuals
simulationOutput <- simulateResiduals(fittedModel = WAL_model_binom, n = 1000)

# Plot residual diagnostics
plot(simulationOutput)

# Test for overdispersion
testDispersion(simulationOutput)

emmeans(WAL_model_binom,~Species)
emmeans(WAL_model_binom, pairwise ~ Species, type = "response")


mean_removal_per_species <- data_WAL %>%
  group_by(Species) %>%
  summarise(mean_removal = mean(Removal, na.rm = TRUE)) %>%
  arrange(desc(mean_removal))

ggplot(mean_removal_per_species, aes(x = Species, y = mean_removal)) +
  geom_boxplot(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Seed Removal by Species at Site BEA",
       x = "Species", y = "Seed Removal")+
  ylim(0, 1)  # fix y-axis from 0 to 1

# VOR MODEL-------------------------------------------------------------------------

data_VOR <- data_merged %>%
  filter(Site=="VOR")

# Model number of seeds removed out of NbSeedStart (trials)
VOR_model_binom <- glmer(cbind(SeedsRemoved, SeedsNotRemoved) ~ TreatmentNr + Species + PC1 + (1 | Week),
                         data = data_VOR,
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa"))
plot(VOR_model_binom)
summary(VOR_model_binom)


# Simulate residuals
simulationOutput <- simulateResiduals(fittedModel = VOR_model_binom, n = 1000)

# Plot residual diagnostics
plot(simulationOutput)

# Test for overdispersion
testDispersion(simulationOutput)

emmeans(VOR_model_binom,~Species)
emmeans(VOR_model_binom, pairwise ~ Species, type = "response")


mean_removal_per_species <- data_VOR %>%
  group_by(Species) %>%
  summarise(mean_removal = mean(Removal, na.rm = TRUE)) %>%
  arrange(desc(mean_removal))

ggplot(mean_removal_per_species, aes(x = Species, y = mean_removal)) +
  geom_boxplot(fill = "steelblue") +
  theme_minimal() +
  labs(title = "Seed Removal by Species at Site BEA",
       x = "Species", y = "Seed Removal")+
  ylim(0, 1)  # fix y-axis from 0 to 1


# -------------------------------------------------------------------------



# 1. Split data by Site
data_by_site <- split(data_merged, data_merged$Site)

# 2. Function to check if site has >=2 species and >=2 treatments
is_site_eligible <- function(df) {
  n_species <- n_distinct(df$Species)
  n_treatment <- n_distinct(df$TreatmentNr)
  return(n_species >= 2 & n_treatment >= 2)
}

# 3. Function to run model safely (tryCatch) and return NULL if error
run_site_model <- function(df) {
  tryCatch(
    glmer(cbind(SeedsRemoved, SeedsNotRemoved) ~ TreatmentNr + Species + PC1 + (1 | Week) + (1 | Station),
          data = df,
          family = binomial,
          control = glmerControl(optimizer = "bobyqa")),
    error = function(e) NULL
  )
}

# 4. Filter eligible sites & run models
models_by_site <- data_by_site %>%
  keep(is_site_eligible) %>%    # keep only eligible sites
  map(run_site_model)            # run models

# 5. Remove NULL models (in case of any error)
models_by_site <- compact(models_by_site)

# 6. Extract fixed effects summaries (tidy)
model_summaries <- imap(models_by_site, ~ broom.mixed::tidy(.x, effects = "fixed") %>% mutate(Site = .y))

model_summaries_df <- bind_rows(model_summaries)

# 7. Run post-hoc pairwise contrasts by site for TreatmentNr (example)
posthoc_by_site <- imap(models_by_site, function(mod, site) {
  emmeans(mod, pairwise ~ Species, type = "response")$contrasts %>%
    as.data.frame() %>%
    mutate(Site = site)
})

posthoc_df <- bind_rows(posthoc_by_site)

# Now you have:
# - model_summaries_df with fixed effect estimates per site
# - posthoc_df with pairwise contrasts for TreatmentNr per site

# You can view results like this:
print(model_summaries_df)
print(posthoc_df)







# -------------------------------------------------------------------------
#To clean
#Analysis

#To do : 
#Add the seeds that were found later
#check the percentage of mistakes to make an overall mistake percentage check

install.packages("FactoMineR")
install.packages("factoextra")
library(dplyr)
library(tidyr)
library(FactoMineR)  # or prcomp (base R)
library(factoextra)  # for visualization
library(ggplot2)
library(tibble)

setwd()
env_data <- readRDS("env_data.rds")
data_seeds <- readRDS("data_cleaned_seeds.rds")
shelter_data <- readRDS("shelter_data.rds")
station_data <- readRDS("station_data.rds")


names(env_data)
names(station_data)
names(merged_data)
names(shelter_data)

env_data <- env_data %>%
  rename(
    Veg_cover = `Veg_cover_%`,
    Branches = `Branches_%`,
    Rocks = `Rocks_%`,
    Nurselog = `Nurselog_%`
  )


# Step 1: Convert columns to numeric and create mean canopy variable
env_data <- env_data %>%
  mutate(across(c(`Canopy_cover`, `...12`, `...13`, `...14`), 
                ~ as.numeric(.))) %>%
  mutate(Canopy_Mean = rowMeans(select(., `...12`, `...13`, `...14`), na.rm = TRUE))

str(env_data)


convert_cover_score <- function(x) {
  as.numeric(case_when(
    x == "0"     ~ 0,       # explicitly no cover
    x == "Trace" ~ 0.01,    # barely visible
    x == "1"     ~ 2.5,
    x == "2"     ~ 15,
    x == "3"     ~ 37.5,
    x == "4"     ~ 62.5,
    x == "5"     ~ 87.5,
    TRUE         ~ NA_real_
  ))
}

# Apply it: convert to character first
env_data <- env_data %>%
  mutate(across(c(Veg_cover, Branches, Rocks, Nurselog),
                ~ convert_cover_score(as.character(.)))) %>%
  mutate(`Holes_nr_2m` = as.numeric(`Holes_nr_2m`))



# Step 2: Select environmental variables for PCA
# Select relevant environmental variables for PCA
env_vars <- env_data %>%
  select(Site, Station_number, Veg_cover, Branches, Rocks, Nurselog, 
         Holes_nr_2m, Canopy_Mean) %>%
  drop_na()  # Remove rows with missing values (PCA can't handle them)

# Step 3: Scale the environmental variables (without Site and Station_number)
env_scaled <- env_vars %>%
  select(-Site, -Station_number) %>%
  scale()

# Step 4: Run PCA
pca_result <- prcomp(env_scaled, center = TRUE, scale. = FALSE)
summary(pca_result)

pca_result$rotation



# Plot
summary_table <- env_data %>%
  select(Site, Station_number, Veg_cover, Canopy_Mean, Notes) %>%
  arrange(Site, as.numeric(Station_number))  # Sort nicely by site and station number

print(summary_table)


ggplot(summary_table, aes(x = Canopy_Mean, y = Veg_cover)) +
  geom_point(aes(color = Site), size = 3) +
  geom_text(aes(label = ifelse(!is.na(Notes), Notes, "")), 
            hjust = 0, vjust = 1.2, size = 3, check_overlap = TRUE) +
  labs(x = "Canopy Mean (%)", y = "Vegetation Cover (%)",
       title = "Canopy vs Vegetation Cover with Field Notes") +
  theme_minimal()

plot(env_data$Canopy_Mean)
plot(env_data$Veg_cover)
plot(env_data$Canopy_Mean, env_data$Veg_cover,
     xlab = "Canopy Mean (%)", ylab = "Vegetation Cover (%)",
     main = "Relationship between Canopy Cover and Vegetation Cover")


biplot(pca_result, scale = 0)
fviz_pca_biplot(pca_result, repel = TRUE)

# Step 5: Add PCA scores (PC1 and PC2) back to the original env_data
# Keep only rows with complete cases matching env_vars (same order)
env_data_pca <- env_data %>%
  filter(complete.cases(env_vars)) %>%
  mutate(PC1 = pca_res$ind$coord[,1],
         PC2 = pca_res$ind$coord[,2])

# Now env_data_pca has your original data + PC1 and PC2 scores

site_summary <- env_data %>%
  group_by(Site) %>%
  summarise(
    mean_Veg_cover = mean(Veg_cover, na.rm = TRUE),
    sd_Veg_cover = sd(Veg_cover, na.rm = TRUE),
    mean_Canopy_Mean = mean(Canopy_Mean, na.rm = TRUE),
    sd_Canopy_Mean = sd(Canopy_Mean, na.rm = TRUE),
    mean_Rocks = mean(Rocks, na.rm = TRUE),
    sd_Rocks = sd(Rocks, na.rm = TRUE),
    mean_branch_cover = mean(Branches, na.rm = TRUE),
    sd_branch_cover = sd(Branches, na.rm = TRUE),
    mean_nurselog_cover = mean(Nurselog, na.rm = TRUE),
    sd_nurselog_cover = sd(Nurselog, na.rm = TRUE)
  ) %>%
  arrange(Site)

# Reshape your summary to long format
env_summary_long <- site_summary %>%
  pivot_longer(cols = -Site, names_to = "Variable", values_to = "Value")

# Plot
ggplot(env_summary_long, aes(x = Site, y = Value, fill = Variable)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Environmental Variables per Site", y = "Mean Value", x = "Site") +
  theme_minimal()

# Plot heatmap (raw values)
ggplot(env_summary_long, aes(x = Variable, y = Site, fill = Value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(title = "Environmental Variables per Site (Raw Values)",
       x = "Variable", y = "Site") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Step 1: Select and scale numeric environmental variables
env_index <- env_data %>%
  select(Site, Station_number, Veg_cover, 
         Canopy_Mean, Branches,
         Rocks, Nurselog ) %>%
  mutate(across(c(Veg_cover, Canopy_Mean, Branches, Rocks, Nurselog), scale)) %>%
  rowwise() %>%
  mutate(Environment_Index = mean(c_across(Veg_cover:Nurselog), na.rm = TRUE)) %>%
  ungroup()

env_index <- env_index %>%
  mutate(Environment_Type = case_when(
    Environment_Index >=  0.5 ~ "Dense",
    Environment_Index <= -0.5 ~ "Open",
    TRUE ~ "Intermediate"
  ))

ggplot(env_index %>% mutate(Station_number = factor(Station_number, levels = as.character(1:10))),
       aes(x = Station_number, y = Environment_Index, fill= Environment_Type)) +
  geom_col() + coord_flip() +
  facet_wrap(~ Site, scales = "free_y") +
  labs(title = "Environmental Index per Station, Faceted by Site", x = "Station Number", y = "Index Score") +
  scale_fill_manual(values = c(Dense = "darkgreen", Intermediate = "goldenrod", Open = "lightblue")) +
  theme_minimal()


library(stats)

# Select numeric environmental variables
env_vars <- env_data %>%
  select(Site, Station_number, Veg_cover = Veg_cover, Canopy = Canopy_Mean, Branches, Rocks, Nurselog)

# Scale the variables (mean=0, sd=1)
env_scaled <- env_vars %>%
  mutate(across(c(Veg_cover, Canopy, Branches, Rocks, Nurselog), scale))

# Run PCA (only on numeric columns)
pca_result <- prcomp(env_scaled %>% select(Veg_cover, Canopy, Branches, Rocks, Nurselog), center = FALSE, scale. = FALSE)

# Add PC1 scores to data (environmental index)
env_pca_index <- env_scaled %>%
  mutate(Env_Index_PC1 = pca_result$x[,1])


# Combine PC1 scores with Site and Station_number
env_pca_scores <- env_vars %>%
  select(Site, Station_number) %>%
  mutate(PC1 = pca_result$x[,1])

# Plot PC1 scores per station, faceted by Site
ggplot(env_pca_scores, aes(x = factor(Station_number, levels = as.character(sort(unique(Station_number)))), y = PC1)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  facet_wrap(~ Site, scales = "free_y") +
  labs(title = "PC1 Environmental Scores per Station", x = "Station Number", y = "PC1 Score") +
  theme_minimal()



env_means <- env_data %>%
  group_by(Site, Station_number) %>%
  summarise(
    Canopy_mean = mean(Canopy_Mean, na.rm = TRUE),
    Veg_cover_mean = mean(Veg_cover, na.rm = TRUE)
  ) %>%
  ungroup()


env_means_long <- env_means %>%
  pivot_longer(cols = c(Canopy_mean, Veg_cover_mean),
               names_to = "Variable",
               values_to = "Value")


ggplot(env_means_long, aes(x = factor(Station_number), y = Value)) +
  geom_col(fill = "steelblue") +
  facet_grid(Variable ~ Site, scales = "free_y") +
  labs(title = "Mean Canopy and Vegetation Cover per Station",
       x = "Station", y = "Mean % Cover") +
  theme_minimal()


ggplot(env_means_long, aes(x = factor(Station_number), y = Value, fill = Variable)) +
  geom_col(position = "dodge") +
  facet_wrap(~ Site) +
  labs(title = "Mean Canopy Openness and Vegetation Cover per Station",
       x = "Station", y = "Mean % Cover") +
  scale_fill_manual(values = c("Canopy_mean" = "lightblue", "Veg_cover_mean" = "darkgreen")) +
  theme_minimal()


str(data_seeds)


env_summary <- env_data %>%
  group_by(Site, Station_number) %>%
  summarise(
    Canopy_Mean = mean(Canopy_Mean, na.rm = TRUE),
    Veg_cover = mean(Veg_cover, na.rm = TRUE),
    PC1 = mean(PC1, na.rm = TRUE)  # if applicable
  )


data_merged <- data_seeds %>%
  left_join(env_means, by = c("Site", "Station"))

