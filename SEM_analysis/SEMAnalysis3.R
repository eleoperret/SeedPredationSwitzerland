
library(dplyr)


data<-readRDS("Data/data_cleaned_seeds.rds")
camera<-readRDS("Data/CameraData_clean.RDS")
environement<-readRDS("Data/env_data.RDS")
str(camera)


str(data)

#Only using the data from the open access boxes
seedBoxplot <- data %>%
  group_by(Site, Species, TreatmentNr) %>%
  summarise(
    Total_Seeds = sum(NbSeedStart, na.rm = TRUE),
    Seeds_Eaten = sum(NbSeedStart - SeedsCount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Seeds_Eaten = pmax(Seeds_Eaten, 0),        # replace negative values with 0
    Removal_Prop = Seeds_Eaten / Total_Seeds
  ) 
str(seedBoxplot)

data<-data%>%
  filter(TreatmentNr==1)%>%
  filter(!Site%in%c("NAT","CEL"))

seed <- data %>%
  group_by(Site,Station, Species, TreatmentNr) %>%
  summarise(
    Total_Seeds = sum(NbSeedStart, na.rm = TRUE),
    Seeds_Eaten = sum(NbSeedStart - SeedsCount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Seeds_Eaten = pmax(Seeds_Eaten, 0),        # replace negative values with 0
    Removal_Prop = Seeds_Eaten / Total_Seeds
  )  

seed2 <- data %>%
  group_by(Site) %>%
  summarise(
    Total_Seeds = sum(NbSeedStart, na.rm = TRUE),
    Seeds_Eaten = sum(NbSeedStart - SeedsCount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Seeds_Eaten = pmax(Seeds_Eaten, 0),        # replace negative values with 0
    Removal_Prop = Seeds_Eaten / Total_Seeds
  )  

seed3 <- data %>%
  group_by(Site,Station) %>%
  summarise(
    Total_Seeds = sum(NbSeedStart, na.rm = TRUE),
    Seeds_Eaten = sum(NbSeedStart - SeedsCount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Seeds_Eaten = pmax(Seeds_Eaten, 0),        # replace negative values with 0
    Removal_Prop = Seeds_Eaten / Total_Seeds
  )  


#Predator data
predators <- camera %>%
  filter(!Species %in% c("Vehicles/Humans/Livestock"))%>%
  filter(treatment=="FULL")%>%
  rename(Station = `Site Name`) %>%
  rename(Site= "site")%>%
  filter(!Site%in%c("NAT","CEL"))  %>%
  mutate(Station = str_extract(Station, "\\d+"),
         Station = as.character(Station)
  )
  

predator_diversity <- predators %>%
  group_by(Site, Station) %>%
  summarise(Predator_richness = n_distinct(Species))

predators <- predators %>%
  mutate(Date = as.Date(Timestamp))

predator_daily <- predators %>%
  group_by(Site, Station, Date, Species) %>%
  summarise(Daily_visits = sum(`Sighting Count`), .groups = "drop")

predator_activity <- predator_daily %>%
  group_by(Site, Station, Species) %>%
  summarise(
    Total_visits = sum(Daily_visits),
    Days_monitored = 16, 
    Predator_visits = Total_visits/Days_monitored, # average visits per day
  )

predator_activity2 <- predator_daily %>%
  group_by(Site) %>%
  summarise(
    Total_visits = sum(Daily_visits),
    Days_monitored = 16, 
    Predator_visits = Total_visits/Days_monitored, 
    Predator_richness = n_distinct(Species)
  )

predator_activity3 <- predator_daily %>%
  group_by(Site, Station) %>%
  summarise(
    Total_visits = sum(Daily_visits),
    Days_monitored = 16, 
    Predator_visits = Total_visits/Days_monitored, 
    Predator_richness = n_distinct(Species)
  )

#Environemental data
str(environement)
environement <- environement %>%
  rename(Station= "Station_number")%>%
  filter(!Site%in%c("NAT","CEL"))%>%
  mutate(
    Veg_cover_mid = case_when(
      `Veg_cover_%` == "Trace" ~ 0,
      `Veg_cover_%` == "0"     ~ 0,
      `Veg_cover_%` == "1"     ~ 2.5,
      `Veg_cover_%` == "2"     ~ 15,
      `Veg_cover_%` == "3"     ~ 37.5,
      `Veg_cover_%` == "4"     ~ 62.5,
      `Veg_cover_%` == "5"     ~ 87.5,
      TRUE                     ~ NA_real_
    ),
    Canopy_cover = as.numeric(Canopy_cover) 
  )

env_data <- environement %>%
  group_by(Site, Station) %>%
  summarise(
    Veg_cover = mean(Veg_cover_mid, na.rm = TRUE),
    Canopy_cover = mean(Canopy_cover, na.rm = TRUE)
  ) %>%
  ungroup()

env_data2 <- environement %>%
  group_by(Site) %>%
  summarise(
    Veg_cover = mean(Veg_cover_mid, na.rm = TRUE),
    Canopy_cover = mean(Canopy_cover, na.rm = TRUE)
  ) %>%
  ungroup()



#Merging based on question
#Comparing sites?
str(seed2)
str(predator_activity2)
str(env_data2)

# Make site names consistent
predator_activity2 <- predator_activity2 %>%
  rename(Site = site)

# Merge seed + predator + environment data
sem_site <- seed2 %>%
  left_join(predator_activity2, by = "Site") %>%
  left_join(env_data2, by = "Site")%>%
sem_site_scaled <- sem_site %>%
  mutate(
    Canopy_cover = scale(Canopy_cover),
    Veg_cover = scale(Veg_cover),
    Predator_visits = scale(Predator_visits),
    Predator_richness = scale(Predator_richness),
    Removal = scale(Removal_Prop)
  )





# PerStationSite ----------------------------------------------------------


# Merge seed + predator + environment data
sem_station <- seed3 %>%
  left_join(predator_activity3, by = c("Site", "Station")) %>%
  left_join(env_data, by = c("Site", "Station")) %>%
  filter(!Site %in% c("CEL", "NAT"))

sem_station_scaled <- sem_station %>%
  mutate(
    Canopy_cover = scale(Canopy_cover),
    Veg_cover = scale(Veg_cover),
    Predator_visits = scale(Predator_visits),
    Predator_richness = scale(Predator_richness),
    Removal = scale(Removal_Prop)
  )

#SEM Model for sites
sem_model <- '
  # Latent variables
  Habitat =~ Canopy_cover + Veg_cover
  Predator_pressure =~ Predator_visits + Predator_richness

  # Regressions
  Removal_Prop ~ Habitat + Predator_pressure
  Predator_pressure ~ Habitat  # Predator affected by habitat

  # Covariance (if desired)
  # Habitat ~~ Predator_pressure
'

fit_sem <- sem(sem_model, data = sem_station_scaled, estimator = "MLR")  # robust estimator
summary(fit_sem, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

#Simpler model
sem_model2 <- '
  Predator_pressure =~ Predator_visits + Predator_richness
  Removal ~ Predator_pressure + Canopy_cover + Veg_cover
'
fit_sem2 <- sem(sem_model2, data = sem_station_scaled, estimator = "MLR")
summary(fit_sem2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

sem_model_habitat <- '
  # Latent variable
  Predator_pressure =~ 1*Predator_visits + Predator_richness

  # Structural regressions
  Predator_pressure ~ Canopy_cover + Veg_cover
  Removal ~ Predator_pressure
'
fit_sem_habitat <- sem(sem_model_habitat, data = sem_station_scaled, estimator = "MLR")
summary(fit_sem_habitat, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


sem_model_habitat_partial <- '
  # Latent variable
  Predator_pressure =~ 1*Predator_visits + Predator_richness

  # Structural regressions
  Predator_pressure ~ Canopy_cover + Veg_cover
  Removal ~ Predator_pressure + Canopy_cover + Veg_cover
'
fit_sem_habitat_partial <- sem(sem_model_habitat_partial, data = sem_station_scaled, estimator = "MLR")
summary(fit_sem_habitat_partial, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


library(DiagrammeR)

grViz("
digraph SEM {
  
  # Node definitions
  node [shape=box, style=filled, color=lightblue, fontname=Helvetica] 
    Canopy [label='Canopy_cover']
    Veg [label='Veg_cover']
    Removal [label='Removal']

  node [shape=ellipse, style=filled, color=green, fontname=Helvetica] 
    Predator [label='Predator_pressure']

  # Edges with standardized estimates (example from your model)
  Canopy -> Predator [label='-0.37']
  Veg -> Predator [label='0.07']
  Predator -> Removal [label='0.56']
}
")


grViz("
digraph SEM {
  
  # Nodes
  node [shape=box, style=filled, color=lightblue] 
    Canopy [label='Canopy_cover']
    Veg [label='Veg_cover']
    Removal [label='Removal']

  node [shape=ellipse, style=filled, color=green] 
    Predator [label='Predator_pressure']

  # Significant paths
  Predator -> Removal [label='0.56', color=black]

  # Tested but non-significant paths
  Canopy -> Removal [label='0.035', style=dashed, color=grey]
  Veg -> Removal [label='-0.047', style=dashed, color=grey]
  Canopy -> Predator [label='-0.372', style=dashed, color=grey]
  Veg -> Predator [label='0.07', style=dashed, color=grey]
}
")



# -------------------------------
# SEM Assumption Checks - Station Data
# -------------------------------

library(dplyr)
library(ggplot2)
library(GGally)
library(tidyr)

# Select variables of interest
sem_vars <- sem_station_scaled %>%
  select(Removal_Prop, Canopy_cover, Veg_cover, Predator_visits, Predator_richness)

# 1. Data type check
str(sem_vars)

# 2. Univariate normality check (Shapiro-Wilk test)
normality_results <- apply(sem_vars, 2, shapiro.test)
normality_results

# 2b. Visual inspection of distributions
sem_vars_long <- sem_vars %>%
  pivot_longer(cols = everything())
ggplot(sem_vars_long, aes(x = value)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  labs(title = "Distributions of SEM variables")

sem_vars_clean <- sem_vars %>% mutate(across(everything(), ~as.numeric(.x)))
GGally::ggpairs(sem_vars_clean)

# 4. Homoscedasticity check - residuals
# Fit a linear model as a proxy
lm1 <- lm(Removal_Prop ~ Canopy_cover + Veg_cover + Predator_visits + Predator_richness, data = sem_station_scaled)
# Plot residuals vs fitted values
plot(lm1, which = 1, main = "Residuals vs Fitted (Removal_Prop)")

# 5. Sample size info
n_obs <- nrow(sem_station_scaled)
n_params <- 10  # approximate number of parameters in your SEM
cat("Number of observations:", n_obs, "\n")
cat("Approximate number of SEM parameters:", n_params, "\n")
cat("Observations per parameter:", round(n_obs / n_params, 2), "\n")

# 6. Outlier detection
par(mfrow=c(2,3))
boxplot(sem_vars$Removal_Prop, main = "Removal_Prop")
boxplot(sem_vars$Canopy_cover, main = "Canopy_cover")
boxplot(sem_vars$Veg_cover, main = "Veg_cover")
boxplot(sem_vars$Predator_visits, main = "Predator_visits")
boxplot(sem_vars$Predator_richness, main = "Predator_richness")
par(mfrow=c(1,1))

# Optional: Winsorize extreme values if needed
# library(DescTools)
# sem_vars <- sem_vars %>%
#   mutate(across(everything(), ~Winsorize(.x, probs = c(0.01, 0.99))))

# -------------------------------
# End of SEM Assumption Checks
# -------------------------------


# SEM diagram
grViz("
digraph SEM {
  graph [rankdir=TB, fontsize=12]

  # Latent variable
  Predator_pressure [shape=oval, label='Predator_pressure']

  # Observed variables
  Canopy_cover [shape=box, label='Canopy_cover']
  Veg_cover [shape=box, label='Veg_cover']
  Predator_visits [shape=box, label='Predator_visits']
  Predator_richness [shape=box, label='Predator_richness']
  Removal_Prop [shape=box, label='Removal_Prop']

  # Paths to latent variable
  Predator_pressure -> Predator_visits [label='1.04', fontcolor=blue]
  Predator_pressure -> Predator_richness [label='0.61', fontcolor=blue]

  # Structural paths
  Canopy_cover -> Predator_pressure [label='-0.36', fontcolor=red]
  Veg_cover -> Predator_pressure [label='0.07', style=dashed]
  Predator_pressure -> Removal_Prop [label='0.58', fontcolor=blue]
  Canopy_cover -> Removal_Prop [label='0.035', style=dashed]
  Veg_cover -> Removal_Prop [label='-0.047', style=dashed]

  # R-squared
  Removal_Prop [xlabel='R²=0.336']
  Predator_pressure [xlabel='R²=0.117']

  # Graph aesthetics
  node [fontname=Helvetica]
  edge [fontsize=10]
}
")




# PerSeedSpecies ----------------------------------------------------------

#Comparing for site and multigroup species
sem_data_species <- seed %>%
  left_join(predator_activity3, by = c("Site", "Station")) %>%
  left_join(env_data, by = c("Site", "Station")) %>%
  select(Site, Species, Total_Seeds, Seeds_Eaten, Removal_Prop,
         Total_visits, Days_monitored, Predator_visits, Predator_richness,
         Veg_cover, Canopy_cover)

sem_data_species_scaled <- sem_data_species %>%
  mutate(across(c(Removal_Prop, Canopy_cover, Veg_cover, Predator_visits, Predator_richness),
                ~scale(.)[,1]))

# Example SEM model (based on your previous best-fitting model)
sem_model_species <- '
  # Latent variable for predator pressure
  Predator_pressure =~ 1*Predator_visits + Predator_richness

  # Structural regressions
  Predator_pressure ~ Canopy_cover + Veg_cover
  Removal_Prop ~ Predator_pressure + Canopy_cover + Veg_cover
'

# Fit multi-group SEM by Species
fit_sem_species <- sem(sem_model_species, 
                       data = sem_data_species_scaled,
                       group = "Species",  # key for multi-group
                       estimator = "MLR") # robust estimator

# Summarize results
summary(fit_sem_species, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


#Work with less slicing







