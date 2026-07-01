#SEM
getwd()
setwd ("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_CH/Seed_predation_CH_analysis_Desktop")

library(dplyr)
library(vegan)


# Data_import -------------------------------------------------------------
data_seeds <- readRDS("Data/data_cleaned_seeds.rds")
data_camera<-readRDS("Data/CameraData_clean")


# DataCleaning ------------------------------------------------------------

#Cleaning camera data
data_camera <- data_camera %>%
  rename(Station = `Site Name`)
data_camera <- data_camera %>%
  mutate(Station = str_remove(Station, "Station\\s*"))
data_camera<-data_camera %>%
  filter(treatment!="EMPTY")

#Grouping and creating a sighting number
data_camera_sightings <- data_camera %>%
  group_by(site, Station, Species) %>%     # group by site, station name, and week
  summarise(Sightings_total = n(),      # count number of rows (sightings)
            .groups = "drop")
data_camera_sightings_2<-data_camera_sightings%>%
  filter(Species!= "Vehicles/Humans/Livestock")

head(data_camera_sightings_2)


# Option 1: Diversity calculation
data_wide <- data_camera_sightings_2 %>%
  tidyr::pivot_wider(names_from = Species,
                     values_from = Sightings_total,
                     values_fill = 0)

# Option 2: Shannon index per site and station
predator_index <- data_wide %>%
  rowwise() %>%
  mutate(
    Shannon = diversity(c_across(-c(site, Station)), index = "shannon"),
    Species_richness = sum(c_across(-c(site, Station)) > 0)
  ) %>%
  ungroup()
predator_index_clean<-predator_index%>%
  select(site, Station, Shannon, Species_richness)

head(predator_index)

data_merged <- data_seeds %>%
  left_join(predator_index_clean,
            by = c("Site" = "site", "Station"))

data_veg<-readRDS("Data/env_data.rds")

#Still need to add this year data 
data_mergedFULL<- data_merged %>%
  left_join(data_veg,
            by = c("Site" = "Site", "Station"= "Station_number"))

saveRDS(data_mergedFULL, file = "C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_CH/Seed_predation_CH_analysis_Desktop/Data/data_mergedFULL.rds")

str(data_mergedFULL)





#Not sure what happens after this. 





#SEM
library(lavaan)

data_mergedFULL <- data_mergedFULL %>%
  rename(
    Veg_cover = `Veg_cover_%`,
    Branches_cover = `Branches_%`,
    Rocks_cover = `Rocks_%`,
    Nurselog_cover = `Nurselog_%`
  )

sem_model <- '
  # Predator community depends on environment
  Shannon ~ Canopy_cover + Veg_cover + Site
  Species_richness ~ Canopy_cover + Veg_cover + Site

  # Seed removal depends on predators and treatment
  Removal ~ Shannon + Species_richness + TreatmentNr + Canopy_cover + Veg_cover + Site + Species
'

fit <- sem(sem_model, data = data_mergedFULL)
summary(fit, standardized = TRUE, fit.measures = TRUE)


dagify(y ~ x2 + x3,
       x2 ~ x1,
       x3 ~ x2) %>% 
  ggdag() 


library(ggdag)
sem_dag <- dagify(
  Shannon ~ Canopy_cover + Veg_cover + Site,
  Species_richness ~ Site + Veg_cover + Canopy_cover,
  Removal ~ Shannon + Species_richness + Canopy_cover + Veg_cover + TreatmentNr + Site + Species
)

# Plot it
ggdag(sem_dag, text = TRUE, text_size = 1.5) + 
  theme_dag()


#Second try
sem_model <- '
    # Seed removal depends on 
  Removal ~ Shannon + TreatmentNr + Canopy_cover + Veg_cover + Site + Species
  Canopy_cover~Site
  Veg_cover~Site + Canopy_cover
  Shannon~ Site
'
sem_dag <- dagify(
  Removal ~ Shannon + TreatmentNr + Canopy_cover + Veg_cover + Site + Species, 
  Canopy_cover~Site,
  Veg_cover~Site + Canopy_cover,
  Shannon~Site
)
ggdag(sem_dag, text = TRUE, text_size = 1.5) + 
  theme_dag()

fit <- sem(sem_model, data = data_mergedFULL)
summary(fit, standardized = TRUE, fit.measures = TRUE)


#Trying another method

library(dplyr)
library(lavaan)


#Exercise 4: Latent and Composite variable
sem_dag <- dagify(
  Removal ~ Shannon + TreatmentNr + Habitat_structure + Species,
  Shannon ~ Site + Habitat_structure,
  # Latent construct definition (for conceptual clarity)
  Canopy_cover ~ Habitat_structure,
  Veg_cover ~ Habitat_structure,
  latent = "Habitat_structure"
)
ggdag(sem_dag, text = TRUE, text_size = 1.5) + 
  theme_dag()

sem_dag_pred <- dagify(
  # Main causal relationships
  Removal ~ Predation_pressure + TreatmentNr + SeedSpecies + DominantTree,
  Predation_pressure ~ Shannon + Canopy_cover + Veg_cover)

ggdag(sem_dag_pred, text = TRUE, text_size = 1.5) + 
  theme_dag()

###STOPPED THERE : Categories for the veg cover to establish
#Problems with some of the variables
#Attribute to each seed specie a value
#Or do this 3 times for each species
#Seed removal between boundaries can be problematic but have to use the order dunction?
#REmove Site




sem_dag <- dagify(
  ## response
  Removal ~ Treatment + Distance_to_tree + Habitat_structure + Predator_visits + Predator_diversity + Species,
  
  ## predator behaviour (measured)
  Predator_visits ~ Habitat_structure + Treatment + Distance_to_tree + Camera_effort + Site,
  Predator_diversity ~ Habitat_structure + Site,
  
  ## habitat latent (reflective indicators)
  Habitat_structure ~ Tree_composition + Site,
  Canopy_cover ~ Habitat_structure,
  Veg_cover    ~ Habitat_structure,
  
  ## camera / site context
  Camera_effort ~ Site,
  Tree_composition ~ Site,
  
  ## declare latent
  latent = "Habitat_structure"
)

# quick layout / check
plot(sem_dag)

library(ggplot2)

ggdag(sem_dag, layout = "nicely") +
  theme_dag() +
  labs(title = "SEM conceptual DAG: Habitat vs Predator Behaviour Effects on Seed Removal")



library(ggdag)
library(ggplot2)
library(dplyr)

# Convert dagitty DAG to ggdag tibble
dag_df <- tidy_dagitty(sem_dag)

# Define variable types manually
node_types <- tibble(
  name = c("Habitat_structure", "Canopy_cover", "Veg_cover",
           "Predator_visits", "Predator_diversity", "Treatment",
           "Distance_to_tree", "Removal", "Species", "Tree_composition",
           "Camera_effort", "Site"),
  type = c("latent", "observed", "observed",
           "observed", "observed", "observed",
           "observed", "response", "observed", "observed",
           "observed", "observed")
)

# Join variable types to DAG
dag_df <- dag_df %>%
  left_join(node_types, by = "name") %>%
  mutate(
    shape = case_when(
      type == "latent" ~ "ellipse",
      type == "response" ~ "rect",
      TRUE ~ "rect"
    ),
    fill = case_when(
      type == "latent" ~ "#FFD580",      # light orange
      type == "response" ~ "#C3E6CB",    # pale green
      TRUE ~ "#D9EAF7"                   # light blue
    )
  )

# Plot nicely
ggplot(dag_df, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_node(aes(shape = shape, fill = fill), color = "black", size = 10) +
  geom_dag_text(color = "black", size = 3.5, family = "sans") +
  scale_shape_manual(values = c(rect = 22, ellipse = 21)) +
  scale_fill_identity() +
  theme_dag(base_size = 14) +
  labs(title = "SEM Conceptual Model: Direct vs Indirect Effects on Seed Removal")


list.files()


head(data_mergedFULL)



##SEM Vizualisation

#install.packages("DiagrammeR")
library(DiagrammeR)
library(dplyr)
library(stringr)

grViz("
digraph DAG {
  rankdir=LR;
  node [shape = box, style = filled, color = lightgrey]

  SeedProduction [label='Seed Production\n(masting)']
  SeedTraits [label='Seed Traits\n(size, mass)']
  SeedAbundance [label='Seed Abundance\n(at station)']
  Treatment [label='Treatment\n(seed mix / open vs restricted)']
  StandSite [label='Stand / Site']
  StandComp [label='Stand Composition\n(tree dominance)']
  Elevation [label='Elevation']
  Env [label='Environmental Context\n(canopy, cover)']
  PredCommunity [label='Predator Community']
  PredActivity [label='Predator Activity\n(camera visits']
  Weather [label='Weather / Week / Season']
  CameraDetect [label='Camera Detection\nprobability (obs)']
  HumanDist [label='Human Disturbance']
  SeedRemoval [label='Seed Removal\n(outcome)']

  # Edges
  SeedProduction -> SeedAbundance
  SeedProduction -> PredCommunity

  SeedTraits -> SeedRemoval
  SeedTraits -> PredActivity

  SeedAbundance -> PredActivity
  SeedAbundance -> SeedRemoval

  Treatment -> SeedAbundance
  Treatment -> SeedRemoval

  StandSite -> StandComp
  StandSite -> Env
  StandSite -> PredCommunity

  StandComp -> PredCommunity
  Elevation -> StandComp
  Elevation -> Env

  Env -> PredActivity
  Weather -> PredActivity
  HumanDist -> PredActivity
  HumanDist -> CameraDetect

  PredCommunity -> PredActivity
  PredActivity -> SeedRemoval

  CameraDetect -> PredActivity [style=dashed, label='measurement']
}
")

#install.packages("dagitty")
library(dagitty)

d <- dagitty("
dag {
  SeedProduction -> SeedAbundance
  SeedProduction -> PredCommunity
  SeedTraits -> SeedRemoval
  SeedTraits -> PredActivity
  SeedAbundance -> PredActivity
  SeedAbundance -> SeedRemoval
  Treatment -> SeedAbundance
  Treatment -> SeedRemoval
  StandSite -> StandComp
  StandSite -> Env
  StandSite -> PredCommunity
  StandComp -> PredCommunity
  Elevation -> StandComp
  Elevation -> Env
  Env -> PredActivity
  Weather -> PredActivity
  HumanDist -> PredActivity
  HumanDist -> CameraDetect
  PredCommunity -> PredActivity
  PredActivity -> SeedRemoval
  CameraDetect -> PredActivity
}
")

plot(d)  # quick base plot (or use dagitty::coordinates to refine)


list.files()

