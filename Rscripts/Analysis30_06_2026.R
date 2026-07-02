

library(dplyr)
library(ggplot2)
library(purrr)
library(ggeffects)
library(glmmTMB)

getwd()
setwd("C:/Users/eperret/polybox - Eleonore Perret (eleonore.perret@usys.ethz.ch)@polybox.ethz.ch/phD/PhD/R/Seed_predation/Seed_predation_CH/Seed_predation_CH_analysis_Desktop")
 list.files()
list.files("Data/Data_For_Analysis/")
list.files("Data/CameraData/")

#Importing datasets
seed_data<-read.csv("Data/Data_For_Analysis/Seeds_trap_data2.csv", sep=";") #from seed traps
removal_data2024<-read.csv("Data/Data_For_Analysis/FinalCorrected_review_EP.csv", sep=";")
removal_data2025<-read.csv("Data/Data_For_Analysis/SeedSortingData_2025_Clean.csv", sep=",")
#to add camera_data


# Seed data cleaning ------------------------------------------------------

#Seed sorting data clean-up
seed_data<-seed_data%>%
  mutate(total_seeds=Filled_seeds+Cones_filled_seeds)

seeds_years<-seed_data%>%
  group_by(Year,Species,Site)%>%
  summarise(total=sum(total_seeds))

seeds_years <- seeds_years %>%
  mutate(Species = recode(Species,
                          "Abies alba " = "Abies alba",
                          "Fagus sylvatica " = "Fagus sylvatica",
                          "Fraxinus excelsior " = "Fraxinus excelsior",
                          "Larix decidua " = "Larix decidua",
                          "Pinus cembra " = "Pinus cembra",
                          "Thuja spp. " = "Thuja spp.",
                          "Umbellif. " = "Umbellif.",
                          "unknown" = "Unknown",
                          "Tilia platyphyllus" = "Tilia platyphyllos"
  ))

seeds_years <- seeds_years %>%
  mutate(Site = recode(Site,
                          "BET " = "BET",
                          "CEL " = "CEL",
                          "NAT " = "NAT",
                          "SCH " = "SCH",
                          "BEA " = "BEA",
                          "VOR " = "VOR"
  ))

unique(seeds_years$Species)
unique(seeds_years$Site)

#Selecting the seeds that belong to my experiment
seeds_interest<-seeds_years%>%
  filter(Species%in% c("Abies alba", "Acer pseudoplatanus","Fagus sylvatica","Picea abies","Tilia cordata"))%>%
  filter (Site != c("CEL"))

#Plotting
seeds_interest_summed <- seeds_interest %>%
  group_by(Year, Species, Site) %>%
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop")

seeds_disco <- seeds_interest%>%
  filter(Site %in% c("BET","SCH", "VOR"))%>%
  group_by(Year, Species) %>%
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop")

ggplot(seeds_disco, aes(x=Year, y= total, group= Species, color= Species))+
         geom_line()+
         geom_point()+
        labs(x = "Year", y = "Total seeds", color = "Species") +
  theme_minimal()
         
seeds_interest_summed2<-seeds_interest_summed%>%
  filter(Species %in%
           c("Fagus sylvatica", "Picea abies"))

ggplot(seeds_interest_summed, aes(x = Year, y = total, color = Species, group = Species)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Site) +
  labs(x = "Year", y = "Total seeds", color = "Species") +
  theme_minimal()


ggplot(seeds_interest_summed, aes(x = Year, y = total, color = Site, group = Site)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Species) +
  labs(x = "Year", y = "Total seeds", color = "Species") +
  theme_minimal()


# Seed removal data  ------------------------------------------------------

str(removal_data2024)
str(removal_data2025)

removal_data<-merge(removal_data2024,removal_data2025, by = c("DateBag","Site"))
removal_data <- bind_rows(
  removal_data2024 %>% mutate(Year = 2024),
  removal_data2025 %>% mutate(Year = 2025)
)

# Calculate proportion removed
removal_data <- removal_data %>%
  mutate(PropRemoved = pmax((NbSeedStart - SeedsCount) / NbSeedStart, 0)) %>%
  filter(Site %in% c("BEA", "BET","NEU","SCH","VOR","WAL"))
  

# Summarize: mean and SE per Year, Species, Site
summary_data <- removal_data %>%
  group_by(Year, Species, Site) %>%
  summarise(
    MeanProp = mean(PropRemoved, na.rm = TRUE),
    SE = sd(PropRemoved, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

# Plot
ggplot(summary_data, aes(x = Species, y = MeanProp, fill = factor(Year))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = MeanProp - SE, ymax = MeanProp + SE),
    position = position_dodge(width = 0.9),
    width = 0.25
  ) +
  facet_wrap(~ Site) +
  labs(
    x = "Species",
    y = "Proportion of seeds removed",
    fill = "Year",
    title = "Seed removal by species and site, 2024 vs 2025"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Joint -------------------------------------------------------------------

# Full join to keep all sites from both datasets, even non-matching ones
combined_data <- full_join(
  seeds_interest_summed,
  summary_data,
  by = c("Year", "Species", "Site")
)

# Scaling factor: map proportion (0-1) onto the seed count range so both
# quantities are visible on the same plot. We scale per the max seed count
# so the line doesn't disappear or dominate.
max_seeds <- max(combined_data$total, na.rm = TRUE)
scale_factor <- max_seeds  # since MeanProp is 0-1, multiply by max to match range

ggplot(combined_data, aes(x = factor(Year))) +
  geom_bar(aes(y = total, fill = Species, group = Species),
           stat = "identity", position = position_dodge(width = 0.9), alpha = 0.7) +
  geom_line(aes(y = MeanProp * scale_factor, color = Species, group = Species),
            linewidth = 1, position = position_dodge(width = 0.9)) +
  geom_point(aes(y = MeanProp * scale_factor, color = Species, group = Species),
             size = 2, position = position_dodge(width = 0.9)) +
  facet_wrap(~ Site) +
  scale_y_continuous(
    name = "Total seeds produced",
    sec.axis = sec_axis(~ . / scale_factor, name = "Proportion removed")
  ) +
  labs(x = "Year", fill = "Species (bars = production)", color = "Species (line = removal)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Thinking about the model ------------------------------------------------

rank_by_site_year <- seeds_interest_summed %>%
  group_by(Site, Year) %>%
  mutate(DominanceRank = rank(-total, ties.method = "first")) %>%
  ungroup() %>%
  mutate(DominanceRank = factor(DominanceRank, labels = c("Dominant"))) %>%
  select(Site, Year, Species, DominanceRank)

removal_data <- removal_data %>%
  left_join(rank_by_site_year, by = c("Site", "Year", "Species"))



#Testing a model for : " Is the heaviest seed producer the most removed?"
#Year since mast

removal_data <- removal_data %>%
  mutate(NbRemoved = NbSeedStart - SeedsCount,
         NbRemoved = pmax(NbRemoved, 0))  # in case you haven't already added this

model_rank_only <- glmmTMB(
  cbind(NbRemoved, SeedsCount) ~ DominanceRank +
    (1 | Site) + (1 | Station),
  family = binomial,
  data = removal_data
)


table(removal_data$DominanceRank, removal_data$Site)
levels(removal_data$DominanceRank)
summary(model_rank_only)

preds <- ggpredict(model_rank, terms = c("YearsSinceMast", "DominanceRank"))
plot(preds)
