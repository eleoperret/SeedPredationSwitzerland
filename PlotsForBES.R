# PLOTsForBES -------------------------------------------------------------



# Packages ----------------------------------------------------------------

library(tidyverse)
install.packages("ggthemes")
library(ggthemes)  # for colorblind palette if needed
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(vegan)  # for diversity calculations
library(tibble)


# DataImport --------------------------------------------------------------

data<-readRDS("Data/data_cleaned_seeds.rds")
camera<-readRDS("Data/CameraData_clean.RDS")


# Data statistics ---------------------------------------------------------

data_boxplot<-data%>%
  filter(!Site%in%c("NAT","CEL"))

#As no removal almost by insects
data_boxplot3<-data_boxplot%>%
  filter(TreatmentNr=="1")

# Calculate summary statistics 
seed_summary_species <- data_boxplot3 %>%
  group_by(Species) %>%
  summarise(
    mean_removal = mean(Removal, na.rm = TRUE),
    se_removal   = sd(Removal, na.rm = TRUE) / sqrt(n())
  )

seed_summary_site <- data_boxplot3 %>%
  group_by(Site) %>%
  summarise(
    mean_removal = mean(Removal, na.rm = TRUE),
    se_removal   = sd(Removal, na.rm = TRUE) / sqrt(n())
  )

seed_summary_treatment <- data_boxplot %>%
  group_by(TreatmentNr) %>%
  summarise(
    mean_removal = mean(Removal, na.rm = TRUE),
    se_removal   = sd(Removal, na.rm = TRUE) / sqrt(n())
  )


# First PLots -------------------------------------------------------------

#Scatterplots

# Species summary for mean
seed_summary_species <- data_boxplot3 %>%
  group_by(Species) %>%
  summarise(
    mean_removal = mean(Removal, na.rm = TRUE),
    se_removal   = sd(Removal, na.rm = TRUE) / sqrt(n())
  )

ggplot(data_boxplot, aes(x = Species, y = Removal)) +
  
  # Raw scatter points in transparent grey
  geom_jitter(width = 0.15, height = 0, alpha = 0.4, color = "grey40", size = 2) +
  
  # Mean removal as square
  geom_point(
    data = seed_summary_species,
    aes(x = Species, y = mean_removal),
    shape = 21,                 # square
    fill = "#0072B2",
    color = "black",
    size = 6
  ) +
  
  # Set y-axis from 0 to 1 with breaks at 0.0, 0.1, ..., 1.0
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0.01, 0.05)) # slight padding at top
  ) +
  
  scale_x_discrete(expand = expansion(mult = 0.15)) +
  
  theme_classic(base_size = 14) +
  labs(
    title = "Seed Removal per Species",
    y = "Seed Removal Proportion",
    x = NULL
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(
      face = "bold",
      margin = margin(b = 15, t = 20)  # extra space above title
    ),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)  # extra outer padding
  )


#For treatment 
# Summary per treatment
seed_summary_treatment <- data_boxplot %>%
  group_by(TreatmentNr) %>%
  summarise(
    mean_removal = mean(Removal, na.rm = TRUE),
    se_removal   = sd(Removal, na.rm = TRUE) / sqrt(n())
  )

# Plot
# Treatment plot with manual spacing control
ggplot(data_boxplot, aes(x = TreatmentNr, y = Removal)) +
  
  # Raw scatter points in transparent grey
  geom_jitter(
    width = 0.3,   # controls horizontal spread of points within each treatment
    height = 0, 
    alpha = 0.4, 
    color = "grey40", 
    size = 2
  ) +
  
  # Mean removal as square
  geom_point(
    data = seed_summary_treatment,
    aes(x = TreatmentNr, y = mean_removal),
    shape = 21,        # filled square
    fill = "darkblue",
    color = "black",
    size = 6
  ) +
  
  # Y-axis manual limits and padding
  scale_y_continuous(
    limits = c(-0.05, 1.05),   # extra space below 0 and above 1
    breaks = seq(0, 1, by = 0.2)
  ) +
  
  # X-axis manual padding between categories
  scale_x_discrete(
    expand = expansion(add = c(0.5, 0.5))  # adds space on left and right
  ) +
  
  theme_classic(base_size = 14) +
  labs(
    title = "Seed Removal per Treatment",
    y = "Seed Removal Proportion",
    x = "Treatment"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", margin = margin(b = 15, t = 20))
  )


#For sites
# Define site order by elevation
site_order <- c("VOR", "WAL", "NEU", "SCH", "BET", "BEA")
site_labels <- c(
  "VOR (487 m)", "WAL (500 m)", "NEU (609 m)",
  "SCH (773 m)", "BET (1196 m)", "BEA (1532 m)"
)

# Summary per site
seed_summary_site <- data_boxplot3 %>%
  filter(Site %in% site_order) %>%
  group_by(Site) %>%
  summarise(
    mean_removal = mean(Removal, na.rm = TRUE),
    se_removal   = sd(Removal, na.rm = TRUE) / sqrt(n())
  ) %>%
  mutate(Site = factor(Site, levels = site_order))

# Plot
ggplot(data_boxplot3 %>% filter(Site %in% site_order), aes(x = Site, y = Removal)) +
  
  # Raw scatter points in transparent grey
  geom_jitter(width = 0.15, height = 0, alpha = 0.4, color = "grey40", size = 2) +
  
  # Mean removal as square
  geom_point(
    data = seed_summary_site,
    aes(x = Site, y = mean_removal),
    shape = 21,                 # filled square
    fill = "lightblue",
    color = "black",
    size = 4
  ) +
  
  # Y-axis from 0 to 1
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  
  scale_x_discrete(labels = site_labels, expand = expansion(mult = 0.15)) +
  
  theme_classic(base_size = 14) +
  labs(
    title = "Seed Removal per Site (ordered by elevation)",
    y = "Seed Removal Proportion",
    x = "Site"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", margin = margin(b = 15, t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  )


# Predator community ------------------------------------------------------

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

str(predators)

# Summarize total sightings per predator species
predator_summary <- predators %>%
  group_by(Species) %>%
  summarise(Total_Sightings = sum(`Sighting Count`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    ymax = cumsum(Total_Sightings),
    ymin = lag(ymax, default = 0)
  )

# Order species by Total_Sightings (descending) for legend
predator_summary <- predator_summary %>%
  arrange(desc(Total_Sightings)) %>%
  mutate(Species = factor(Species, levels = Species))

# Generate a color palette with one color per species
n_species <- nrow(predator_summary)
palette <- colorRampPalette(brewer.pal(8, "Set2"))(n_species)

# Donut plot
ggplot(predator_summary, aes(
  ymax = ymax, ymin = ymin,
  xmax = 4, xmin = 3,
  fill = Species)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +  # donut hole
  scale_fill_manual(values = palette) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  ) +
  labs(title = "Overall Predator Community",
       fill = "Species")

unique(predator_summary$Species)

common_names <- tibble(
  Species = c(
    "Apodemus", "Bird", "Cricetidae", "Sciuridae", "Glis",
    "Domestic Cat", "Leporid", "Wild boar", "Marten", "Micromys",
    "Soricidae", "Mustelid", "Unidentifiable", "Unknown", "Badger",
    "Domestic dog", "Eliomys", "Other", "Wolf", "Red fox"
  ),
  Common = c(
    "Wood mice", "Birds", "Voles", "Squirrels", "Dormouse",
    "Cat", "Hares/Rabbits", "Wild boar", "Marten", "Harvest mice",
    "Shrews", "Mustelids", "Unidentifiable", "Unknown", "Badger",
    "Dog", "Garden dormouse", "Other", "Wolf", "Red fox"
  )
)


# Summarize total sightings per species
predator_summary <- predators %>%
  group_by(Species) %>%
  summarise(Total_Sightings = sum(`Sighting Count`, na.rm = TRUE)) %>%
  ungroup() %>%
  # join with common names
  left_join(common_names, by = "Species") %>%
  mutate(
    ymax = cumsum(Total_Sightings),
    ymin = lag(ymax, default = 0),
    Common = ifelse(is.na(Common), Species, Common)
  ) %>%
  arrange(desc(Total_Sightings)) %>%
  mutate(Common = factor(Common, levels = Common))  # order legend by abundance

# Generate color palette (one color per species)
n_species <- nrow(predator_summary)
palette <- colorRampPalette(brewer.pal(8, "Set2"))(n_species)

# Donut plot
ggplot(predator_summary, aes(
  ymax = ymax, ymin = ymin,
  xmax = 4, xmin = 3,
  fill = Common)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y") +
  xlim(c(2, 4)) +  # donut hole
  scale_fill_manual(values = palette) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  ) +
  labs(title = "Overall Predator Community",
       fill = "Species")

# Large species to exclude
too_large <- c("Domestic Cat", "Domestic dog", "Wild boar", "Wolf", 
               "Badger", "Hares/Rabbits", "Red fox", "Mustelids", "Unidentifiable")

# Summarize total sightings per species
predator_summary <- predators %>%
  filter(!Species %in% too_large) %>%  # remove large species
  group_by(Species) %>%
  summarise(Total_Sightings = sum(`Sighting Count`, na.rm = TRUE)) %>%
  ungroup() %>%
  # join with common names
  left_join(common_names, by = "Species") %>%
  mutate(Common = ifelse(is.na(Common), Species, Common)) %>%
  arrange(desc(Total_Sightings)) %>%  # order by abundance
  mutate(Common = factor(Common, levels = Common))  # ensure legend order

# Normalize cumulative values to 1
total <- sum(predator_summary$Total_Sightings)
predator_summary <- predator_summary %>%
  mutate(
    ymin = lag(cumsum(Total_Sightings) / total, default = 0),
    ymax = cumsum(Total_Sightings) / total
  )

# Generate color palette (one color per species)
n_species <- nrow(predator_summary)
palette <- colorRampPalette(brewer.pal(8, "Set2"))(n_species)

# Donut plot
ggplot(predator_summary, aes(
  ymax = ymax, ymin = ymin,
  xmax = 4, xmin = 3,
  fill = Common)) +
  geom_rect(color = "white") +
  coord_polar(theta = "y", start = 0) +  # first slice at 12 o'clock
  xlim(c(2, 4)) +  # donut hole
  scale_fill_manual(values = palette) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  ) +
  labs(title = "Predator Community (species able to access 5×5 cm boxes)",
       fill = "Species")


# Summarize total sightings per species and calculate percentage
predator_table <- predators %>%
  filter(!Species %in% too_large) %>%  # remove large species
  group_by(Species) %>%
  summarise(Total_Sightings = sum(`Sighting Count`, na.rm = TRUE)) %>%
  ungroup() %>%
  # join with common names
  left_join(common_names, by = "Species") %>%
  mutate(Common = ifelse(is.na(Common), Species, Common)) %>%
  # calculate percentage of total sightings
  mutate(Percentage = round(100 * Total_Sightings / sum(Total_Sightings), 1)) %>%
  arrange(desc(Percentage)) %>%  # order by percentage
  select(Common, Total_Sightings, Percentage)


# Summarize total sightings per species per site
predator_site_counts <- predators %>%
  group_by(Site, Species) %>%
  summarise(Total_Sightings = sum(`Sighting Count`, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = Species, values_from = Total_Sightings, values_fill = 0)

# Calculate diversity metrics per site
diversity_by_site <- predator_site_counts %>%
  rowwise() %>%
  mutate(
    Shannon = diversity(c_across(-Site), index = "shannon"),  # Shannon diversity
    Richness = sum(c_across(-Site) > 0)                       # species richness
  ) %>%
  select(Site, Shannon, Richness)

diversity_by_site

#PLotting the site diversity of predator with removal
# Elevations per site
site_elev <- tibble(
  Site = c("VOR", "WAL", "NEU", "SCH", "BET", "BEA", "CEL", "NAT"),
  Elevation = c(487, 500, 609, 773, 1196, 1532, 1896, 1907)
)

# Merge elevation into plot data
plot_data2 <- diversity_by_site %>%
  left_join(seed_summary_site %>% select(Site, mean_removal), by = "Site") %>%
  left_join(site_elev, by = "Site") %>%
  # Create label with elevation
  mutate(SiteLabel = paste0(Site, " (", Elevation, " m)")) %>%
  # Order by elevation
  arrange(Elevation) %>%
  mutate(SiteLabel = factor(SiteLabel, levels = SiteLabel))

# Plot
ggplot(plot_data2, aes(x = SiteLabel)) +
  # Shannon diversity as bars
  geom_col(aes(y = Shannon), fill = "grey80", color = "black", width = 0.6) +
  # Seed removal as scaled line
  geom_line(aes(y = mean_removal * 3, group = 1), 
            color = "#0072B2", linewidth = 1.2) +
  geom_point(aes(y = mean_removal * 3), 
             color = "#0072B2", size = 3) +
  scale_y_continuous(
    name = "Shannon Diversity",
    sec.axis = sec_axis(~./3, name = "Average Seed Removal")
  ) +
  theme_classic(base_size = 14) +
  labs(
    title = "Predator Shannon Diversity and Seed Removal per Site",
    x = "Site (ordered by elevation)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

ggplot(plot_data2, aes(x = SiteLabel)) +
  # Seed removal as bars on the left axis
  geom_col(aes(y = mean_removal, fill = "Seed Removal"), color = "black", width = 0.6) +
  # Shannon diversity as line + points scaled to match secondary axis
  geom_line(aes(y = Shannon / max(Shannon, na.rm = TRUE) * max(mean_removal, na.rm = TRUE), 
                group = 1, color = "Shannon Diversity"), linewidth = 1.2) +
  geom_point(aes(y = Shannon / max(Shannon, na.rm = TRUE) * max(mean_removal, na.rm = TRUE),
                 color = "Shannon Diversity"), size = 3) +
  
  scale_y_continuous(
    name = "Average Seed Removal",   # left axis
    limits = c(0, max(plot_data2$mean_removal, na.rm = TRUE) * 1.1),
    sec.axis = sec_axis(~ . / max(plot_data2$mean_removal, na.rm = TRUE) * max(plot_data2$Shannon, na.rm = TRUE),
                        name = "Shannon Diversity")  # right axis
  ) +
  
  scale_fill_manual(name = "", values = c("Seed Removal" = "#0072B2")) +
  scale_color_manual(name = "", values = c("Shannon Diversity" = "grey50")) +
  
  theme_classic(base_size = 14) +
  labs(
    title = "Predator Seed Removal and Shannon Diversity per Site",
    x = "Site (ordered by elevation)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

# Merge the data with Shannon diversity
plot_data_combined <- data_boxplot3 %>%
  filter(Site %in% site_order) %>%
  group_by(Site) %>%
  summarise(
    mean_removal = mean(Removal, na.rm = TRUE),
    se_removal   = sd(Removal, na.rm = TRUE) / sqrt(n())
  ) %>%
  left_join(diversity_by_site, by = "Site") %>%   # add Shannon
  mutate(Site = factor(Site, levels = site_order))

# Plot
ggplot(data_boxplot3 %>% filter(Site %in% site_order), aes(x = Site, y = Removal)) +
  
  # Raw removal scatter points
  geom_jitter(width = 0.15, height = 0, alpha = 0.4, color = "grey40", size = 2) +
  
  # Mean removal as square
  geom_point(
    data = plot_data_combined,
    aes(x = Site, y = mean_removal),
    shape = 21, fill = "#0072B2", color = "black", size = 4
  ) +
  
  # Shannon diversity as line + points (scaled)
  geom_line(
    data = plot_data_combined,
    aes(x = Site, y = Shannon / max(Shannon, na.rm = TRUE) * max(mean_removal, na.rm = TRUE),
        group = 1, color = "Shannon Diversity"),
    linewidth = 1.2
  ) +
  geom_point(
    data = plot_data_combined,
    aes(x = Site, y = Shannon / max(Shannon, na.rm = TRUE) * max(mean_removal, na.rm = TRUE),
        color = "Shannon Diversity"),
    size = 3
  ) +
  
  # Scale y-axis for removal (left) and Shannon (right)
  scale_y_continuous(
    name = "Seed Removal Proportion",
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = expansion(mult = c(0.01, 0.05)),
    sec.axis = sec_axis(
      ~ . / max(plot_data_combined$mean_removal, na.rm = TRUE) * max(plot_data_combined$Shannon, na.rm = TRUE),
      name = "Shannon Diversity"
    )
  ) +
  
  scale_x_discrete(labels = site_labels, expand = expansion(mult = 0.15)) +
  scale_color_manual(name = "", values = c("Shannon Diversity" = "darkgreen")) +
  
  theme_classic(base_size = 14) +
  labs(
    title = "Seed Removal and Shannon Diversity per Site (ordered by elevation)",
    x = "Site"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", margin = margin(b = 15, t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    legend.position = "top"
  )





# DOminant tree -----------------------------------------------------------
#PLOT FOR DOMINANT TREE


dominant_species_seed <- tibble(
  Site = c("BEA", "BET", "CEL", "NAT", "NEU", "SCH", "VOR", "WAL"),
  Dominant_Species = c(
    "Picea abies",       # BEA
    "Fagus sylvatica",   # BET
    "Pinus cembra",      # CEL
    "Pinus mugo",        # NAT
    "Fagus sylvatica",   # NEU
    "Fagus sylvatica",   # SCH
    "Abies alba",        # VOR
    "Fagus sylvatica"    # WAL
  )
)

str(data_boxplot)

dominant_species_seed <- tibble(
  Site = c("BEA", "BET", "CEL", "NAT", "NEU", "SCH", "VOR", "WAL"),
  Dominant_Species = c(
    "Picea abies",       # BEA
    "Fagus sylvatica",   # BET
    "Pinus cembra",      # CEL
    "Pinus mugo",        # NAT
    "Fagus sylvatica",   # NEU
    "Fagus sylvatica",   # SCH
    "Abies alba",        # VOR
    "Fagus sylvatica"    # WAL
  )
)

removal_summary <- data_boxplot %>%
  group_by(Site, Species) %>%
  summarise(
    mean_removal = mean(Removal, na.rm = TRUE),
    .groups = "drop"
  )


plot_data <- removal_summary %>%
  left_join(dominant_species_seed, by = "Site") %>%
  mutate(
    is_dominant = ifelse(Species == Dominant_Species, "Dominant", "Other")
  )


# Define site elevations
site_elev <- tibble(
  Site = c("BET", "VOR", "WAL", "NEU", "SCH", "BEA", "CEL", "NAT"),
  Elevation = c(1196, 487, 500, 609, 773, 1532, 1896, 1907)
)

# Color-blind friendly colors
dominant_color <- "#0072B2"  # blue
other_color <- "grey70"

# Prepare plot data
plot_data <- data_boxplot3 %>%
  group_by(Site, Species) %>%
  summarise(mean_removal = mean(Removal, na.rm = TRUE), .groups = "drop") %>%
  left_join(dominant_species_seed, by = "Site") %>%
  left_join(site_elev, by = "Site") %>%
  mutate(
    fill_color = ifelse(Species == Dominant_Species, dominant_color, other_color),
    Site = factor(Site, levels = site_elev$Site[order(site_elev$Elevation)])  # order by elevation
  ) %>%
  group_by(Site) %>%
  arrange(desc(mean_removal)) %>%
  mutate(Species = factor(Species, levels = unique(Species))) %>%
  ungroup()

# Plot
ggplot(plot_data, aes(x = Species, y = mean_removal, fill = fill_color)) +
  geom_col(color = "black", width = 0.7) +
  facet_wrap(~Site, scales = "free_x") +
  scale_fill_identity(name = "Species Status", labels = c("Dominant", "Other"), guide = "legend") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Average Seed Removal per Species per Site",
    y = "Average Removal Proportion",
    x = "Species"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )


list.files("Data")

library(readxl)
library(dplyr)
library(vegan)  # for Shannon index
library(ggplot2)

# Load seed fall data
seed_fall <- read_excel("Data/SortedSeeds_LWF_Raw_2023.xlsm")
str(seed_fall)


library(dplyr)
library(ggplot2)

# Seed fall per site × species
seed_fall_species <- seed_fall %>%
  group_by(Site, Species) %>%
  summarise(total_seeds = sum(`Number of Seeds`, na.rm = TRUE), .groups = "drop")

library(dplyr)

library(dplyr)

library(dplyr)
library(ggplot2)

# 1. Summarize seed removal per site and species
removal_summary <- data_boxplot %>%
  filter(Site != "WAL") %>%         # remove WAL as no seed fall data
  group_by(Site, Species) %>%
  summarise(mean_removal = mean(Removal, na.rm = TRUE), .groups = "drop")

# 2. Summarize seed fall per site and species
seedfall_summary <- seed_fall %>%
  filter(stand != "WAL") %>%       # remove WAL
  group_by(stand, spp) %>%
  summarise(total_seeds = sum(loose_sds_filled, na.rm = TRUE), .groups = "drop") %>%
  rename(Site = stand, Species = spp)

# 3. Join both datasets
plot_data <- removal_summary %>%
  left_join(seedfall_summary, by = c("Site", "Species")) %>%
  filter(!is.na(total_seeds))       # remove NAs in seed fall

library(dplyr)
library(ggplot2)
library(ggpattern)

library(ggplot2)
library(dplyr)

# Prepare highlight columns
plot_data <- plot_data %>%
  group_by(Site) %>%
  mutate(
    highlight_removal = ifelse(mean_removal == max(mean_removal, na.rm = TRUE), "Highest Removal (2024)", "Other Removal (2024)"),
    highlight_fall = ifelse(total_seeds == max(total_seeds, na.rm = TRUE), TRUE, FALSE)
  ) %>%
  ungroup()

ggplot(plot_data, aes(x = Species)) +
  # Seed removal as bars
  geom_col(aes(y = mean_removal, fill = highlight_removal), color = "black", width = 0.6) +
  # Seed fall as line
  geom_line(aes(y = total_seeds / max(total_seeds, na.rm = TRUE) * 0.3, group = 1, color = "Seed Fall (2023)"), linewidth = 1.2) +
  geom_point(aes(y = total_seeds / max(total_seeds, na.rm = TRUE) * 0.3, color = "Seed Fall (2023)"), size = 3) +
  # Star for highest producer
  geom_point(
    data = subset(plot_data, highlight_fall),
    aes(y = total_seeds / max(total_seeds, na.rm = TRUE) * 0.3),
    shape = 8, size = 4, color = "red"
  ) +
  facet_wrap(~Site, scales = "free_x") +
  scale_y_continuous(
    name = "Average Seed Removal Proportion (2024)",
    limits = c(0, 0.3),
    sec.axis = sec_axis(~ . / 0.3 * max(plot_data$total_seeds, na.rm = TRUE), name = "Total Seed Fall (2023)")
  ) +
  scale_fill_manual(name = "", values = c("Highest Removal (2024)" = "#E69F00", "Other Removal (2024)" = "grey80")) +
  scale_color_manual(name = "", values = c("Seed Fall (2023)" = "grey50")) +
  theme_classic(base_size = 14) +
  labs(
    title = "Seed Removal (2024) and Seed Fall (2023) per Site and Species",
    x = "Species"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )


# Define site elevations
site_elev <- tibble(
  Site = c("VOR", "NEU", "SCH", "BET", "BEA"),  # exclude WAL
  Elevation = c(487, 609, 773, 1196, 1532)
)

# Merge elevation info and convert Site to factor ordered by elevation
plot_data <- plot_data %>%
  left_join(site_elev, by = "Site") %>%
  mutate(Site = factor(Site, levels = site_elev$Site[order(site_elev$Elevation)]))

# Plot
ggplot(plot_data, aes(x = Species)) +
  # Seed removal as bars
  geom_col(aes(y = mean_removal, fill = highlight_removal), color = "black", width = 0.6) +
  
  # Seed fall as line
  geom_line(aes(y = total_seeds / max(total_seeds, na.rm = TRUE) * 0.3, group = 1, color = "Seed Fall (2023)"), linewidth = 1.2) +
  geom_point(aes(y = total_seeds / max(total_seeds, na.rm = TRUE) * 0.3, color = "Seed Fall (2023)"), size = 3) +
  
  # Star for highest producer
  geom_point(
    data = subset(plot_data, highlight_fall),
    aes(y = total_seeds / max(total_seeds, na.rm = TRUE) * 0.3),
    shape = 8, size = 4, color = "red"
  ) +
  
  facet_wrap(~Site, scales = "free_x") +
  scale_y_continuous(
    name = "Average Seed Removal Proportion (2024)",
    limits = c(0, 0.3),
    sec.axis = sec_axis(~ . / 0.3 * max(plot_data$total_seeds, na.rm = TRUE), name = "Total Seed Fall (2023)")
  ) +
  scale_fill_manual(name = "", values = c("Highest Removal (2024)" = "#E69F00", "Other Removal (2024)" = "grey80")) +
  scale_color_manual(name = "", values = c("Seed Fall (2023)" = "grey50")) +
  theme_classic(base_size = 14) +
  labs(
    title = "Seed Removal (2024) and Seed Fall (2023) per Site and Species",
    x = "Species"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "top"
  )
