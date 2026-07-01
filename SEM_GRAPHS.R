library(dagitty)
library(ggdag)
library(tidyverse)
library(dplyr)

# ============================================================================
# DÉFINITION DU DAG POUR VOTRE SEM
# ============================================================================

dag_code <- 'dag {
  # ===== VARIABLES OBSERVÉES (rectangles bleus) =====
  Canopy_cover [pos="0,3"]
  Veg_cover [pos="0,2"]
  Tree_dominance [pos="0,1"]
  
  Predator_visits [pos="0,6"]
  Predator_diversity [pos="0,5"]
  
  Treatment [pos="0,0"]
  Species [pos="0,-1"]
  Site [pos="0,7"]
  
  # ===== VARIABLES LATENTES (ellipses oranges) =====
  Habitat_structure [latent, pos="2,2"]
  Predator_pressure [latent, pos="2,5.5"]
  
  # ===== VARIABLE RÉPONSE (rectangle vert) =====
  Removal [outcome, pos="5,3.5"]
  
  # ===== MODÈLE DE MESURE (variables → latentes) =====
  Canopy_cover -> Habitat_structure
  Veg_cover -> Habitat_structure
  Tree_dominance -> Habitat_structure
  
  Predator_visits -> Predator_pressure
  Predator_diversity -> Predator_pressure
  
  # ===== RELATIONS STRUCTURELLES =====
  # Effet de l\'habitat sur la pression de prédation
  Habitat_structure -> Predator_pressure
  
  # Effets sur le removal (variable réponse)
  Predator_pressure -> Removal
  Habitat_structure -> Removal
  Treatment -> Removal
  Species -> Removal
  Site -> Removal
  
  # Variables de contrôle
  Site -> Habitat_structure
  Site -> Predator_pressure
  Treatment -> Predator_pressure
}'

# Créer le DAG
sem_dag <- dagitty(dag_code)

# Convertir en tibble pour ggplot
dag_df <- tidy_dagitty(sem_dag)

# ============================================================================
# DÉFINIR LES TYPES DE NŒUDS POUR LA VISUALISATION
# ============================================================================

node_types <- tibble(
  name = c("Canopy_cover", "Veg_cover", "Tree_dominance",
           "Predator_visits", "Predator_diversity",
           "Treatment", "Species", "Site",
           "Habitat_structure", "Predator_pressure",
           "Removal"),
  type = c(rep("observed_habitat", 3),
           rep("observed_predator", 2),
           rep("control", 3),
           rep("latent", 2),
           "response"),
  label = c("Canopy\ncover", "Vegetation\ncover", "Tree\ndominance",
            "Predator\nvisits", "Predator\ndiversity",
            "Treatment", "Seed\nspecies", "Site",
            "Habitat\nstructure", "Predator\npressure",
            "Seed\nRemoval")
)

# Joindre les types au DAG
dag_df <- dag_df %>%
  left_join(node_types, by = "name") %>%
  mutate(
    type = ifelse(is.na(type), "observed", type),
    label = ifelse(is.na(label), name, label)
  )

# ============================================================================
# CRÉER LE GRAPHIQUE PRINCIPAL
# ============================================================================

p1 <- ggplot(dag_df, aes(x = x, y = y, xend = xend, yend = yend)) +
  # Arêtes (flèches)
  geom_dag_edges(
    edge_color = "gray40",
    edge_width = 0.8,
    arrow_directed = grid::arrow(length = unit(10, "pt"), type = "closed")
  ) +
  # Nœuds (points)
  geom_dag_point(
    aes(color = type, shape = type),
    size = 18
  ) +
  # Étiquettes
  geom_dag_text(
    aes(label = label),
    color = "black",
    size = 3.5,
    fontface = "bold"
  ) +
  # Couleurs personnalisées
  scale_color_manual(
    values = c(
      "observed_habitat" = "#3498DB",      # Bleu pour habitat
      "observed_predator" = "#9B59B6",     # Violet pour prédateurs
      "control" = "#95A5A6",               # Gris pour contrôles
      "latent" = "#F39C12",                # Orange pour latentes
      "response" = "#27AE60"               # Vert pour réponse
    ),
    labels = c(
      "observed_habitat" = "Variables d'habitat",
      "observed_predator" = "Variables de prédation",
      "control" = "Variables de contrôle",
      "latent" = "Variables latentes",
      "response" = "Variable réponse"
    ),
    name = "Type de variable"
  ) +
  # Formes personnalisées
  scale_shape_manual(
    values = c(
      "observed_habitat" = 15,      # Carré
      "observed_predator" = 15,     # Carré
      "control" = 15,               # Carré
      "latent" = 19,                # Cercle
      "response" = 18               # Diamant
    ),
    guide = "none"
  ) +
  # Thème
  theme_dag() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    title = "Structural Equation Model (SEM)",
    subtitle = "Effets de l'environnement et de la pression de prédation sur le removal des graines"
  )

print(p1)

# ============================================================================
# GRAPHIQUE ALTERNATIF : LAYOUT HIÉRARCHIQUE
# ============================================================================

# Recréer avec positions hiérarchiques plus claires
dag_hierarchical <- 'dag {
  # Niveau 1 : Variables de contrôle (en haut)
  Site [pos="3,5"]
  Treatment [pos="1,5"]
  Species [pos="5,5"]
  
  # Niveau 2 : Variables observées d\'habitat
  Canopy_cover [pos="0,4"]
  Veg_cover [pos="1,4"]
  Tree_dominance [pos="2,4"]
  
  # Niveau 2 : Variables observées de prédation
  Predator_visits [pos="4,4"]
  Predator_diversity [pos="5,4"]
  
  # Niveau 3 : Variables latentes
  Habitat_structure [latent, pos="1,3"]
  Predator_pressure [latent, pos="4.5,3"]
  
  # Niveau 4 : Variable réponse
  Removal [outcome, pos="2.75,1"]
  
  # Relations
  Canopy_cover -> Habitat_structure
  Veg_cover -> Habitat_structure
  Tree_dominance -> Habitat_structure
  
  Predator_visits -> Predator_pressure
  Predator_diversity -> Predator_pressure
  
  Habitat_structure -> Predator_pressure
  
  Predator_pressure -> Removal
  Habitat_structure -> Removal
  Treatment -> Removal
  Species -> Removal
  Site -> Removal
  
  Site -> Habitat_structure
  Site -> Predator_pressure
  Treatment -> Predator_pressure
}'

sem_dag_hier <- dagitty(dag_hierarchical)
dag_df_hier <- tidy_dagitty(sem_dag_hier)

dag_df_hier <- dag_df_hier %>%
  left_join(node_types, by = "name") %>%
  mutate(
    type = ifelse(is.na(type), "observed", type),
    label = ifelse(is.na(label), name, label)
  )

p2 <- ggplot(dag_df_hier, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(
    edge_color = "gray40",
    edge_width = 0.8,
    arrow_directed = grid::arrow(length = unit(10, "pt"), type = "closed")
  ) +
  geom_dag_point(
    aes(color = type, shape = type),
    size = 20
  ) +
  geom_dag_text(
    aes(label = label),
    color = "black",
    size = 3.5,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c(
      "observed_habitat" = "#3498DB",
      "observed_predator" = "#9B59B6",
      "control" = "#95A5A6",
      "latent" = "#F39C12",
      "response" = "#27AE60"
    ),
    labels = c(
      "observed_habitat" = "Variables d'habitat",
      "observed_predator" = "Variables de prédation",
      "control" = "Variables de contrôle",
      "latent" = "Variables latentes",
      "response" = "Variable réponse"
    ),
    name = "Type de variable"
  ) +
  scale_shape_manual(
    values = c(
      "observed_habitat" = 15,
      "observed_predator" = 15,
      "control" = 15,
      "latent" = 19,
      "response" = 18
    ),
    guide = "none"
  ) +
  theme_dag() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    title = "SEM - Layout hiérarchique",
    subtitle = "De gauche à droite : Variables de contrôle → Variables observées → Latentes → Réponse"
  )

print(p2)

# ============================================================================
# GRAPHIQUE SIMPLIFIÉ : FOCUS SUR LES VARIABLES LATENTES
# ============================================================================

dag_simple <- 'dag {
  # Variables latentes et réponse seulement
  Habitat_structure [latent, pos="1,2"]
  Predator_pressure [latent, pos="1,4"]
  Removal [outcome, pos="3,3"]
  
  # Variables de contrôle principales
  Treatment [pos="1,1"]
  Species [pos="1,5"]
  Site [pos="1,3"]
  
  # Relations principales
  Habitat_structure -> Predator_pressure
  Habitat_structure -> Removal
  Predator_pressure -> Removal
  Treatment -> Removal
  Species -> Removal
  Site -> Removal
  Site -> Habitat_structure
  Site -> Predator_pressure
}'

sem_dag_simple <- dagitty(dag_simple)
dag_df_simple <- tidy_dagitty(sem_dag_simple)

node_types_simple <- tibble(
  name = c("Habitat_structure", "Predator_pressure", "Removal",
           "Treatment", "Species", "Site"),
  type = c("latent", "latent", "response", "control", "control", "control"),
  label = c("Habitat\nstructure", "Predator\npressure", "Seed\nRemoval",
            "Treatment", "Seed\nspecies", "Site")
)

dag_df_simple <- dag_df_simple %>%
  left_join(node_types_simple, by = "name")

p3 <- ggplot(dag_df_simple, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(
    edge_color = "gray40",
    edge_width = 1.2,
    arrow_directed = grid::arrow(length = unit(12, "pt"), type = "closed")
  ) +
  geom_dag_point(
    aes(color = type, shape = type),
    size = 25
  ) +
  geom_dag_text(
    aes(label = label),
    color = "black",
    size = 4.5,
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c(
      "control" = "#95A5A6",
      "latent" = "#F39C12",
      "response" = "#27AE60"
    ),
    labels = c(
      "control" = "Variables de contrôle",
      "latent" = "Variables latentes (composites)",
      "response" = "Variable réponse"
    ),
    name = "Type de variable"
  ) +
  scale_shape_manual(
    values = c(
      "control" = 15,
      "latent" = 19,
      "response" = 18
    ),
    guide = "none"
  ) +
  theme_dag() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 11),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    title = "SEM simplifié - Relations structurelles principales",
    subtitle = "Focus sur les variables latentes et la variable réponse"
  )

print(p3)

# ============================================================================
# LÉGENDE ET EXPLICATIONS
# ============================================================================

cat("\n")
cat("╔═══════════════════════════════════════════════════════════════════════╗\n")
cat("║                    EXPLICATION DU DIAGRAMME SEM                       ║\n")
cat("╚═══════════════════════════════════════════════════════════════════════╝\n\n")

cat("🔵 RECTANGLES BLEUS : Variables observées d'habitat\n")
cat("   • Canopy cover, Vegetation cover, Tree dominance\n")
cat("   → Mesurent la structure de l'habitat\n\n")

cat("🟣 RECTANGLES VIOLETS : Variables observées de prédation\n")
cat("   • Predator visits, Predator diversity\n")
cat("   → Mesurent la pression de prédation\n\n")

cat("🟠 CERCLES ORANGE : Variables latentes (non observées directement)\n")
cat("   • Habitat structure : composite des variables d'habitat\n")
cat("   • Predator pressure : composite des variables de prédation\n")
cat("   → Représentent des concepts théoriques\n\n")

cat("⬜ RECTANGLES GRIS : Variables de contrôle\n")
cat("   • Treatment, Species, Site\n")
cat("   → Facteurs expérimentaux et contextuels\n\n")

cat("💚 DIAMANT VERT : Variable réponse\n")
cat("   • Seed Removal (taux de prédation)\n")
cat("   → Ce que vous cherchez à expliquer\n\n")

cat("➡️  FLÈCHES : Relations causales\n")
cat("   • Variables observées → Latentes : définition du concept\n")
cat("   • Latentes/Contrôles → Removal : effets causaux\n\n")

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("ÉQUATIONS DU MODÈLE :\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat("MODÈLE DE MESURE (variables latentes) :\n")
cat("  Habitat_structure =~ Canopy_cover + Veg_cover + Tree_dominance\n")
cat("  Predator_pressure =~ Predator_visits + Predator_diversity\n\n")

cat("MODÈLE STRUCTUREL (relations causales) :\n")
cat("  Predator_pressure ~ Habitat_structure + Site + Treatment\n")
cat("  Removal ~ Predator_pressure + Habitat_structure + Treatment + Species + Site\n\n")

cat("═══════════════════════════════════════════════════════════════════════\n\n")

# Sauvegarder les graphiques
ggsave("SEM_diagram_complete.png", plot = p1, width = 12, height = 10, dpi = 300)
ggsave("SEM_diagram_hierarchical.png", plot = p2, width = 12, height = 10, dpi = 300)
ggsave("SEM_diagram_simplified.png", plot = p3, width = 10, height = 8, dpi = 300)

cat("✓ Graphiques sauvegardés :\n")
cat("  • SEM_diagram_complete.png\n")
cat("  • SEM_diagram_hierarchical.png\n")
cat("  • SEM_diagram_simplified.png\n")