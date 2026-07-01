library(lavaan)
library(tidyverse)
library(dagitty)
library(ggdag)
library(semPlot)

data<-readRDS("Data/data_mergedFULL.rds")

str(data)


#Preparation of the data

#Vegetation cover
data <- data %>%
  mutate(Veg_cover = as.numeric(Veg_cover),
         Veg_cover_mid = case_when(
           Veg_cover == 0 ~ 0,
           Veg_cover == 1 ~ 2.5,
           Veg_cover == 2 ~ 15,
           Veg_cover == 3 ~ 37.5,
           Veg_cover == 4 ~ 62.5,
           Veg_cover == 5 ~ 87.5,
           TRUE ~ NA_real_
         ))
#DOminance of tree per sites
data <- data %>%
  mutate(Tree_dominance = case_when(
    Site == "Neunkirch" ~ "Fagus sylvatica",
    Site == "Vordemwald" ~ "Abies alba",
    Site == "Bettlachstock" ~ "Fagus sylvatica",
    Site == "Nationalpark" ~ "Pinus mugo",
    Site == "Celerina" ~ "Pinus cembra",
    Site == "Beatenberg" ~ "Picea abies",
    Site == "Schaenis" ~ "Fagus sylvatica",
    TRUE ~ NA_character_
  ))
#Other option
data <- data %>%
  mutate(Forest_type = case_when(
    Tree_dominance %in% c("Fagus sylvatica", "Tilia platyphyllos") ~ "Deciduous",
    Tree_dominance %in% c("Abies alba", "Picea abies", "Pinus mugo", "Pinus cembra", "Larix decidua") ~ "Coniferous",
    TRUE ~ "Mixed"
  ))



# ============================================================================
# GUIDE : PRÉPARATION DES VARIABLES POUR LE SEM
# ============================================================================

# VARIABLES ET LEUR FORMAT RECOMMANDÉ :
# 
# a) Canopy_cover : NUMÉRIQUE (déjà OK) - pourcentage ou proportion
# 
# b) Veg_cover : ORDINALE → convertir en numérique
#    Méthode 1 (RECOMMANDÉE) : Utiliser le milieu de chaque catégorie
#    0 → 0, 0-5% → 2.5, 5-25% → 15, 25-50% → 37.5, etc.
#    
# c) Tree_dominance : CATÉGORIELLE → variables dummy OU multigroupe
#    Option 1 : Dummy variables (si peu de catégories)
#    Option 2 : Multigroupe SEM (RECOMMANDÉ si effet modérateur attendu)
#    
# d) Predator_visits : NUMÉRIQUE - visites/jour de caméra (standardisé par effort)
#    Formula : visits / (camera_days)
#    
# e) Predator_diversity : NUMÉRIQUE - richesse spécifique par site
#    
# f) Removal : PROPORTION (0-1) - RECOMMANDÉ
#    Ou : succès binomial (pour modèle à plusieurs niveaux)
#    Formula : (NbSeedStart - SeedsLeft) / NbSeedStart
#    
# g) Site : FACTEUR - pour effets aléatoires ou fixed effects
#    
# h) Treatment : FACTEUR BINAIRE (Open vs Closed) - variable de contrôle
#    
# i) Seed_species : FACTEUR - pour multigroupe SEM

# ============================================================================
# 1. PRÉPARATION DES DONNÉES
# ============================================================================

prepare_data_for_sem <- function(data) {
  
  data_prepared <- data %>%
    mutate(
      # a) Canopy cover : déjà numérique
      Canopy_cover_num = as.numeric(Canopy_cover),
      
      # b) Vegetation cover : convertir catégories en numérique
      Veg_cover_num = case_when(
        Veg_cover == "0" | Veg_cover == 0 ~ 0,
        Veg_cover == "0-5%" ~ 2.5,
        Veg_cover == "5-25%" ~ 15,
        Veg_cover == "25-50%" ~ 37.5,
        Veg_cover == "50-75%" ~ 62.5,
        Veg_cover == "75-100%" ~ 87.5,
        TRUE ~ as.numeric(Veg_cover) # Si déjà numérique
      ),
      
      # c) Tree dominance : créer dummies (référence = espèce la plus commune)
      # Adapter selon vos espèces réelles
      Tree_Picea = ifelse(grepl("Picea|Spruce", Tree_dominance, ignore.case = TRUE), 1, 0),
      Tree_Abies = ifelse(grepl("Abies|Fir", Tree_dominance, ignore.case = TRUE), 1, 0),
      Tree_Fagus = ifelse(grepl("Fagus|Beech", Tree_dominance, ignore.case = TRUE), 1, 0),
      # Ajouter autres espèces si nécessaire
      
      # d) Predator visits : standardiser par effort caméra
      # Supposons que vous avez Camera_days ou Camera_effort
      Predator_visits_std = Predator_visits / Camera_effort,
      
      # e) Predator diversity : déjà numérique (richesse)
      Predator_diversity_num = as.numeric(Predator_diversity),
      
      # f) Removal : proportion (RECOMMANDÉ pour SEM)
      Removal_prop = (NbSeedStart - SeedsLeft) / NbSeedStart,
      # Transformation logit pour normalité (optionnel)
      Removal_logit = log((Removal_prop + 0.001) / (1 - Removal_prop + 0.001)),
      
      # g) Site : facteur
      Site_f = as.factor(Site),
      
      # h) Treatment : facteur binaire
      Treatment_f = factor(Treatment, levels = c("Closed", "Open")),
      Treatment_binary = ifelse(Treatment == "Open", 1, 0),
      
      # i) Seed species : facteur
      Species_f = as.factor(Species),
      
      # Standardiser les variables continues pour meilleure convergence
      Canopy_cover_z = scale(Canopy_cover_num)[,1],
      Veg_cover_z = scale(Veg_cover_num)[,1],
      Predator_visits_z = scale(Predator_visits_std)[,1],
      Predator_diversity_z = scale(Predator_diversity_num)[,1]
    ) %>%
    # Enlever les NA dans les variables clés
    filter(!is.na(Removal_prop), !is.na(Predator_visits_std))
  
  return(data_prepared)
}

# Appliquer la préparation
# data_sem <- prepare_data_for_sem(data_mergedFULL)

# ============================================================================
# 2. MODÈLE SEM PRINCIPAL (RECOMMANDÉ)
# ============================================================================

# STRUCTURE RECOMMANDÉE avec variables latentes

sem_model_full <- '
  # ===== MODÈLE DE MESURE =====
  
  # Variable latente : Habitat structure
  Habitat_structure =~ Canopy_cover_z + Veg_cover_z + Tree_Picea + Tree_Abies
  
  # Variable latente : Predator pressure
  Predator_pressure =~ Predator_visits_z + Predator_diversity_z
  
  # ===== MODÈLE STRUCTUREL =====
  
  # Effets sur la pression de prédation
  Predator_pressure ~ Habitat_structure + Site_f
  
  # Effets sur le removal (VARIABLE RÉPONSE)
  Removal_prop ~ Predator_pressure + 
                 Habitat_structure + 
                 Treatment_binary + 
                 Species_f +
                 Site_f
  
  # Interactions potentielles (à tester)
  # Removal_prop ~ Predator_pressure:Treatment_binary
  # Removal_prop ~ Habitat_structure:Species_f
  
  # Covariances
  Site_f ~~ Treatment_binary
'

# ============================================================================
# 3. APPROCHES ALTERNATIVES SELON VOS QUESTIONS
# ============================================================================

# ----- APPROCHE 1 : UN SEM PAR TREATMENT (Simple mais perd de l'info) -----

sem_by_treatment <- function(data, treatment_level) {
  
  data_subset <- data %>% filter(Treatment == treatment_level)
  
  model <- '
    Habitat_structure =~ Canopy_cover_z + Veg_cover_z + Tree_Picea + Tree_Abies
    Predator_pressure =~ Predator_visits_z + Predator_diversity_z
    
    Predator_pressure ~ Habitat_structure + Site_f
    Removal_prop ~ Predator_pressure + Habitat_structure + Species_f + Site_f
  '
  
  fit <- sem(model, data = data_subset, estimator = "MLR")
  return(fit)
}

# Utilisation :
# fit_open <- sem_by_treatment(data_sem, "Open")
# fit_closed <- sem_by_treatment(data_sem, "Closed")

# ----- APPROCHE 2 : MULTIGROUPE SEM PAR SEED SPECIES (RECOMMANDÉ) -----

sem_multigroup_species <- '
  # Modèle identique pour tous les groupes
  Habitat_structure =~ Canopy_cover_z + Veg_cover_z + Tree_Picea + Tree_Abies
  Predator_pressure =~ Predator_visits_z + Predator_diversity_z
  
  Predator_pressure ~ Habitat_structure + Site_f
  Removal_prop ~ c("b1", "b2", "b3")*Predator_pressure +  # Contraintes d\'égalité à tester
              c("b4", "b5", "b6")*Habitat_structure + 
              Treatment_binary + 
              Site_f
'

# Estimation multigroupe
# fit_multigroup <- sem(sem_multigroup_species, 
#                       data = data_sem, 
#                       group = "Species_f",
#                       estimator = "MLR")

# Test d'invariance des paramètres entre espèces
# fit_constrained <- sem(sem_multigroup_species, 
#                        data = data_sem, 
#                        group = "Species_f",
#                        group.equal = c("regressions"),
#                        estimator = "MLR")
# 
# anova(fit_multigroup, fit_constrained)  # Test si les effets diffèrent entre espèces

# ----- APPROCHE 3 : MODÈLE AVEC TREATMENT COMME MODÉRATEUR (MEILLEUR) -----

sem_with_interaction <- '
  Habitat_structure =~ Canopy_cover_z + Veg_cover_z + Tree_Picea + Tree_Abies
  Predator_pressure =~ Predator_visits_z + Predator_diversity_z
  
  # Variables d\'interaction (créer avant dans les données)
  Predator_pressure ~ Habitat_structure + Site_f
  
  Removal_prop ~ Predator_pressure + 
                 Habitat_structure + 
                 Treatment_binary +
                 Pred_x_Treatment +  # Interaction Predator_pressure × Treatment
                 Hab_x_Treatment +   # Interaction Habitat_structure × Treatment
                 Species_f +
                 Site_f
'

# Créer les interactions dans les données :
# data_sem <- data_sem %>%
#   mutate(
#     Pred_x_Treatment = Predator_pressure * Treatment_binary,
#     Hab_x_Treatment = Habitat_structure * Treatment_binary
#   )

# ============================================================================
# 4. ESTIMATION ET DIAGNOSTICS
# ============================================================================

estimate_and_diagnose <- function(model, data) {
  
  # Estimation
  fit <- sem(model, 
             data = data,
             estimator = "MLR",      # Robust ML (gère non-normalité)
             missing = "fiml",       # Gestion valeurs manquantes
             se = "robust",
             test = "bootstrap")     # Bootstrap pour IC robustes
  
  # Résumé
  cat("=" , rep("=", 70), "\n", sep = "")
  cat("RÉSUMÉ DU MODÈLE\n")
  cat("=" , rep("=", 70), "\n\n", sep = "")
  
  print(summary(fit, standardized = TRUE, fit.measures = TRUE, rsquare = TRUE))
  
  # Indices d'ajustement
  cat("\n\n", "=" , rep("=", 70), "\n", sep = "")
  cat("ÉVALUATION DE L'AJUSTEMENT\n")
  cat("=" , rep("=", 70), "\n\n", sep = "")
  
  fit_idx <- fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", 
                                "rmsea", "rmsea.ci.lower", "rmsea.ci.upper",
                                "srmr", "aic", "bic"))
  
  print(round(fit_idx, 3))
  
  # Interprétation
  cat("\nInterprétation :\n")
  cat("  CFI  :", round(fit_idx["cfi"], 3), 
      ifelse(fit_idx["cfi"] > 0.95, "✓ EXCELLENT", 
             ifelse(fit_idx["cfi"] > 0.90, "✓ Acceptable", "✗ À améliorer")), "\n")
  cat("  TLI  :", round(fit_idx["tli"], 3), 
      ifelse(fit_idx["tli"] > 0.95, "✓ EXCELLENT", 
             ifelse(fit_idx["tli"] > 0.90, "✓ Acceptable", "✗ À améliorer")), "\n")
  cat("  RMSEA:", round(fit_idx["rmsea"], 3), 
      ifelse(fit_idx["rmsea"] < 0.05, "✓ EXCELLENT", 
             ifelse(fit_idx["rmsea"] < 0.08, "✓ Acceptable", "✗ À améliorer")), "\n")
  cat("  SRMR :", round(fit_idx["srmr"], 3), 
      ifelse(fit_idx["srmr"] < 0.08, "✓ BON", "✗ À améliorer"), "\n\n")
  
  # Paramètres importants
  cat("\n", "=" , rep("=", 70), "\n", sep = "")
  cat("FACTEURS IMPORTANTS POUR REMOVAL\n")
  cat("=" , rep("=", 70), "\n\n", sep = "")
  
  params <- parameterEstimates(fit, standardized = TRUE) %>%
    filter(lhs == "Removal_prop", op == "~") %>%
    mutate(
      sig = case_when(
        pvalue < 0.001 ~ "***",
        pvalue < 0.01 ~ "**",
        pvalue < 0.05 ~ "*",
        pvalue < 0.10 ~ ".",
        TRUE ~ "ns"
      ),
      effect_size = case_when(
        abs(std.all) < 0.1 ~ "négligeable",
        abs(std.all) < 0.3 ~ "petit",
        abs(std.all) < 0.5 ~ "moyen",
        TRUE ~ "grand"
      )
    ) %>%
    arrange(desc(abs(std.all))) %>%
    select(Facteur = rhs, Beta = est, Beta_std = std.all, SE = se, 
           p_value = pvalue, Sig = sig, Taille = effect_size)
  
  print(params)
  
  # Visualisation
  p <- params %>%
    ggplot(aes(x = reorder(Facteur, Beta_std), y = Beta_std, fill = Beta_std > 0)) +
    geom_col() +
    geom_text(aes(label = Sig), hjust = -0.3, size = 5) +
    coord_flip() +
    labs(
      title = "Effets sur le removal des graines",
      subtitle = "Coefficients standardisés (β) avec significativité",
      x = NULL,
      y = "Effet standardisé (β)",
      caption = "*** p<0.001, ** p<0.01, * p<0.05, . p<0.10"
    ) +
    scale_fill_manual(values = c("#E74C3C", "#27AE60"), 
                      labels = c("Négatif", "Positif"),
                      name = "Direction") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom") +
    geom_hline(yintercept = 0, linetype = "dashed")
  
  print(p)
  
  # Diagramme du modèle
  semPaths(fit, 
           what = "std",
           whatLabels = "std",
           layout = "tree2",
           edge.label.cex = 0.7,
           sizeMan = 8,
           sizeLat = 10,
           fade = FALSE,
           residuals = FALSE,
           intercepts = FALSE,
           style = "lisrel",
           title = TRUE)
  
  return(fit)
}

# ============================================================================
# 5. RECOMMANDATIONS FINALES
# ============================================================================

cat("\n\n")
cat("╔════════════════════════════════════════════════════════════════════════╗\n")
cat("║                    RECOMMANDATIONS POUR VOTRE SEM                      ║\n")
cat("╚════════════════════════════════════════════════════════════════════════╝\n\n")

cat("📊 FORMAT DES VARIABLES (résumé) :\n")
cat("  ✓ Canopy cover        → Numérique (%) [STANDARDISER]\n")
cat("  ✓ Vegetation cover    → Numérique (milieu catégories) [STANDARDISER]\n")
cat("  ✓ Tree dominance      → Dummies (1 par espèce) OU multigroupe\n")
cat("  ✓ Predator visits     → Numérique (visites/jour) [STANDARDISER]\n")
cat("  ✓ Predator diversity  → Numérique (richesse) [STANDARDISER]\n")
cat("  ✓ Removal             → Proportion 0-1 [PAS standardiser]\n")
cat("  ✓ Treatment           → Binaire (0=Closed, 1=Open)\n")
cat("  ✓ Seed species        → Facteur pour multigroupe\n")
cat("  ✓ Site                → Facteur (6 niveaux)\n\n")

cat("🎯 STRATÉGIE D'ANALYSE RECOMMANDÉE :\n\n")

cat("ÉTAPE 1 : Modèle de base\n")
cat("  → SEM complet avec Treatment comme prédicteur\n")
cat("  → Variables latentes : Habitat_structure + Predator_pressure\n")
cat("  → Évaluer l'ajustement général\n\n")

cat("ÉTAPE 2 : Tester les modérateurs\n")
cat("  → Ajouter interactions Treatment × Predator_pressure\n")
cat("  → Comparer modèles avec/sans interactions\n\n")

cat("ÉTAPE 3 : Analyse par espèce de graine\n")
cat("  → Multigroupe SEM (Species_f comme grouping variable)\n")
cat("  → Tester si les effets sont invariants entre espèces\n")
cat("  → Si différents : rapporter séparément\n\n")

cat("ÉTAPE 4 : Effets aléatoires (si nécessaire)\n")
cat("  → Si forte variabilité entre Sites : considérer modèle multiniveau\n")
cat("  → Package 'lme4' ou 'nlme' pour mixed effects\n\n")

cat("⚠️  POINTS D'ATTENTION :\n")
cat("  • Predator visits : DOIT être standardisé par effort caméra\n")
cat("  • Removal : proportion (0-1) fonctionne bien avec MLR estimator\n")
cat("  • Tree dominance : mieux en multigroupe qu'en dummies si effet modérateur\n")
cat("  • N petit par groupe ? Éviter trop de groupes (préférer Treatment comme prédicteur)\n")
cat("  • Vérifier VIF pour multicolinéarité (Habitat_structure variables)\n\n")

cat("📈 CRITÈRES DE DÉCISION :\n")
cat("  Séparer par Treatment SI :\n")
cat("    → Hypothèse forte d'effets différents\n")
cat("    → N suffisant dans chaque groupe (>100)\n")
cat("    → Test d'interaction non significatif mais théoriquement attendu\n\n")
cat("  Multigroupe par Species SI :\n")
cat("    → Hypothèse de traits fonctionnels différents\n")
cat("    → Test d'invariance rejette contraintes\n")
cat("    → N suffisant par espèce (>50-100)\n\n")

cat("💡 MON CONSEIL : Commencer avec le modèle complet incluant Treatment\n")
cat("   comme prédicteur + multigroupe par Species. C'est le plus informatif !\n\n")



# =============================================================
# MOCK DATASET FOR SEM TESTING
# =============================================================

set.seed(42)  # reproducibility
n <- 300

# Sites and structure
sites <- c("BEA", "NEU", "CEL", "BET", "VOR", "NAT", "SCH")
species <- c("Abies alba", "Picea abies", "Fagus sylvatica")
treatments <- c("Open", "Closed")

mock_data <- data.frame(
  Site = sample(sites, n, replace = TRUE),
  Treatment = sample(treatments, n, replace = TRUE),
  Seed_species = sample(species, n, replace = TRUE)
)

# Canopy cover (numeric, %)
mock_data$Canopy_cover <- round(runif(n, 10, 95), 1)

# Veg_cover (ordinal converted to numeric midpoints)
veg_cat <- sample(1:5, n, replace = TRUE, prob = c(0.1, 0.25, 0.3, 0.25, 0.1))
mock_data$Veg_cover <- factor(veg_cat, levels = 1:5)
mock_data$Veg_cover_mid <- c(2.5, 15, 37.5, 62.5, 87.5)[veg_cat]

# Tree_dominance (categorical)
dominant_species <- c("Fagus sylvatica", "Abies alba", "Picea abies")
mock_data$Tree_dominance <- sample(dominant_species, n, replace = TRUE)

# Predator visits (visits/day; numeric)
# Simulate slightly higher visits with more vegetation
mock_data$Predator_visits <- round(rnorm(n, mean = 3 + 0.02 * mock_data$Veg_cover_mid, sd = 1.2), 2)
mock_data$Predator_visits[mock_data$Predator_visits < 0] <- 0  # no negative

# Predator diversity (species richness; numeric)
mock_data$Predator_diversity <- round(rnorm(n, mean = 5 + 0.01 * mock_data$Canopy_cover, sd = 1), 1)
mock_data$Predator_diversity[mock_data$Predator_diversity < 1] <- 1

# Removal (proportion 0–1; influenced by visits and vegetation)
mock_data$Removal <- plogis(
  -1 + 0.05 * mock_data$Predator_visits + 0.01 * mock_data$Veg_cover_mid - 0.02 * mock_data$Canopy_cover
)
mock_data$Removal <- round(mock_data$Removal, 3)

# Add a camera effort variable if you want to standardize visits
mock_data$Camera_days <- sample(4:7, n, replace = TRUE)
mock_data$Visits_per_day <- mock_data$Predator_visits / mock_data$Camera_days

# Convert factors
mock_data <- mock_data %>%
  dplyr::mutate(
    Site = factor(Site),
    Treatment = factor(Treatment),
    Seed_species = factor(Seed_species),
    Tree_dominance = factor(Tree_dominance)
  )

# Check structure
str(mock_data)
head(mock_data)


# =============================================================
# 2. SEM MODEL SPECIFICATION
# =============================================================

model_sem <- '
  # Latent variables
  Habitat =~ Canopy_cover + Veg_cover_mid
  Predator_activity =~ Predator_visits + Predator_diversity
  
  # Structural paths
  Predator_activity ~ Habitat
  Removal ~ Predator_activity + Habitat
  
  # (Optional) Correlation between latent variables
  Habitat ~~ Predator_activity
'
fit_sem <- sem(model_sem, data = mock_data, estimator = "MLR")
summary(fit_sem, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


install.packages("lavaanPlot")
library(lavaanPlot)

lavaanPlot(model = fit_sem, coefs = TRUE, stand = TRUE, sig = 0.05)



# -----------------------
# 2) MODÈLE DE BASE (latents + Treatment comme prédicteur observé)
# -----------------------
model_base <- '
  # variables latentes
  Habitat =~ Canopy_cover + Veg_cover_mid
  Predator_activity =~ Visits_per_day + Predator_diversity

  # relations structurelles
  Predator_activity ~ Habitat
  Removal ~ Predator_activity + Habitat + Treatment

  # covariance latente (utile si corrélées)
  Habitat ~~ Predator_activity
'

# Ajuster modèle MULTIGROUPE (par espèce de graine)
fit_base_mg <- sem(model_base, data = mock_data, group = "Seed_species", estimator = "MLR")
cat("=== Résumé du modèle multigroupe (configural) ===\n")
summary(fit_base_mg, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# -----------------------
# 3) TEST D'INVARIANCE (chargements égaux = metric invariance)
#    - comparer configural vs metric (group.equal = "loadings")
# -----------------------
fit_metric_mg <- sem(model_base, data = df, group = "Seed_species",
                     group.equal = c("loadings"), estimator = "MLR")

cat("\n=== Comparaison configural vs metric (anova) ===\n")
print(anova(fit_base_mg, fit_metric_mg))   # test de rapport de vraisemblance robuste

# Interprétation : si test significatif -> les chargements diffèrent entre groupes

# -----------------------
# 4) INTERACTIONS : Treatment × Predator_pressure
#    Approche pratique : estimer d'abord le score de la latente Predator_activity,
#    puis créer un terme d'interaction observé et ré-estimer un modèle "observed"
#    (lavaan ne gère pas trivialement interaction latent × observed sans XWITH).
# -----------------------
# 4a) Estimer un modèle en pool (single-group) pour extraire scores latents
fit_single_for_scores <- sem(model_base, data = df, estimator = "MLR")
# facteur score (predator_activity) : lavPredict(type = "lv")
lv_scores <- lavPredict(fit_single_for_scores, type = "lv")
df$Predator_activity_score <- as.numeric(lv_scores[,"Predator_activity"])

# Créer l'interaction observée
df$PredAct_x_Treat <- df$Predator_activity_score * df$Treatment_num

# 4b) Nouveau modèle utilisant le score observé (Predator_activity_score) + interaction
#     Ici Predator activity est maintenant un indicateur observé pour l'interaction.
model_inter <- '
  Habitat =~ Canopy_cover + Veg_cover_mid

  # Removal expliqué par score observé, habitat, treatment et interaction
  Removal ~ Predator_activity_score + Habitat + Treatment_num + PredAct_x_Treat
'

fit_inter <- sem(model_inter, data = df, estimator = "MLR")
cat("\n=== Résumé modèle avec interaction (observée via score latent) ===\n")
summary(fit_inter, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Interpréter : coefficient de PredAct_x_Treat indique modulation (interaction).
# Si significatif -> l'effet de Predator_activity diffère selon Treatment.

# -----------------------
# 5) MULTIGROUPE AVEC L'INTERACTION
#    Option : après avoir calculé le score (df$Predator_activity_score), on peut
#    estimer le modèle Observed en multigroupe (group = "Seed_species")
# -----------------------
fit_inter_mg <- sem(model_inter, data = df, group = "Seed_species", estimator = "MLR")
cat("\n=== Résumé modèle OBSERVED (interaction) en multigroupe ===\n")
summary(fit_inter_mg, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# -----------------------
# 6) VÉRIFICATIONS (taille des groupes, VIF, distribution)
# -----------------------
cat("\n=== Taille des groupes (par espèce) ===\n")
print(table(df$Seed_species))

# Vérifier corrélations rapides
cat("\n=== Corrélations (variables principales) ===\n")
print(round(cor(df %>% dplyr::select(Canopy_cover, Veg_cover_mid, Visits_per_day, Predator_diversity, Removal)), 2))