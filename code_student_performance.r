library(MASS)
library(jtools)
library(patchwork)
library(huxtable)
library(broom)
library(performance)
library(parameters)
library(datawizard)
library(see)
library(effectsize)
library(insight)
library(correlation)
library(modelbased)
library(glue)
library(scales)
library(GGally)
library(ggpubr)
library(car)
library(lmtest)
library(rstatix)
library(matrixTests)
library(ggfortify)
library(qqplotr)
library(collapse)
library(janitor)
library(kableExtra)
library(tidyverse)

set.seed(42)
rm(list = ls())
# Global options
options(pillar.width = 130, width = 200, scipen = 999, digits = 5)


# --- 1. Data Loading and Initial Inspection ---
data_raw <- read_csv("project.csv")
glimpse(data_raw)

# Checking structure, missing values, and distinct counts
namlab(data_raw, N = TRUE, Ndistinct = TRUE, class = TRUE)


# --- 2. Data Cleaning and Factor Conversion ---
data_clean <- data_raw |>
  mutate(
    # Nominal variables
    sexe = factor(sexe, levels = 1:3, labels = c("Female", "Male", "Other")),
    school_type = factor(school_type, levels = 1:2, labels = c("Public", "Private")),
    web_access = factor(web_access, levels = 1:2, labels = c("No", "Yes")),
    extra_act = factor(extra_act, levels = 1:2, labels = c("No", "Yes")),
    study_method = factor(study_method, levels = 1:6, 
                          labels = c("Online Videos", "Coaching", "Notes", "Textbook", "Group Study", "Mixed")),
    
    # Ordinal variables (Ordered categories)
    agecat = factor(agecat, levels = 1:5, 
                    labels = c("[14.0, 15.0]", "[15.1, 16.1]", "[16.1, 17.1]", "[17.1, 18.1]", "[18.1, 19.0]")),
    
    parent_educ = factor(parent_educ, levels = 1:6, 
                         labels = c("No Formal", "High School", "Graduate", "Post Graduate 1", "Post Graduate 2", "PhD")),
    
    sleep_qual = factor(sleep_qual, levels = 1:3, labels = c("Poor", "Average", "Good")),
    
    attend_pct_cat = factor(attend_pct_cat, levels = 1:4, 
                            labels = c("[50, 62[", "[62, 72[", "[72, 81[", "[81, 100[")),
    
    trav_time = factor(trav_time, levels = 1:4, 
                       labels = c("<15 Min","15-30 Min" ,"30-60 Min", ">60 Min"))
  )

# --- 3. Variable Labeling ---
data_clean <- data_clean |>
  relabel(
    id = "ID", 
    y = "Grade", 
    age = "Age", 
    agecat = "Age Category", 
    sexe = "Gender", 
    school_type = "School Type", 
    parent_educ = "Parental Education", 
    study_hrs = "Weekly Study Hours", 
    sleep_hrs = "Sleep Duration (hours)", 
    sleep_qual = "Sleep Quality", 
    attend_pct = "School Attendance (%)", 
    attend_pct_cat = "Categorical School Attendance (%)", 
    web_access = "Web Access", 
    trav_time = "Commute Time", 
    extra_act = "Extracurricular Activities",
    study_method = "Study Method"
  )

# --- 4. Final Verification ---
glimpse(data_clean)
namlab(data_clean, N = TRUE, Ndistinct = TRUE, class = TRUE)

# --- 5. Basic Data Inspection ---

# Sample Size and Dimensions
print(paste("Number of rows:", nrow(data_raw)))
print(paste("Number of columns:", ncol(data_raw)))

# Variable Names
print(names(data_raw))

# Variable Types
# We use a named vector for a cleaner output in the console
sapply(data_clean, class)

# --- 6. Factor Levels & Reference Management ---

# Displaying all levels for categorical variables
data_clean |>
  dplyr::select(where(is.factor)) |>
  map_dfr(~ tibble(Levels = paste(levels(.x), collapse = ", ")), .id = "Variable") |>
  knitr::kable(caption = "Overview of Factor Levels")

# Setting Reference Levels (Baselines)
# Strategy:
# 1. Nominal variables: Set the most frequent category as reference (for stability).
# 2. Ordinal variables (e.g., Age, Education): Keep the natural lowest value as reference.

data_clean <- data_clean |>
  mutate(
    # Only applying frequency reordering to Nominal variables
    sexe = fct_infreq(sexe),
    school_type = fct_infreq(school_type),
    web_access = fct_infreq(web_access),
    extra_act = fct_infreq(extra_act),
    study_method = fct_infreq(study_method)
    # Note: agecat, parent_educ, etc. are NOT touched here to preserve order
  )

# Verify the final reference levels (first level displayed = reference)
data_clean |>
  dplyr::select(where(is.factor)) |>
  map_dfr(~ tibble(Reference_Category = levels(.x)[1]), .id = "Variable") |>
  knitr::kable(caption = "Selected Reference Level for Each Factor")


# --- 7. Data Integrity & Cleaning ---

# Checking for issues (Pre-cleaning assessment)
# We count how many rows are duplicated based on ID
nb_double <- sum(duplicated(data_clean$id))

# We count how many rows contain at least one NA
nb_na_rows <- sum(!complete.cases(data_clean))

print(paste("Duplicate IDs found:", nb_double))
print(paste("Rows with missing values:", nb_na_rows))

# Applying the Cleaning Pipeline
n_init <- nrow(data_clean)

# A. Remove Duplicates
data_clean <- data_clean |> distinct()
n_after_dedup <- nrow(data_clean)
n_duplicates <- n_init - n_after_dedup

# B. Remove Missing Values
data_clean <- data_clean |> drop_na()
n_final <- nrow(data_clean)
n_na_removed <- n_after_dedup - n_final

# Summary of cleaning
n_total_removed <- n_init - n_final
pct_removed <- round((n_total_removed / n_init) * 100, 1)

print(paste0("Cleaning Complete. Removed ", n_total_removed, " observations (", pct_removed, "%)."))


# --- 8. Plausibility Check (Range Verification) ---
# We check the minimum and maximum values to ensure data coherence.

data_clean |>
  dplyr::select(where(is.numeric)) |>
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") |>
  group_by(Variable) |>
  summarise(
    Minimum = min(Value, na.rm = TRUE),
    Maximum = max(Value, na.rm = TRUE)
  ) |>
  knitr::kable(caption = "Range Verification (Min-Max)")


# --- 9. Visual Outlier Detection (Boxplots) ---
vars_num <- c("y", "age", "study_hrs", "sleep_hrs", "attend_pct")

data_clean |> 
  dplyr::select(all_of(vars_num)) |> 
  pivot_longer(everything(), names_to = "var", values_to = "value") |> 
  ggplot(aes(x = var, y = value)) +
  facet_wrap(~ var, scales = "free", ncol = 3) + 
  
  # Boxplot aesthetics
  geom_boxplot(fill = "orange", outlier.colour = "red", outlier.size = 2, alpha = 0.6) + 
  
  theme_bw(base_size = 12) +
  labs(title = "Outlier Detection (Boxplots)", x = "", y = "Values") +
  
  # Cleaning: Remove x-axis labels as they are already in the facet titles
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


# --- 10. Outlier Detection ---
outliers_table <- data_clean |>
  dplyr::select(id, all_of(vars_num)) |>
  pivot_longer(-id, names_to = "variable", values_to = "value") |>
  group_by(variable) |>
  rstatix::identify_outliers(value)

# printing a table
outliers_table |>
  knitr::kable(caption = "Summary of detected outlier")|>
  kableExtra::collapse_rows(columns = 1, valign = "top")

# --- 11. Redundancy Analysis (Feature Selection) ---
# We determine whether to keep the continuous or categorical representations
# for Age, Sleep, and Attendance based on linearity and information loss.

# A. Linearity & Redundancy Check (Visual Inspection)

# 1. Age (Continuous vs Categorical)
p1 <- ggplot(data_clean, aes(x = agecat, y = age)) +
  geom_boxplot(linewidth = 0.25) +
  # Correction des labels
  labs(y = "Age (Continuous)", x = "Age Category") +
  theme_bw(base_size = 14)

# 2. Sleep (Quantity vs Quality)
p2 <- ggplot(data_clean, aes(x = sleep_qual, y = sleep_hrs)) +
  geom_boxplot(linewidth = 0.25) +
  labs(y = "Sleep Quantity (Hours)", x = "Sleep Quality (Rating)") +
  theme_bw(base_size = 14)

# 3. Attendance (Percent vs Category)
p3 <- ggplot(data_clean, aes(x = attend_pct_cat, y = attend_pct)) +
  geom_boxplot(linewidth = 0.25) +
  # Correction des labels
  labs(y = "Attendance % (Continuous)", x = "Attendance Category") +
  theme_bw(base_size = 14)

# Arrange plots
ggpubr::ggarrange(p1, p2, p3, ncol = 3, nrow = 1)

# --- Observation 1: Age & Attendance ---
# The boxplots show a clear linear progression: as the category increases, 
# the continuous value increases strictly. 
# converting these variables into categories (binning) would result in 
# 'Information Loss' (discretization bias) without adding non-linear value.
# DECISION: We retain the CONTINUOUS versions (age, attend_pct) to preserve variance.

#--- Observation 2: Sleep hours & Sleep Quality ---
#Visual Inspection: Boxplots showed significant overlap, suggesting that 
#  sleep duration (hrs) is not a perfect proxy for sleep quality (rating) we hence keep them both 

# --- 12. Final Dataset Creation ---
# Conclusion: We remove the redundant categorical variables to minimize multicollinearity.

data_final <- data_clean |>
  dplyr::select(-agecat, -attend_pct_cat)

# Final structure check
namlab(data_final, N = TRUE, Ndistinct = TRUE, class = TRUE)


## --------------------Exploratory data analysis ----------------------------

idx <- sample(seq_len(nrow(data_final)), size = floor(0.8*nrow(data_final)))
data_train <- data_final[idx, ]
data_test  <- data_final[-idx, ]


#---------------------1. Descriptive statistics for quantitatives variables ------------------------
dplyr::select(data_train, y, age, study_hrs, sleep_hrs, attend_pct) |>
  get_summary_stats(show = c("n", "mean", "sd", "median", "q1", "q3", "min", "max", "iqr"))|>
  knitr::kable(
    caption = "Statistics",
    align = "llcc")

#--------------------2.frequency tables (counts and proportions) for categorical variables----------



vars_cat <- c("sexe", "school_type", "parent_educ", "web_access", 
              "trav_time", "extra_act", "study_method")

# Helper function to generate summary stats for a single variable
make_summary <- function(var_name) {
  data_train |>
    tabyl(!!sym(var_name)) |>              # Create frequency table
    adorn_pct_formatting(digits = 2) |>    # Format percentages
    arrange(desc(n)) |>                    # Sort by frequency
    rename(Category = 1) |>                # Standardize column name
    mutate(Variable = var_name) |>         # Add variable name for grouping
    dplyr::select(Variable, Category, n, percent)
}

# Apply function to all variables and display one consolidated table
map_dfr(vars_cat, make_summary) |>   
  knitr::kable(
    caption = "Distribution of Categorical Variables (Counts & Proportions)",
    align = "llcc"
  ) |>
  kableExtra::collapse_rows(columns = 1, valign = "top") # Group rows by Variable


#-------------3.Visual Exploration ------------------------------

# A. Calculate Distribution Metrics
s_y <- skewness(data_train$y, na.rm = TRUE)
k_y <- kurtosis(data_train$y, na.rm = TRUE)

# B. Histogram (Distribution Shape)
p_hist <- ggplot(data_train, aes(x = y)) + 
  geom_histogram(bins = 15, color = "black", fill = "firebrick", alpha = 0.8) +
  theme_bw(base_size = 12) +
  labs(
    title = "Distribution of Grades",
    subtitle = paste0("Skewness: ", round(s_y, 2), " | Kurtosis: ", round(k_y, 2)),
    x = "Grade (y)", y = "Frequency"
  )

# C. Boxplot 
p_box <- ggplot(data_train, aes(y = y)) + # Map y to y-axis for vertical boxplot
  geom_boxplot(fill = "orange", outlier.colour = "red", outlier.size = 2, alpha = 0.6) +
  theme_bw(base_size = 12) +
  labs(title = "Boxplot of Grades", x = "", y = "Grade (y)") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Display plots side-by-side
ggarrange(p_hist, p_box, ncol = 2, nrow = 1)
  
#--------------------4 testing y against quantitative predictors -------------

dplyr::select(data_train, y, age, study_hrs, sleep_hrs, attend_pct) |>
  pivot_longer(cols = c(age, study_hrs, sleep_hrs, attend_pct)) |>
  ggplot(aes(x = value, y = y)) +
  
  # Using 2 columns fits better on a standard A4 page
  facet_wrap(vars(name), ncol = 2, scales = "free_x") + 
  
  # Plot aesthetics
  geom_point(size = 2, shape = 21, fill = "red", color = "black", alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  
  theme_bw(base_size = 12) +
  labs(
    title = "Bivariate Analysis: Quantitative Predictors vs. Target",
    x = "Predictor Value", 
    y = "Grade (y)"
  )

# ------------------------------------------------------------------------------
# INTERPRETATION:
# 1. Linearity: Visual inspection suggests a general linear relationship 
#    between these predictors and the final grade, supporting the use of OLS.
#
# 2. Variance (Homoscedasticity):
#    - Age & Attendance: Variance appears relatively constant (Homoscedastic).
#    - Sleep & Study Hours: We observe signs of Heteroscedasticity (a "megaphone" 
#      shape), where variance increases as values increase. 
# ------------------------------------------------------------------------------
#--------------------5 testing y against categorical predictors ----------------------------

dplyr::select(data_train, y, sexe, sleep_qual,school_type, parent_educ, 
              web_access, trav_time, sleep_qual,extra_act, study_method) |>
  pivot_longer(cols = -y) |> # Pivot all columns except 'y'
  ggplot(aes(x = value, y = y)) +
  
  # Layout: 3 columns fits well for 7 variables
  facet_wrap(vars(name), scales = "free_x", ncol = 3) + 
  
  # Aesthetics
  geom_boxplot(fill = "lightblue", alpha = 0.7, outlier.colour = "red", size = 0.3) +
  
  labs(
    title = "Impact of Categorical Factors on Grades",
    x = "Category", 
    y = "Grade (y)"
  ) +
  theme_bw(base_size = 12) +
  # Rotate x-axis labels to prevent overlapping
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------------------------------------------------------------------
# INTERPRETATION:
# 1. Level Differences: We look for shifts in the median line (the bar inside 
#    the box) between categories. If one box is clearly higher than another, 
#    that factor likely influences the grade.
#
# 2. Spread/Variance: The height of the box (IQR) shows the variability of grades
#    within that group.
#
# 3. Outliers: Red dots indicate students with grades exceptionally far from 
#    their group's average.
# ------------------------------------------------------------------------------
#-----------------------6 summarizing assocation -----------------------------------
# ------------------------------------------------------------------------------
# 6.1 Continuous Variables (Pearson Correlation)
# ------------------------------------------------------------------------------

# Calculate and display correlation matrix
data_train |>
  dplyr::select(age, sleep_hrs, study_hrs, attend_pct) |>
  correlation(method = "pearson", p_adjust = "none") |>
  kbl(caption = "Pearson Correlation Matrix (Continuous Variables)") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# INTERPRETATION:
# We detect moderate correlations between age/sleep_hrs and sleep_hrs/study_hrs.
# While not high enough to cause severe multicollinearity (e.g., r > 0.8), 
# we should remain aware of them during modeling.

# ------------------------------------------------------------------------------
# 6.2 Categorical Variables (Interaction Screening via AIC)
# ------------------------------------------------------------------------------
# Strategy: We systematically test all pairwise interactions between categorical 
# variables to see if any significantly improve the model fit (Delta AIC).

test_cat_interactions <- function(vars, target, data){
  combinaisons <- combn(vars, 2, simplify = FALSE)
  results <- data.frame()
  
  for(pair in combinaisons) {
    v1 <- pair[1]
    v2 <- pair[2]
    
    # Define formulas
    f_add <- as.formula(paste(target, "~", v1, "+", v2))
    f_int <- as.formula(paste(target, "~", v1, "*", v2))
    
    # Fit models
    m_add <- lm(f_add, data = data)
    m_int <- lm(f_int, data = data)
    
    # Calculate Delta AIC (Positive Delta = Interaction is better)
    delta <- AIC(m_add) - AIC(m_int) 
    
    results <- rbind(results, data.frame(
      Pair = paste(v1, "*", v2),
      AIC_Add = AIC(m_add),
      AIC_Int = AIC(m_int),
      Delta_AIC = delta))
  }
  
  # Return sorted results
  results |> arrange(desc(Delta_AIC))
}

# Run the screen
cat_vars <- c("sexe", "parent_educ", "school_type", "sleep_qual", 
              "web_access", "trav_time", "extra_act", "study_method")

interaction_results <- test_cat_interactions(cat_vars, "y", data_train)

# Display top 5 results
interaction_results |> 
  head(5) |>
  kable(caption = "Top 5 Categorical Interactions (Screening)", digits = 2)

# CONCLUSION: 
# Looking at Delta AIC, no interaction shows a strong improvement (Deltas are 
# likely small or negative). We proceed without categorical-categorical interactions.

# ------------------------------------------------------------------------------
# 6.3 Mixed Predictors (Visual Screening for Slopes)
# ------------------------------------------------------------------------------
# We visually screen for interactions by looking for "crossing lines" or 
# significantly different slopes in the scatterplots.

# Plot 1: Sleep Hours vs Grade, colored by Sleep Quality
s1 <- ggplot(data_train, aes(x = sleep_hrs, y = y, color = sleep_qual)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Sleep Hours", y = "Grade", color = "Sleep Qual") +
  theme_bw(base_size = 11) + theme(legend.position = "bottom")

# Plot 2: Sleep Hours vs Grade, colored by Sex
s2 <- ggplot(data_train, aes(x = sleep_hrs, y = y, color = sexe)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Sleep Hours", y = "Grade", color = "Sex") +
  theme_bw(base_size = 11) + theme(legend.position = "bottom")

# Plot 3: Age vs Grade, colored by Study Method (CORRECTED LABELS)
s3 <- ggplot(data_train, aes(x = age, y = y, color = study_method)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Age (Years)", y = "Grade", color = "Method") + # Fixed Label
  theme_bw(base_size = 11) + theme(legend.position = "bottom")

# Plot 4: Study Hours vs Grade, colored by Sleep Quality (CORRECTED LABELS)
s4 <- ggplot(data_train, aes(x = study_hrs, y = y, color = sleep_qual)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Study Hours", y = "Grade", color = "Sleep Qual") + # Fixed Label
  theme_bw(base_size = 11) + theme(legend.position = "bottom")

# Arrange in a grid
ggarrange(s1, s2, s3, s4, ncol = 4, nrow = 1)

# INTERPRETATION:
# We visually screened mixed-type predictors. We display here the scatterplots 
# where we inspected for diverging regression slopes. Parallel lines suggest 
# no interaction, while intersecting lines (like in s4) suggest a potential 
# moderating effect to be tested in the modeling phase.

#-------------4. Ordinal Predictor  ------------------------------

# Attend_pct, Sleep_hrs, Study_hrs, Parental education, Sleep quality, 
# Extracurricular Activities, School Type, Web Access, Commute Time: increasing trend 

# Others: No trend

#-----------5. Conclusion -------------------
#The distribution of final grades is approximately normal [ou slightly skewed], 
#showing no extreme outliers that would require robust regression methods. 
#No transformation of the target variable (like log-transform) appears necessary.

#Looking at the data y vs numeric variables we conclude that given the slope of the age we do not keep it as covariate 
#Looking at categorical variables we keep evrything except the gender  
#looking at associations between categorical and categorical we do not keep any problmes of complexity for no gane 
# for numeric and categorical cf scatter plot we keep study_hrs and sleep_qual, age and study_method, sleep_hrs and sleep_quality and sleep_hrs and sexe


#-------------------- Building regression models


#---------------------1) Builiding intuition -----------
mod_age <- lm( y ~ age, data=data_train)
summary(mod_age)

mod_sexe <- lm(y ~ sexe, data=data_train)
summary(mod_sexe)

mod_school_type <-  lm(y ~ school_type, data=data_train)
summary(mod_school_type)

mod_parent_educ <-  lm(y ~ parent_educ, data=data_train)
summary(mod_parent_educ)

mod_study_hrs <-  lm(y ~ study_hrs, data=data_train)
summary(mod_study_hrs)

mod_sleep_hrs <- lm(y ~ sleep_hrs, data=data_train)
summary(mod_sleep_hrs)

mod_sleep_qual <- lm(y ~ sleep_qual, data=data_train)
summary(mod_sleep_qual)

mod_attend_pct <- lm(y ~ attend_pct, data=data_train)
summary(mod_attend_pct)

mod_web_access <- lm(y ~ web_access, data=data_train)
summary(mod_web_access)

mod_trav_time <- lm(y ~ trav_time, data=data_train)
summary(mod_trav_time)

mod_extra_act <-  lm(y ~ extra_act, data=data_train)
summary(mod_extra_act)

mod_study_method <-  lm(y ~ study_method, data=data_train)
summary(mod_study_method)

export_summs(mod_age, mod_study_hrs, mod_extra_act,
             model.names = c("Age Only", "Study Hrs Only", "Extra Act Only"),
             statistics = c("N" = "nobs", "R²" = "r.squared"),
             digits = 3)

#---------------------2) Builiding intuition -----------
mod_null <- lm(y~1 ,data= data_train)
mod_forw <- stepAIC(mod_null, y ~ age + study_hrs + sleep_hrs + attend_pct + sexe + school_type +
                      parent_educ + sleep_qual + web_access + trav_time + extra_act + study_method
                    , data= data_train, trace=T, direction=c('forward'))

mod_full <- lm(y~., data = data_train)
summary(mod_full)
# This confirms previous speculation we don't need age and sexe 
# forward backwar and both gives us the same model 

# ------------3 adding interaction
#adding interaction to mod_forw

mod_inter <- lm(y ~  study_hrs + sleep_hrs + attend_pct  + school_type +
                parent_educ + sleep_qual + web_access + trav_time + extra_act + study_method + sleep_qual:sleep_hrs+ age:study_method + sexe:sleep_hrs+ study_hrs:sleep_qual , data = data_train)
summary(mod_inter)

#Keeping only logical intercation given sumary mod inter

mod_inter_modified <- lm(y ~ study_hrs + sleep_hrs + attend_pct  + school_type +
                  parent_educ + sleep_qual + web_access + trav_time + extra_act + study_method + sleep_qual:sleep_hrs+ study_hrs:sleep_qual , data = data_train)

summary(mod_inter_modified)

anova( mod_inter, mod_inter_modified)


# 1. Fonction pour calculer les métriques proprement
get_model_metrics <- function(model, model_name, data_train, data_test) {
  
  # A. Prédictions (Sans data leakage : on applique le modèle sur le test)
  pred_train <- predict(model, newdata = data_train)
  pred_test  <- predict(model, newdata = data_test)
  
  # B. Récupération des valeurs réelles
  y_train <- data_train$y
  y_test  <- data_test$y
  
  # C. Calcul des métriques
  # MSE (Mean Squared Error)
  mse_train <- mean((y_train - pred_train)^2)
  mse_test  <- mean((y_test - pred_test)^2)
  
  # RMSE (Root Mean Squared Error) - Plus facile à interpréter (même unité que la note)
  rmse_train <- sqrt(mse_train)
  rmse_test  <- sqrt(mse_test)
  
  # AIC & R2 Adj (Propriétés intrinsèques du modèle sur le Train)
  aic_val <- AIC(model)
  r2_adj  <- summary(model)$adj.r.squared
  
  # D. Retour sous forme de ligne de tableau
  return(data.frame(
    Modele       = model_name,
    AIC          = aic_val,
    R2_Adj_Train = r2_adj,
    MSE_Train    = mse_train,
    RMSE_Train   = rmse_train,
    MSE_Test     = mse_test,
    RMSE_Test    = rmse_test
  ))
}

# 2. On compile les résultats pour tes 4 modèles
# Assure-toi que tes objets mod_forw, mod_full, etc. existent bien
tableau_comparatif <- rbind(
  get_model_metrics(mod_forw, "Stepwise Forward", data_train, data_test),
  get_model_metrics(mod_full, "Full Model", data_train, data_test),
  get_model_metrics(mod_inter, "Interactions (Complex)", data_train, data_test),
  get_model_metrics(mod_inter_modified, "Interactions (Optimized)", data_train, data_test)
)

# 3. Affichage du tableau esthétique
tableau_comparatif |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |> # Arrondi
  kable(
    caption = "Comparaison de la Performance des Modèles (Train vs Test)",
    col.names = c("Modèle", "AIC", "R² Adj (Train)", 
                  "MSE (Train)", "RMSE (Train)", 
                  "MSE (Test)", "RMSE (Test)"),
    align = "lcccccc"
  ) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) |>
  
  # Mise en valeur de la colonne RMSE Test (Le juge de paix)
  column_spec(7, bold = TRUE, background = "#e6f2ff", border_left = TRUE) |>
  
  # Ajout d'une barre de défilement si besoin
  scroll_box(width = "100%")


mod_final <- mod_inter_modified

##########################
#------------------------- 4) Diagnostics, assumptions, and robustness---------------------------


# Residuals vs Fitted 

autoplot(mod_final, which = 1, ncol = 1, colour = "grey", shape = 21, size = 2) +
  theme_bw(base_size = 14) +
  labs_pubr()

# <clezrly notice no trend and a line close to 0 

# Residuals vs Predictors (Numerical ones)

res_df <- augment(mod_final) %>%
  mutate(resid = .resid, fitted = .fitted)

p_res1 <- ggplot(res_df, aes(x = study_hrs, y = resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(shape = 21, size = 2, colour = "grey") +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Study hours", y = "Residuals", title = "Residuals vs Study hours") +
  theme_bw(base_size = 14) +
  ggpubr::theme_pubr()

p_res2 <- ggplot(res_df, aes(x = sleep_hrs, y = resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(shape = 21, size = 2, colour = "grey") +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Study hours", y = "Residuals", title = "Residuals vs Study hours") +
  theme_bw(base_size = 14) +
  ggpubr::theme_pubr()

p_res3 <- ggplot(res_df, aes(x = attend_pct, y = resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(shape = 21, size = 2, colour = "grey") +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Study hours", y = "Residuals", title = "Residuals vs Study hours") +
  theme_bw(base_size = 14) +
  ggpubr::theme_pubr()

p_res1 + p_res2 + p_res3

# <clezrly notice no trend and a line close to 0 

# Residuals vs Predictors (Categorical ones)

p4 <- ggplot(res_df, aes(x = school_type, y = resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_boxplot() +
  labs(x = "School type", y = "Residuals", title = "Residuals vs School type") +
  theme_bw(base_size = 14) +
  ggpubr::theme_pubr()

p5 <- ggplot(res_df, aes(x = parent_educ, y = resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_boxplot() +
  labs(x = "School type", y = "Residuals", title = "Residuals vs School type") +
  theme_bw(base_size = 14) +
  ggpubr::theme_pubr()

p6 <- ggplot(res_df, aes(x = sleep_qual, y = resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_boxplot() +
  labs(x = "School type", y = "Residuals", title = "Residuals vs School type") +
  theme_bw(base_size = 14) +
  ggpubr::theme_pubr()

p7 <- ggplot(res_df, aes(x = web_access, y = resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_boxplot() +
  labs(x = "School type", y = "Residuals", title = "Residuals vs School type") +
  theme_bw(base_size = 14) +
  ggpubr::theme_pubr()

p8 <- ggplot(res_df, aes(x = trav_time, y = resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_boxplot() +
  labs(x = "School type", y = "Residuals", title = "Residuals vs School type") +
  theme_bw(base_size = 14) +
  ggpubr::theme_pubr()

p9 <- ggplot(res_df, aes(x = extra_act, y = resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_boxplot() +
  labs(x = "School type", y = "Residuals", title = "Residuals vs School type") +
  theme_bw(base_size = 14) +
  ggpubr::theme_pubr()

p10 <- ggplot(res_df, aes(x = study_method, y = resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_boxplot() +
  labs(x = "School type", y = "Residuals", title = "Residuals vs School type") +
  theme_bw(base_size = 14) +
  ggpubr::theme_pubr()

(p4 + p5) / (p6 + p7) / (p8 + p9) / p10

# <clezrly notice no trend and a line close to 0 



# Residuals vs Ommitted variables (Age + Sexe)

res_df2 <- data_train |>
  mutate(resid = resid(mod_final))

p11 <- ggplot(res_df2, aes(x = age, y = resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_point(shape = 21, size = 2, colour = "grey") +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Age (omitted)", y = "Residuals", title = "Residuals vs Age (omitted)") +
  theme_bw(base_size = 14) +
  ggpubr::theme_pubr()

p12 <- ggplot(res_df2, aes(x = sexe, y = resid)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_boxplot() +
  labs(x = "Sexe (omitted)", y = "Residuals", title = "Residuals vs Sexe (omitted)") +
  theme_bw(base_size = 14) +
  ggpubr::theme_pubr()

p11 + p12

# Homoskedastic reisudals: Scale-Location Plot + Breusch-Pagan 

autoplot(mod_final, which = 3, ncol = 1, colour = "grey", shape = 21, size = 2) +
  theme_bw(base_size = 14) +
  labs_pubr()

ncvTest(mod_final)

# Scale location close to one and no visible trend 


# Uncorrelated Residuals: Durbin-Watson

durbinWatsonTest(mod_final)

#no correlation 

# Normality of Residuals: QQ-plot + Shapiro 

autoplot(mod_final,2)

shapiro.test(residuals(mod_final))

#residuals are gaussians 

# Outlier/High Leverage: tout est bon !
influenceIndexPlot(mod_final)
outlierTest(mod_final)


# L’analyse diagnostique fondée sur les résidus studentisés, la distance de Cook et le leverage ne met 
# pas en évidence d’observations exerçant une influence excessive sur les estimations du modèle. 
# Des analyses de sensibilité, réalisées en excluant les observations les plus extrêmes, conduisent à des 
# estimations et des conclusions très similaires (Cook Distance). Les principaux résultats apparaissent donc robustes.



#----------------------------------- 5) Interpretation and inference---------------------

# model equation -> see qmd

# Coefficient table 

model_parameters(mod_final, ci_method = "residual", digits = 1)


# Scenario based interpretation 

new_students <- data.frame(
  id = c(5001, 5002),
  age = c(15, 18),
  sexe = c("Male","Other"),
  study_hrs = c(7, 14),
  sleep_hrs = c(7, 9),
  attend_pct = c(66, 93),
  school_type = c("Public", "Private"),
  parent_educ = c("High School", "Post Graduate 2"),
  sleep_qual = c("Average", "Good"),
  web_access = c("Yes","Yes"),
  trav_time = c(">60 Min", "<15 Min"),
  extra_act = c("No", "Yes"),
  study_method = c("Notes", "Mixed")
)


preds <- predict(mod_final, newdata = new_students  , interval = "confidence")
  

  
print(preds)

student_1 <- data_clean %>%
  filter(id == 1)

student_1

pred_student <- predict(mod_final, student_1, interval = "confidence")
pred_student


# Calibrating plot 

predicted_values <- predict(mod_final, newdata = data_test)

plot_data <- data.frame(
  pred_val = predicted_values,
  obs_val = data_test$y
)

ggplot(plot_data, aes(x = pred_val, y = obs_val)) +
  # Add the points
  geom_point(alpha = 0.5, color = "steelblue", size = 2) +
  
  # Add the perfect prediction line (y = x)
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  
  # Add labels and theme
  labs(
    title = "Calibration Check: Predicted vs. Observed",
    subtitle = "Points on the red dashed line represent perfect predictions",
    x = "Predicted Grade",
    y = "Observed Grade"
  ) +
  theme_minimal() +
  coord_fixed()
