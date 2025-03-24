library(dplyr)
library(tidyr)
library(readr)
library(lme4) # for LMM
library(tibble)


# Load data
# Read all four files
denmark_food <- read_csv("Food Production/denmark_prod_clean.csv")
finland_food <- read_csv("Food Production/finland_prod_clean.csv")
sweden_food <- read_csv("Food Production/sweden_prod_clean.csv")
norway_food <- read_csv("Food Production/norway_prod_clean.csv")

denmark_suhu <- read_csv("Temperature in Group/D_suhu_all_clean_data.csv")
finland_suhu <- read_csv("Temperature in Group/F_suhu_all_clean_data.csv")
sweden_suhu <- read_csv("Temperature in Group/S_suhu_all_clean_data.csv")
norway_suhu <- read_csv("Temperature in Group/N_suhu_all_clean_data.csv")

######################################################### Food Data Preparation
# Add country column to each dataset
denmark_food$Country <- "Denmark"
finland_food$Country <- "Finland"
sweden_food$Country <- "Sweden"
norway_food$Country <- "Norway"

# Find all unique food items across the four datasets
food_items <- unique(c(colnames(denmark_food), colnames(finland_food), colnames(sweden_food), colnames(norway_food)))

# Remove non-food item columns (Year, Country)
food_items <- setdiff(food_items, c("Year", "Country"))

# Ensure all datasets have the same columns by adding missing food items with NA
add_missing_cols <- function(df, food_items) {
  missing_cols <- setdiff(food_items, colnames(df))
  df[missing_cols] <- NA
  return(df)
}

denmark_food <- add_missing_cols(denmark_food, food_items)
finland_food <- add_missing_cols(finland_food, food_items)
sweden_food <- add_missing_cols(sweden_food, food_items)
norway_food <- add_missing_cols(norway_food, food_items)

# Select and reorder columns for consistency
final_cols <- c("Year", "Country", food_items)
denmark_food <- denmark_food[, final_cols]
finland_food <- finland_food[, final_cols]
sweden_food <- sweden_food[, final_cols]
norway_food <- norway_food[, final_cols]

# Combine all into a single stacked dataframe
prod_data <- bind_rows(denmark_food, finland_food, sweden_food, norway_food)

# Save to CSV if needed
#write_csv(prod_data, "Food Production/prod_data_clean.csv")

# Check the structure
glimpse(prod_data)
str(prod_data) # harus numeric

################################################# Temperature Data Preparation

# Function to aggregate temperature by year (ignoring group_id)
aggregate_temperature <- function(df, country_name) {
  df %>%
    group_by(Year) %>%
    summarize(Temperature = mean(annual_temp, na.rm = TRUE)) %>%
    mutate(Country = country_name)
}

# Apply function to each country's data
denmark_suhu <- aggregate_temperature(denmark_suhu, "Denmark")
finland_suhu <- aggregate_temperature(finland_suhu, "Finland")
sweden_suhu <- aggregate_temperature(sweden_suhu, "Sweden")
norway_suhu <- aggregate_temperature(norway_suhu, "Norway")

# Combine all into a single stacked dataframe
suhu_data <- bind_rows(denmark_suhu, finland_suhu, sweden_suhu, norway_suhu)

# Check the structure
glimpse(suhu_data)

########################################### Merge Data for Random Effect Country

# Merge by "Year" and "Country"
merged_data <- prod_data %>%
  left_join(suhu_data, by = c("Year", "Country"))

# Select relevant years (1984-2021)
merged_data <- merged_data %>%
  filter(Year >= 1984 & Year <= 2021)

# Reorder columns: move "Annual_Temp" after "Country"
merged_data <- merged_data %>%
  select(Year, Country, Temperature, everything())

# Check the structure
glimpse(merged_data)

# Replace countries with numeric values
merge_data_c <- merged_data %>%
  mutate(Country = recode(Country, 
                          "Denmark" = 1, 
                          "Finland" = 2, 
                          "Sweden" = 3, 
                          "Norway" = 4))

head(merge_data_c) #Country in number
tail(merge_data_c)


################ Get food Items available in 4 country so Random Effect can run

# Exclude "Year", "Country", and "Temperature" to get only food items
food_items <- setdiff(colnames(merge_data_c), c("Year", "Country", "Temperature"))

# Identify which food items are available for all 4 countries
countries <- unique(merge_data_c$Country)  # List of unique countries

# Initialize an empty dataframe to store food items available in all countries
food_available_all_countries <- c()

# Loop through each food item (columns for food items)
for (food_column in food_items) {
  # Check if the food item has non-missing values for all 4 countries
  food_data <- merge_data_c[, c("Country", food_column)]
  food_data_complete <- food_data[complete.cases(food_data[, food_column]), ]
  
  # If the food item appears in all 4 countries, add it to the list
  if (length(unique(food_data_complete$Country)) == length(countries)) {
    food_available_all_countries <- c(food_available_all_countries, food_column)
  }
}

# Display the list of food items available in all 4 countries
print("Food items available in all 4 countries:")
print(food_available_all_countries)


###################################### Model 1-3: lm & lmm + model 4: AIC + Pval

# Load necessary library
library(lmerTest)  # provides p-values
library(tibble)
library(performance)  # for R^2

# Initialize empty dataframes
summary_m1_df <- data.frame()
summary_m2_df <- data.frame()
summary_m3_df <- data.frame()
summary_m4_df <- data.frame()

# Loop through each food item
for (food_column in food_available_all_countries) {
  print(paste("Fitting models for:", food_column))
  
  # Define formulas
  formula1 <- as.formula(paste0("`", food_column, "` ~ Temperature"))
  formula2 <- as.formula(paste0("`", food_column, "` ~ Temperature + Year"))
  formula3 <- as.formula(paste0("`", food_column, "` ~ Temperature + Year + (1 | Country)"))
  
  # Fit models
  model_m1 <- lm(formula1, data = merge_data_c)
  model_m2 <- lm(formula2, data = merge_data_c)
  model_m3 <- lmer(formula3, data = merge_data_c)  # Random effect model
  
  # Extract summaries
  summary_m1 <- summary(model_m1)
  summary_m2 <- summary(model_m2)
  summary_m3 <- summary(model_m3)  # From lmerTest
  
  # Extract coefficients and p-values
  m1_coef <- coef(summary_m1)
  m2_coef <- coef(summary_m2)
  m3_coef <- coef(summary_m3)
  
  m1_pval <- summary_m1$coefficients[, 4]
  m2_pval <- summary_m2$coefficients[, 4]
  m3_pval <- coef(summary_m3)[, 5]  # lmerTest adds p-values in column 5
  
  # Extract AIC BIC
  aic_m1 <- AIC(model_m1)
  aic_m2 <- AIC(model_m2)
  aic_m3 <- AIC(model_m3)
  
  bic_m1 <- BIC(model_m1)
  bic_m2 <- BIC(model_m2)
  bic_m3 <- BIC(model_m3)
  
  # Extract random effect variance for Food_Item
  random_effects_variance <- as.numeric(VarCorr(model_m3)$Country[1])
  
  # Store Model 1 coefficients
  summary_m1_df <- rbind(summary_m1_df, data.frame(
    Food = food_column,
    Intercept_m1 = m1_coef["(Intercept)", 1], 
    Temperature_m1 = m1_coef["Temperature", 1],
    Pval_Intercept_M1 = m1_pval["(Intercept)"],
    Pval_Temperature_M1 = m1_pval["Temperature"]
  ))
  
  # Store Model 2 coefficients
  summary_m2_df <- rbind(summary_m2_df, data.frame(
    Food = food_column,
    Intercept_m2 = m2_coef["(Intercept)", 1],
    Temperature_m2 = m2_coef["Temperature", 1],
    Year_m2 = ifelse("Year" %in% rownames(m2_coef), m2_coef["Year", 1], NA),
    Pval_Intercept_M2 = m2_pval["(Intercept)"],
    Pval_Temperature_M2 = m2_pval["Temperature"],
    Pval_Year_M2 = ifelse("Year" %in% rownames(m2_coef), m2_pval["Year"], NA)
  ))
  
  # Store Model 3 coefficients with random effect variance
  summary_m3_df <- rbind(summary_m3_df, data.frame(
    Food = food_column,
    Intercept_m3 = m3_coef["(Intercept)", 1],
    Temperature_m3 = m3_coef["Temperature", 1],
    Year_m3 = ifelse("Year" %in% rownames(m3_coef), m3_coef["Year", 1], NA),
    Pval_Intercept_M3 = m3_pval["(Intercept)"],
    Pval_Temperature_M3 = m3_pval["Temperature"],
    Pval_Year_M3 = ifelse("Year" %in% rownames(m3_coef), m3_pval["Year"], NA),
    Random_Variance_Country = random_effects_variance  # Added column
  ))
  
  # Compute R2
  r2_m1 <- summary(model_m1)$r.squared
  r2_m2 <- summary(model_m2)$r.squared
  r2_m3 <- r2(model_m3)
  r2_marginal_m3 <- r2_m3$R2_marginal  # Fixed effects R²
  r2_conditional_m3 <- r2_m3$R2_conditional  # Full model R²
  
  # Store AIC, p-values, random variance, and R² in Model 4
  summary_m4_df <- rbind(summary_m4_df, data.frame(
    Food = food_column,
    
    AIC_M1 = aic_m1, BIC_M1 = bic_m1,
    Pval_Temperature_M1 = m1_pval["Temperature"],
    R2_M1 = r2_m1,
    
    AIC_M2 = aic_m2, BIC_M2 = bic_m2,
    Pval_Temperature_M2 = m2_pval["Temperature"],
    R2_M2 = r2_m2,
    
    AIC_M3 = aic_m3, BIC_M3 = bic_m3,
    Pval_Temperature_M3 = m3_pval["Temperature"],
    
    Random_Variance_Country = random_effects_variance,  # Added column
    r2_marginal_m3 = r2_marginal_m3,  # Fixed effects R²
    r2_conditional_m3 = r2_conditional_m3  # Full model R²
  ))
  
}
# Ensure 'Food' column contains the correct food names
summary_m4_df$Food <- unique(summary_m4_df$Food)

# If row names were mistakenly set as food names, reset them
summary_m4_df <- summary_m4_df %>% rownames_to_column(var = "Food")

# Reset row names to avoid issues
rownames(summary_m4_df) <- NULL


# Print summaries
print(summary_m1_df)
print(summary_m2_df)
print(summary_m3_df)
print(summary_m4_df)


############################# Prepare merged_data for food item as random effect

library(dplyr)
library(tidyr)
library(lmerTest)
library(tibble)

# Convert to long format: Ensure Food_Item is a variable
merge_data_f <- merge_data_c %>%
  pivot_longer(cols = -c(Year, Country, Temperature), 
               names_to = "Food_Item", values_to = "Production")

# Identify food items available in all 4 countries
food_available_all_countries <- merge_data_f %>%
  filter(!is.na(Production)) %>%
  group_by(Food_Item) %>%
  summarise(unique_countries = n_distinct(Country), .groups = "drop") %>%
  filter(unique_countries == 4) %>%
  pull(Food_Item)

# Filter data for valid food items
merge_data_f <- merge_data_f %>%
  filter(Food_Item %in% food_available_all_countries)

# Ensure Year is numeric
merge_data_f <- merge_data_f %>% mutate(Year = as.numeric(Year))

# Get unique countries
countries <- unique(merge_data_f$Country)

#################################################### Model 5-7 and summary at m8

# Initialize empty results dataframes
summary_m5_df <- data.frame()
summary_m6_df <- data.frame()
summary_m7_df <- data.frame()
summary_m8_df <- data.frame()

# Loop through each country
for (country in countries) {
  print(paste("Fitting models for Country:", country))
  
  # Filter data for the current country
  country_data <- merge_data_f %>% filter(Country == country)
  model_m5 <- lm(Production ~ Temperature, data = country_data)
  model_m6 <- lm(Production ~ Temperature + Year, data = country_data)
  model_m7 <- lmer(Production ~ Temperature + Year + (1 | Food_Item), data = country_data)
  
  # Extract summaries
  summary_m5 <- summary(model_m5)
  summary_m6 <- summary(model_m6)
  summary_m7 <- summary(model_m7)
  
  # Extract coefficients and p-values
  m5_coef <- coef(summary_m5)
  m6_coef <- coef(summary_m6)
  m7_coef <- coef(summary_m7)
  
  m5_pval <- summary_m5$coefficients[, 4]
  m6_pval <- summary_m6$coefficients[, 4]
  m7_pval <- coef(summary(model_m7))[, 4]  # Extract p-values safely
  
  # Extract AIC values
  aic_m5 <- AIC(model_m5)
  aic_m6 <- AIC(model_m6)
  aic_m7 <- AIC(model_m7)
  
  bic_m5 <- BIC(model_m5)
  bic_m6 <- BIC(model_m6)
  bic_m7 <- BIC(model_m7)
  
  # Extract random effect variance for Food_Item
  random_effects_variance <- as.numeric(VarCorr(model_m7)$Food_Item[1])
  
  # Compute R2 for Model 5 (only fixed effects)
  r2_m5 <- summary(model_m5)$r.squared  # Standard R²
  
  # Compute R² for Model 6 (only fixed effects)
  r2_m6 <- summary(model_m6)$r.squared  # Standard R²
  
  # Compute R² for Model 7 (Mixed model: Marginal & Conditional)
  r2_m7 <- r2_nakagawa(model_m7)
  marginal_r2_m7 <- r2_m7$R2_marginal
  conditional_r2_m7 <- r2_m7$R2_conditional
  
  # Store Model 5 results
  summary_m5_df <- rbind(summary_m5_df, data.frame(
    Country = country,
    Intercept_m5 = m5_coef["(Intercept)", 1], 
    Temperature_m5 = m5_coef["Temperature", 1],
    Pval_Intercept_m5 = m5_pval["(Intercept)"],
    Pval_Temperature_m5 = m5_pval["Temperature"]
  ))
  
  # Store Model 6 results
  summary_m6_df <- rbind(summary_m6_df, data.frame(
    Country = country,
    Intercept_m6 = m6_coef["(Intercept)", 1],
    Temperature_m6 = m6_coef["Temperature", 1],
    Year_m6 = ifelse("Year" %in% rownames(m6_coef), m6_coef["Year", 1], NA),
    Pval_Intercept_m6 = m6_pval["(Intercept)"],
    Pval_Temperature_m6 = m6_pval["Temperature"],
    Pval_Year_m6 = if ("Year" %in% names(m6_pval)) m6_pval["Year"] else NA
  ))
  
  # Store Model 7 results
  summary_m7_df <- rbind(summary_m7_df, data.frame(
    Country = country,
    Intercept_m7 = m7_coef["(Intercept)", 1],
    Temperature_m7 = m7_coef["Temperature", 1],
    Year_m7 = ifelse("Year" %in% rownames(m7_coef), m7_coef["Year", 1], NA),
    Pval_Intercept_m7 = m7_pval["(Intercept)"],
    Pval_Temperature_m7 = m7_pval["Temperature"],
    Pval_Year_m7 = if ("Year" %in% names(m7_pval)) m7_pval["Year"] else NA,
    Random_Variance_Food_Item = random_effects_variance
  ))
  
  # Store AIC and p-values in Model 8
  summary_m8_df <- rbind(summary_m8_df, data.frame(
    Country = country,
    
    AIC_M5 = aic_m5, BIC_M5 = bic_m5,
    Pval_Temperature_M5 = m5_pval["Temperature"],
    R2_M5 = r2_m5,
    
    AIC_M6 = aic_m6, BIC_M6 = bic_m6,
    Pval_Temperature_M6 = m6_pval["Temperature"],
    R2_M6 = r2_m6,
    
    AIC_M7 = aic_m7, BIC_M7 = bic_m7,
    Pval_Temperature_M7 = m7_pval["Temperature"],
    
    Random_Variance_Country = random_effects_variance,
    Marginal_R2_M7 = marginal_r2_m7,
    Conditional_R2_M7 = conditional_r2_m7
  ))
}

# Reset row names
rownames(summary_m8_df) <- NULL

# Print summaries
print(summary_m5_df)
print(summary_m6_df)
print(summary_m7_df)
print(summary_m8_df)

######################################################## Classic Asumption - LMM

library(car)   # For VIF, Durbin-Watson, requires at least two predictor variables to be meaningful.
library(lmtest) # For Breusch-Pagan
library(ggplot2)
library(performance)  # For checking assumptions in mixed models

# Function to check assumptions for a model
check_assumptions <- function(model, model_name) {
  cat("\nChecking assumptions for", model_name, "\n")
  
  # Linearity & Homoscedasticity: Plot Residuals vs Fitted
  plot(model, which = 1, main = paste("Residuals vs Fitted:", model_name))
  
  # Normality of Residuals: Histogram & Q-Q Plot
  par(mfrow = c(1, 2))
  hist(residuals(model), main = paste("Histogram of Residuals:", model_name), breaks = 20, col = "lightblue")
  qqnorm(residuals(model), main = paste("Q-Q Plot:", model_name))
  qqline(residuals(model), col = "red")
  par(mfrow = c(1, 1))
  
  # Shapiro-Wilk Normality Test
  shapiro_test <- shapiro.test(residuals(model))
  print(shapiro_test)
  
  # Homoscedasticity Test: Breusch-Pagan
  bp_test <- bptest(model)
  print(bp_test)
  
  # Independence Test: Durbin-Watson (for LM models)
  if ("lm" %in% class(model)) {
    dw_test <- dwtest(model)
    print(dw_test)
  }
  
  # Multicollinearity Test: VIF (for LM models)
  if ("lm" %in% class(model)) {
    vif_values <- vif(model)
    print(vif_values)
  }
}

# Run assumption tests for LMM models
check_assumptions(model_m2, "Model 2")
check_assumptions(model_m6, "Model 6")

# Run assumption tests for LMM models with random effect - Globaly not per food / per country
check_model(model_m3)
check_model(model_m7)

######################################################## Classic Asumption - LM

library(car)   # For Durbin-Watson test
library(lmtest) # For Breusch-Pagan test
library(ggplot2)

# Function to check classic assumptions for models with one predictor
check_single_predictor_assumptions <- function(model, model_name) {
  cat("\nChecking assumptions for", model_name, "\n")
  
  # Linearity & Homoscedasticity: Residuals vs Fitted
  plot(model, which = 1, main = paste("Residuals vs Fitted:", model_name))
  
  # Normality of Residuals: Histogram & Q-Q Plot
  par(mfrow = c(1, 2))
  hist(residuals(model), main = paste("Histogram of Residuals:", model_name), breaks = 20, col = "lightblue")
  qqnorm(residuals(model), main = paste("Q-Q Plot:", model_name))
  qqline(residuals(model), col = "red")
  par(mfrow = c(1, 1))
  
  # Shapiro-Wilk Normality Test
  shapiro_test <- shapiro.test(residuals(model))
  print(shapiro_test)
  
  # Homoscedasticity Test: Breusch-Pagan
  bp_test <- bptest(model)
  print(bp_test)
  
  # Independence Test: Durbin-Watson
  dw_test <- dwtest(model)
  print(dw_test)
}

# Run assumption tests for Model 1 and Model 5
check_single_predictor_assumptions(model_m1, "Model 1")
check_single_predictor_assumptions(model_m5, "Model 5")

########################################### Classic Assumption - per 1|country

# Load necessary libraries
library(lmerTest)   # Provides p-values for mixed models
library(tibble)     # Data manipulation
library(performance) # R^2 for mixed models & collinearity in mixed models
library(car)        # VIF for multicollinearity
library(lmtest)     # Breusch-Pagan test for homoscedasticity

# Initialize empty dataframe for assumption test results
classic_assumption_country <- data.frame()

# Loop through each food item
for (food_column in food_available_all_countries) {
  print(paste("Checking assumptions for:", food_column))
  
  # Define formulas
  formula1 <- as.formula(paste0("`", food_column, "` ~ Temperature"))
  formula2 <- as.formula(paste0("`", food_column, "` ~ Temperature + Year"))
  formula3 <- as.formula(paste0("`", food_column, "` ~ Temperature + Year + (1 | Country)"))  # Mixed-effects model
  
  # Fit models
  model_m1 <- lm(formula1, data = merge_data_c)
  model_m2 <- lm(formula2, data = merge_data_c)
  model_m3 <- lmer(formula3, data = merge_data_c)
  
  # Assumption checks
  
  ## Normality of residuals (Shapiro-Wilk Test)
  normality_m1 <- shapiro.test(residuals(model_m1))$p.value
  normality_m2 <- shapiro.test(residuals(model_m2))$p.value
  normality_m3 <- shapiro.test(residuals(model_m3))$p.value
  
  ## Homoscedasticity (Breusch-Pagan Test) – Only applies to M1 and M2
  homoscedasticity_m1 <- bptest(model_m1)$p.value
  homoscedasticity_m2 <- bptest(model_m2)$p.value
  
  ## Homoscedasticity for M3 – Using alternative method (residual variance test)
  homoscedasticity_m3 <- bptest(lm(residuals(model_m3) ~ fitted(model_m3)))$p.value
  
  ## Multicollinearity (VIF - only for Model 2)
  vif_m2 <- max(vif(model_m2))  # Highest VIF value
  
  ## Collinearity for Model 3 (since standard VIF is not applicable)
  collinearity_m3 <- check_collinearity(model_m3)
  max_vif_m3 <- max(collinearity_m3$VIF)  # Extract max VIF value for M3
  
  # Store results in a dataframe
  classic_assumption_country <- rbind(classic_assumption_country, data.frame(
    Food = food_column,
    Normality_pval_M1 = normality_m1,
    Homoscedasticity_pval_M1 = homoscedasticity_m1,
    
    Normality_pval_M2 = normality_m2,
    Homoscedasticity_pval_M2 = homoscedasticity_m2,
    Max_VIF_M2 = vif_m2,
    
    Normality_pval_M3 = normality_m3,
    Homoscedasticity_pval_M3 = homoscedasticity_m3,
    Max_VIF_M3 = max_vif_m3  # Added collinearity for Model 3
  ))
}

# Print results
print(classic_assumption_country)

########################################### Classic Assumption - per 1|food_item

# Load necessary libraries
library(lmerTest)   # Provides p-values for mixed models
library(tibble)     # Data manipulation
library(performance) # R^2 for mixed models & collinearity in mixed models
library(car)        # VIF for multicollinearity
library(lmtest)     # Breusch-Pagan test for homoscedasticity
library(stats)      # ACF function for autocorrelation

# Initialize empty dataframe for assumption test results
classic_assumption_food <- data.frame()


# Loop through each country
for (country in countries) {
  print(paste("Checking assumptions for Country:", country))
  
  # Filter data for the current country
  country_data <- merge_data_f %>% filter(Country == country)
  
  # Fit models
  model_m5 <- lm(Production ~ Temperature, data = country_data)
  model_m6 <- lm(Production ~ Temperature + Year, data = country_data)
  model_m7 <- lmer(Production ~ Temperature + Year + (1 | Food_Item), data = country_data)
  
  # Assumption checks
  
  ## Normality of residuals (Shapiro-Wilk Test)
  normality_m5 <- shapiro.test(residuals(model_m5))$p.value
  normality_m6 <- shapiro.test(residuals(model_m6))$p.value
  normality_m7 <- shapiro.test(residuals(model_m7))$p.value
  
  ## Homoscedasticity (Breusch-Pagan Test) – Only applies to M5 and M6
  homoscedasticity_m5 <- bptest(model_m5)$p.value
  homoscedasticity_m6 <- bptest(model_m6)$p.value
  
  ## Homoscedasticity for M7 – Using alternative method (residual variance test)
  homoscedasticity_m7 <- bptest(lm(residuals(model_m7) ~ fitted(model_m7)))$p.value
  
  ## Multicollinearity (VIF - only for Model 6)
  vif_m6 <- max(vif(model_m6))  # Highest VIF value
  
  ## Collinearity for Model 7 (since standard VIF is not applicable)
  collinearity_m7 <- check_collinearity(model_m7)
  max_vif_m7 <- max(collinearity_m7$VIF)  # Extract max VIF value for M7
  
  ## **Autocorrelation Tests**
  
  # Durbin-Watson Test for autocorrelation (M5 & M6)
  autocorr_m5 <- dwtest(model_m5)$p.value
  autocorr_m6 <- dwtest(model_m6)$p.value
  
  # ACF for mixed models (M7) – checking first lag autocorrelation
  acf_values_m7 <- acf(residuals(model_m7), plot = FALSE)
  autocorr_m7 <- acf_values_m7$acf[2]  # Extract first-lag autocorrelation
  
  # Store results in a dataframe
  classic_assumption_food <- rbind(classic_assumption_food, data.frame(
    Country = country,
    Normality_pval_M5 = normality_m5,
    Homoscedasticity_pval_M5 = homoscedasticity_m5,
    Autocorrelation_pval_M5 = autocorr_m5,
    
    Normality_pval_M6 = normality_m6,
    Homoscedasticity_pval_M6 = homoscedasticity_m6,
    Max_VIF_M6 = vif_m6,
    Autocorrelation_pval_M6 = autocorr_m6,
    
    Normality_pval_M7 = normality_m7,
    Homoscedasticity_pval_M7 = homoscedasticity_m7,
    Max_VIF_M7 = max_vif_m7,  # Added collinearity for Model 7
    Autocorrelation_M7 = autocorr_m7  # Added first-lag ACF for M7
  ))
}

# Print results
print(classic_assumption_food)

########################################## Handle Classic Assumption Violation

# Load necessary libraries
library(lmerTest)  # Provides p-values for mixed models
library(tibble)
library(performance)  # For R²

# Ensure the log-transformed variable exists
merge_data_c$ln_Cauliflowers <- log(merge_data_c$`Cauliflowers and broccoli`)
merge_data_c$ln_Pears <- log(merge_data_c$`Pears`)


#### check assumption again

# Load necessary libraries
library(lmerTest)   # Provides p-values for mixed models
library(tibble)     # Data manipulation
library(performance) # R^2 for mixed models & collinearity in mixed models
library(car)        # VIF for multicollinearity
library(lmtest)     # Breusch-Pagan test for homoscedasticity

# List of food items to analyze
food_items <- c("ln_Cauliflowers","ln_Pears")

# Initialize empty dataframe for assumption test results
classic_assumption_cauli_pears <- data.frame()

# Loop through each food item
for (food_column in food_items) {
  print(paste("Checking assumptions for:", food_column))
  
  # Define formulas
  formula1 <- as.formula(paste0(food_column, " ~ Temperature"))
  formula2 <- as.formula(paste0(food_column, " ~ Temperature + Year"))
  formula3 <- as.formula(paste0(food_column, " ~ Temperature + Year + (1 | Country)"))  # Mixed-effects model
  
  # Fit models
  model_m1 <- lm(formula1, data = merge_data_c)
  model_m2 <- lm(formula2, data = merge_data_c)
  model_m3 <- lmer(formula3, data = merge_data_c)
  
  # Assumption checks
  
  ## Normality of residuals (Shapiro-Wilk Test)
  normality_m1 <- shapiro.test(residuals(model_m1))$p.value
  normality_m2 <- shapiro.test(residuals(model_m2))$p.value
  normality_m3 <- shapiro.test(residuals(model_m3))$p.value
  
  ## Homoscedasticity (Breusch-Pagan Test) – Only applies to M1 and M2
  homoscedasticity_m1 <- bptest(model_m1)$p.value
  homoscedasticity_m2 <- bptest(model_m2)$p.value
  
  ## Homoscedasticity for M3 – Using alternative method (residual variance test)
  homoscedasticity_m3 <- bptest(lm(residuals(model_m3) ~ fitted(model_m3)))$p.value
  
  ## Multicollinearity (VIF - only for Model 2)
  vif_m2 <- max(vif(model_m2))  # Highest VIF value
  
  ## Collinearity for Model 3 (since standard VIF is not applicable)
  collinearity_m3 <- check_collinearity(model_m3)
  max_vif_m3 <- max(collinearity_m3$VIF)  # Extract max VIF value for M3
  
  # Store results in a dataframe
  classic_assumption_cauli_pears <- rbind(classic_assumption_cauli_pears, data.frame(
    Food = food_column,
    Normality_pval_M1 = normality_m1,
    Homoscedasticity_pval_M1 = homoscedasticity_m1,
    
    Normality_pval_M2 = normality_m2,
    Homoscedasticity_pval_M2 = homoscedasticity_m2,
    Max_VIF_M2 = vif_m2,
    
    Normality_pval_M3 = normality_m3,
    Homoscedasticity_pval_M3 = homoscedasticity_m3,
    Max_VIF_M3 = max_vif_m3  # Added collinearity for Model 3
  ))
}

# Print results
print(classic_assumption_cauli_pears)

