library(dplyr)
library(tidyr)

# Read Data
food_all <- read.csv("Food Production/food_prod_all.csv")

### Drop unnecessary columns
food_all_c <- food_all %>%
  select(-c("Domain.Code", "Domain", "Area.Code..M49.", "Element.Code", "Element","Unit",
            "Item.Code..CPC.", "Year.Code", "Flag", "Flag.Description", "Note"))
head(food_all_c)

### Ensure data is sorted properly before pivoting
food_all_c <- food_all_c %>%
  arrange(Area, Year, Item)

# Pivot data (Items become columns)
food_all_wide <- food_all_c %>%
  pivot_wider(names_from = Item, values_from = Value)

head(food_all_wide)

### Replace 0 and NaN as empty string
# Step 1: Replace all NA with 0
food_all_wide <- food_all_wide %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# Step 2: Replace all 0 with an empty string
food_all_wide <- food_all_wide %>%
  mutate(across(where(is.numeric), ~ ifelse(. == 0, "", .)))

head(food_all_wide)


### Separate DataFrames per country
denmark_prod_clean <- food_all_wide %>% filter(Area == "Denmark")
finland_prod_clean <- food_all_wide %>% filter(Area == "Finland")
norway_prod_clean  <- food_all_wide %>% filter(Area == "Norway")
sweden_prod_clean  <- food_all_wide %>% filter(Area == "Sweden")

### Drop 'Area' column and remove columns where all values are empty ("")
clean_dataframe <- function(df) {
  df <- df %>% select(-Area)  # Drop Area column
  df <- df %>% select(where(~ any(. != "")))  # Drop columns where all values are empty
  return(df)
}

# Apply cleaning function to each dataframe
denmark_prod_clean <- clean_dataframe(denmark_prod_clean)
finland_prod_clean <- clean_dataframe(finland_prod_clean)
norway_prod_clean  <- clean_dataframe(norway_prod_clean)
sweden_prod_clean  <- clean_dataframe(sweden_prod_clean)

# View the first few rows of each cleaned dataframe
head(denmark_prod_clean)
head(finland_prod_clean)
head(norway_prod_clean)
head(sweden_prod_clean)

# Save to CSV without row numbers
#write.csv(denmark_prod_clean, "denmark_prod_clean.csv", row.names = FALSE)
#write.csv(finland_prod_clean, "finland_prod_clean.csv", row.names = FALSE)
#write.csv(norway_prod_clean, "norway_prod_clean.csv", row.names = FALSE)
#write.csv(sweden_prod_clean, "sweden_prod_clean.csv", row.names = FALSE)


###################### Function to calculate mean production per year


calculate_annual_production <- function(df) {
  df %>%
    mutate(across(-Year, ~ as.numeric(.), .names = "num_{col}")) %>%  # Convert to numeric
    rowwise() %>%
    mutate(mean_production = mean(c_across(starts_with("num_")), na.rm = TRUE)) %>%
    ungroup() %>%
    select(Year, mean_production)  # Keep only Year and mean production
}

# Calculate mean production for each country
denmark_annual_prod <- calculate_annual_production(denmark_prod_clean)
finland_annual_prod <- calculate_annual_production(finland_prod_clean)
norway_annual_prod  <- calculate_annual_production(norway_prod_clean)
sweden_annual_prod  <- calculate_annual_production(sweden_prod_clean)

# View results
head(denmark_annual_prod)
head(finland_annual_prod)
head(norway_annual_prod)
head(sweden_annual_prod)


######################## Visual annual production


library(ggplot2)

# Combine all datasets with a country identifier
denmark_annual_prod$Country <- "Denmark"
finland_annual_prod$Country <- "Finlandia"
norway_annual_prod$Country <- "Norwegia"
sweden_annual_prod$Country <- "Swedia"

combined_prod <- bind_rows(
  denmark_annual_prod,
  finland_annual_prod,
  norway_annual_prod,
  sweden_annual_prod
)

# Plot
ggplot(combined_prod, aes(x = Year, y = mean_production, color = Country, linetype = Country)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(
    title = "Rataan Produksi Pangan Tahunan Antar Negara",
    x = "Tahun",
    y = "Rataan Produksi Pangan",
    color = "Negara",
    linetype = "Negara"
  ) +
  scale_color_manual(values = c("blue", "red", "green", "black")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    #axis.title.x = element_text(face = "italic"),
    #axis.title.y = element_text(face = "italic"),
    legend.position = "bottom",
    #legend.title = element_text(face = "italic")
  )


########### Box Plot

# Boxplot of annual mean production
ggplot(combined_prod, aes(x = Country, y = mean_production, fill = Country)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal() +
  labs(
    title = "Distribusi Produksi Pangan Tahunan Antar Negara",
    x = "Negara",
    y = "Rataan Produksi Pangan"
  ) +
  scale_fill_manual(values = c("blue", "red", "green", "black")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    #axis.title.x = element_text(face = "italic"),
    #axis.title.y = element_text(face = "italic"),
    axis.text = element_text(size = 10)
  )


