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


################### Find foods available in all 4 country

# Get the list of countries you're interested in
target_countries <- c("Denmark", "Finland", "Norway", "Sweden")

# Filter the dataset to include only those 4 countries
food_all_wide_4 <- food_all_wide %>%
  filter(Area %in% target_countries)

# Identify food item columns (exclude 'Area' and 'Year')
food_columns <- setdiff(names(food_all_wide_4), c("Area", "Year"))

# Initialize a vector to hold food items available in all 4 countries
food_available_all_countries <- c()

# Loop through each food item column
for (food in food_columns) {
  # Get data for this food item
  food_data <- food_all_wide_4[, c("Area", food)]
  colnames(food_data) <- c("Area", "FoodValue")
  
  # Keep only rows where value is not empty
  food_data <- food_data[food_data$FoodValue != "", ]
  
  # Check if all 4 countries are present
  if (all(target_countries %in% unique(food_data$Area))) {
    food_available_all_countries <- c(food_available_all_countries, food)
  }
}

# View result
cat("✅ Food items available in all 4 countries:\n")
print(food_available_all_countries)


###################### Sum all production item of the same country


library(dplyr)
library(tidyr)
library(ggplot2)

# Step 1: Filter dataset
filtered_data <- food_all_wide %>%
  filter(Area %in% target_countries) %>%
  select(c("Area", "Year", all_of(food_available_all_countries)))

# Step 2: Convert "" to 0 for summation
filtered_data[food_available_all_countries] <- lapply(
  filtered_data[food_available_all_countries],
  function(x) as.numeric(ifelse(x == "", 0, x))
)

# Step 3: Sum all food item production per country per year
production_sums <- filtered_data %>%
  group_by(Area, Year) %>%
  summarise(Total_Production = sum(across(all_of(food_available_all_countries)), na.rm = TRUE)) %>%
  ungroup() %>%
  rename(Country = Area)  

# Step 4: Change name for plot
production_sums <- production_sums %>%
  mutate(Country = recode(Country,
                          "Denmark" = "Denmark",
                          "Finland" = "Finlandia",
                          "Norway" = "Norwegia",
                          "Sweden" = "Swedia"))

# Step 5: Plot
ggplot(production_sums, aes(x = Year, y = Total_Production, color = Country, linetype = Country)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(
    title = "Total Produksi Pangan Tahunan Antar Negara",
    x = "Tahun",
    y = "Total Produksi Pangan",
    color = "Negara",
    linetype = "Negara"
  ) +
  scale_color_manual(values = c("Denmark" = "skyblue", 
                                "Finlandia" = "lightgreen", 
                                "Swedia" = "plum", 
                                "Norwegia" = "lightcoral")) +
  scale_linetype_manual(values = c("Denmark" = "solid", 
                                   "Finlandia" = "dashed", 
                                   "Swedia" = "dotted", 
                                   "Norwegia" = "dotdash")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )


########### Box Plot

ggplot(production_sums, aes(x = Country, y = Total_Production, fill = Country)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal() +
  labs(
    title = "Distribusi Total Produksi Pangan Antar Negara",
    x = "Negara",
    y = "Total Produksi Pangan"
  ) +
  scale_fill_manual(values = c("Denmark" = "skyblue", 
                               "Finlandia" = "lightgreen", 
                               "Swedia" = "plum", 
                               "Norwegia" = "lightcoral")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 10),
    legend.position = "none"  # Optional: hide legend since x-axis already shows countries
  )


