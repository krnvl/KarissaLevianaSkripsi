## Grouping Temperature Data per Latitude and Longitude

library(tidyverse)
library(dplyr)
library(ggplot2) # for graph
library(TSA)
library(tseries)

# Load the temperature data working directory : user - tgs ml - skripsi - data2
Dsuhu <- read.csv("Temperature in Group/D_suhu_all.csv")
Fsuhu <- read.csv("Temperature in Group/F_suhu_all.csv")
Nsuhu <- read.csv("Temperature in Group/N_suhu_all.csv")
Ssuhu <- read.csv("Temperature in Group/S_suhu_all.csv")

head(Dsuhu)
head(Fsuhu)
head(Nsuhu)
head(Ssuhu)

####################################################################### Function to Process Data for Each Dataset

process_suhu_data <- function(suhu_data, dataset_name) {
  
  # Drop unnecessary columns
  suhu_data <- subset(suhu_data, select = -c(PARAMETER, ANN))
  
  # Adjust year range
  suhu_data <- subset(suhu_data, YEAR >= 1984 & YEAR <= 2021)
  
  # Create a unique ID for each LAT-LON pair
  lat_lon_groups <- suhu_data %>%
    distinct(LAT, LON) %>%
    mutate(Group_ID = row_number())  # Assign a unique number
  
  # Merge with the original dataset
  suhu_data <- suhu_data %>%
    left_join(lat_lon_groups, by = c("LAT", "LON"))
  
  # Compute the average temperature for each Group_ID per YEAR
  yearly_temp <- suhu_data %>%
    group_by(Group_ID, YEAR) %>%
    summarise(Annual_Avg_Temp = mean(c_across(JAN:DEC), na.rm = TRUE), .groups = "drop")
  
  # Save the cleaned data to CSV (with columns: Group_ID, Year, Annual_Avg_Temp)
  # clean_data <- yearly_temp %>%
  #   select(Group_ID, YEAR, Annual_Avg_Temp) %>%
  #   rename(group_id = Group_ID, year = YEAR, annual_temp = Annual_Avg_Temp)
  # 
  # write.csv(clean_data, paste0(dataset_name, "_clean_data.csv"), row.names = FALSE)
  
  return(yearly_temp)
}

# Process each dataset
D_suhu_yearly_temp <- process_suhu_data(Dsuhu, "D_suhu_all")
F_suhu_yearly_temp <- process_suhu_data(Fsuhu, "F_suhu_all")
N_suhu_yearly_temp <- process_suhu_data(Nsuhu, "N_suhu_all")
S_suhu_yearly_temp <- process_suhu_data(Ssuhu, "S_suhu_all")

print(D_suhu_yearly_temp)

###################### Per Lat-Lon Pair

# Plotting for D_suhu_all
ggplot(D_suhu_yearly_temp, aes(x = YEAR, y = Annual_Avg_Temp, color = as.factor(Group_ID))) +
  geom_line() +
  labs(title = "Annual Average Temperature for Each Group in D_suhu_all",
       x = "Year",
       y = "Annual Average Temperature",
       color = "Group ID") +
  theme_minimal()

# Plotting for F_suhu_all
ggplot(F_suhu_yearly_temp, aes(x = YEAR, y = Annual_Avg_Temp, color = as.factor(Group_ID))) +
  geom_line() +
  labs(title = "Annual Average Temperature for Each Group in F_suhu_all",
       x = "Year",
       y = "Annual Average Temperature",
       color = "Group ID") +
  theme_minimal()

# Plotting for N_suhu_all
ggplot(N_suhu_yearly_temp, aes(x = YEAR, y = Annual_Avg_Temp, color = as.factor(Group_ID))) +
  geom_line() +
  labs(title = "Annual Average Temperature for Each Group in N_suhu_all",
       x = "Year",
       y = "Annual Average Temperature",
       color = "Group ID") +
  theme_minimal()

# Plotting for S_suhu_all
ggplot(S_suhu_yearly_temp, aes(x = YEAR, y = Annual_Avg_Temp, color = as.factor(Group_ID))) +
  geom_line() +
  labs(title = "Annual Average Temperature for Each Group in S_suhu_all",
       x = "Year",
       y = "Annual Average Temperature",
       color = "Group ID") +
  theme_minimal()


##################### Per Country - drop Lat Lon Pair

# Function to aggregate temperature by year (ignoring group_id)
aggregate_temperature <- function(df, country_name) {
  df %>%
    group_by(Year) %>%
    summarize(Temperature = mean(annual_temp, na.rm = TRUE)) %>%
    mutate(Country = country_name)
}

#### Data Visualisation - for Latex
