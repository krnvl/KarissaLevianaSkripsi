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

##################################### Function to Process Data for Each Dataset

process_suhu_data <- function(suhu_data, dataset_name) {
  
  # Drop unnecessary columns
  suhu_data <- subset(suhu_data, select = -c(PARAMETER, ANN))
  
  # Adjust year range
  suhu_data <- subset(suhu_data, YEAR >= 1984 & YEAR <= 2023)
  
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
  
  return(yearly_temp)
}

# Process each dataset
D_suhu_yearly_temp <- process_suhu_data(Dsuhu, "D_suhu_all")
F_suhu_yearly_temp <- process_suhu_data(Fsuhu, "F_suhu_all")
N_suhu_yearly_temp <- process_suhu_data(Nsuhu, "N_suhu_all")
S_suhu_yearly_temp <- process_suhu_data(Ssuhu, "S_suhu_all")

print(D_suhu_yearly_temp)


# Save each cleaned dataset to a CSV file
# write.csv(D_suhu_yearly_temp, "Temperature in Group/D_suhu_all_clean_data.csv", row.names = FALSE)
# write.csv(F_suhu_yearly_temp, "Temperature in Group/F_suhu_all_clean_data.csv", row.names = FALSE)
# write.csv(N_suhu_yearly_temp, "Temperature in Group/N_suhu_all_clean_data.csv", row.names = FALSE)
# write.csv(S_suhu_yearly_temp, "Temperature in Group/S_suhu_all_clean_data.csv", row.names = FALSE)


##################### Per Country - drop Lat Lon Pair

# Aggregate mean temperature for each dataset
D_suhu_agg <- D_suhu_yearly_temp %>%
  group_by(YEAR) %>%
  summarize(Mean_Annual_Temp = mean(Annual_Avg_Temp, na.rm = TRUE))

F_suhu_agg <- F_suhu_yearly_temp %>%
  group_by(YEAR) %>%
  summarize(Mean_Annual_Temp = mean(Annual_Avg_Temp, na.rm = TRUE))

S_suhu_agg <- S_suhu_yearly_temp %>%
  group_by(YEAR) %>%
  summarize(Mean_Annual_Temp = mean(Annual_Avg_Temp, na.rm = TRUE))

N_suhu_agg <- N_suhu_yearly_temp %>%
  group_by(YEAR) %>%
  summarize(Mean_Annual_Temp = mean(Annual_Avg_Temp, na.rm = TRUE))

# Print the first few rows of each aggregated dataset
head(D_suhu_agg)
head(F_suhu_agg)
head(S_suhu_agg)
head(N_suhu_agg)



#### Data Visualisation - for Latex
library(ggplot2)
library(dplyr)

# Add a country column to each dataset
D_suhu_agg <- D_suhu_agg %>% mutate(Country = "Denmark")
F_suhu_agg <- F_suhu_agg %>% mutate(Country = "Finlandia")
S_suhu_agg <- S_suhu_agg %>% mutate(Country = "Swedia")
N_suhu_agg <- N_suhu_agg %>% mutate(Country = "Norwegia")

# Combine all datasets into one
combined_suhu <- bind_rows(D_suhu_agg, F_suhu_agg, S_suhu_agg, N_suhu_agg)

# Plot the annual temperature trends with different line types
ggplot(combined_suhu, aes(x = YEAR, y = Mean_Annual_Temp, 
                          color = Country, linetype = Country, group = Country)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash")) +
  labs(title = "Rataan Suhu Tahunan Antar Negara",
       x = "Tahun",
       y = "Rataan Suhu Tahunan (°C)",
       color = "Negara",
       linetype = "Negara") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    #axis.title.x = element_text(face = "italic"),
    #axis.title.y = element_text(face = "italic"),
    legend.position = "bottom"
  )


####### Box Plot

ggplot(combined_suhu, aes(x = Country, y = Mean_Annual_Temp, fill = Country)) +
  geom_boxplot() +
  labs(
    title = "Distribusi Suhu Tahunan Antar Negara",
    x = "Negara",
    y = "Rataan Suhu Tahunan (°C)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    #axis.title.x = element_text(face = "italic"),
    #axis.title.y = element_text(face = "italic")
  ) +
  scale_fill_manual(values = c("Denmark" = "skyblue", 
                               "Finlandia" = "lightgreen", 
                               "Swedia" = "plum", 
                               "Norwegia" = "lightcoral"))


################################ Plot Per Lat-Lon Pair

ggplot(D_suhu_yearly_temp, aes(x = YEAR, y = Annual_Avg_Temp, color = as.factor(Group_ID))) +
  geom_line() +
  labs(title = "Annual Average Temperature for Each Group in D_suhu_all",
       x = "Year",
       y = "Annual Average Temperature",
       color = "Group ID") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "italic"),
    axis.title.x = element_text(face = "italic"),
    axis.title.y = element_text(face = "italic")
  )

ggplot(F_suhu_yearly_temp, aes(x = YEAR, y = Annual_Avg_Temp, color = as.factor(Group_ID))) +
  geom_line() +
  labs(title = "Annual Average Temperature for Each Group in F_suhu_all",
       x = "Year",
       y = "Annual Average Temperature",
       color = "Group ID") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "italic"),
    axis.title.x = element_text(face = "italic"),
    axis.title.y = element_text(face = "italic")
  )

ggplot(N_suhu_yearly_temp, aes(x = YEAR, y = Annual_Avg_Temp, color = as.factor(Group_ID))) +
  geom_line() +
  labs(title = "Annual Average Temperature for Each Group in N_suhu_all",
       x = "Year",
       y = "Annual Average Temperature",
       color = "Group ID") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "italic"),
    axis.title.x = element_text(face = "italic"),
    axis.title.y = element_text(face = "italic")
  )

ggplot(S_suhu_yearly_temp, aes(x = YEAR, y = Annual_Avg_Temp, color = as.factor(Group_ID))) +
  geom_line() +
  labs(title = "Annual Average Temperature for Each Group in S_suhu_all",
       x = "Year",
       y = "Annual Average Temperature",
       color = "Group ID") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "italic"),
    axis.title.x = element_text(face = "italic"),
    axis.title.y = element_text(face = "italic")
  )


####################################### 2024 Annual Temperature For Prediction


# Define the function to process temperature data for 2024 only
process_temp_2024 <- function(suhu_data) {
  suhu_data %>%
    select(-PARAMETER, -ANN) %>%
    filter(YEAR == 2024) %>%
    {
      lat_lon_groups <- distinct(., LAT, LON) %>%
        mutate(Group_ID = row_number())
      left_join(., lat_lon_groups, by = c("LAT", "LON"))
    } %>%
    group_by(Group_ID, YEAR) %>%
    summarise(Annual_Avg_Temp = mean(c_across(JAN:DEC), na.rm = TRUE), .groups = "drop")
}

# Apply to each dataset
D_2024_summary <- process_temp_2024(Dsuhu)
F_2024_summary <- process_temp_2024(Fsuhu)
N_2024_summary <- process_temp_2024(Nsuhu)
S_2024_summary <- process_temp_2024(Ssuhu)

# Function to compute the average annual temperature for 2024
get_avg_temp_2024 <- function(suhu_data) {
  suhu_data %>%
    filter(YEAR == 2024) %>%
    select(JAN:DEC) %>%
    rowMeans(na.rm = TRUE) %>%
    mean(na.rm = TRUE)
}

# Apply to each dataset
D_avg_2024 <- get_avg_temp_2024(Dsuhu)
F_avg_2024 <- get_avg_temp_2024(Fsuhu)
N_avg_2024 <- get_avg_temp_2024(Nsuhu)
S_avg_2024 <- get_avg_temp_2024(Ssuhu)

# Print results
D_avg_2024 # 9.943667
F_avg_2024 # 3.114943
N_avg_2024 # 5.295733
S_avg_2024 # 4.396873

