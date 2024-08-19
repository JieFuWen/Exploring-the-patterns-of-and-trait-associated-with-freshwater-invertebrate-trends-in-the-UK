# Install and load the required R packages
library(ggplot2)

# Import data
data <- read.csv("D:/BaiduNetdiskDownload/1970-2015/Species_Trends.csv")


library(ggplot2)
library(dplyr)
library(forecast)
library(vegan)
library(broom)


# View data structures
str(data)

# Calculate annual population changes for each species
calculate_annual_counts <- function(first_year, last_year, growth_rate, n_records) {
  years <- seq(first_year, last_year, by=1)
  annual_counts <- numeric(length(years))
  annual_counts[1] <- n_records
  for (i in 2:length(years)) {
    annual_counts[i] <- annual_counts[i-1] * (1 + growth_rate / 100)
  }
  return(data.frame(Year=years, Count=annual_counts))
}

# Generate a data frame of population changes per species per year
annual_counts_data <- data.frame()
for (i in 1:nrow(data)) {
  species_data <- calculate_annual_counts(data$First_Year[i], data$Last_Year[i], data$Mean_growth_rate[i], data$N_Records[i])
  species_data$Species <- data$Species[i]
  annual_counts_data <- rbind(annual_counts_data, species_data)
}

# Trend analysis using linear models
trend_analysis <- annual_counts_data %>%
  group_by(Species) %>%
  do(tidy(lm(Count ~ Year, data = .)))

# View results of trend analyses
print(trend_analysis)

# Define the plotting function
plot_species_trend <- function(data, species_name) {
  species_data <- filter(data, Species == species_name)
  species_name <- str_replace_all(species_name, "[^[:alnum:]_]", "_") 
  p <- ggplot(species_data, aes(x=Year, y=Count)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +
    ggtitle(paste("Linear Trend for", species_name, "Over Time")) +
    xlab("Year") +
    ylab("Estimated Number of Records") +
    theme_minimal() +
    theme(legend.position = "bottom")
  return(p)
}

# Generate trend graphs for all species and save as PDF
save_all_species_trends <- function(data, output_dir) {
  species_list <- unique(data$Species)
  for (species in species_list) {
    p <- plot_species_trend(data, species)
    clean_species_name <- str_replace_all(species, "[^[:alnum:]_]", "_")   # Remove or replace special characters
    output_path <- file.path(output_dir, paste0(clean_species_name, "_Linear_Trend.pdf"))
    ggsave(filename = output_path, plot = p, width = 12, height = 8, dpi = 300)
  }
}
# Set up working directories
output_directory <- "D:/Desktop/pdf_plots/Species_Linear_Trends"

# Assuming that annual_counts_data has been generated
save_all_species_trends(annual_counts_data, output_directory)




# Calculate the Shannon Diversity Index
calculate_shannon_index <- function(data) {
  data %>%
    group_by(Year) %>%
    summarize(shannon_index = diversity(Count, index="shannon"))
}

# Calculate the Shannon Diversity Index for each year
shannon_index_data <- calculate_shannon_index(annual_counts_data)

# View Shannon Diversity Index data
print(shannon_index_data)

# Mapping trends in the Shannon Diversity Index over time
plot_shannon_trends <- function(shannon_data) {
  p <- ggplot(shannon_data, aes(x=Year, y=shannon_index)) +
    geom_line(color="blue") +
    geom_point(color="red") +
    ggtitle("Shannon Diversity Index Over Time") +
    xlab("Year") +
    ylab("Shannon Diversity Index") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.title.x = element_text(face = "bold"), 
          axis.title.y = element_text(face = "bold"))
  print(p)
  ggsave(filename = "Shannon_Diversity_Index_Over_Time.pdf", plot = p, width = 12, height = 8, dpi = 300)
}


# Plotting trends in the Shannon Diversity Index over time
plot_shannon_trends(shannon_index_data)

# Time series analysis and forecasting
forecast_species_trend <- function(species_data) {
  ts_data <- ts(species_data$Count, start=min(species_data$Year), frequency=1)
  fit <- auto.arima(ts_data)
  forecasted <- forecast(fit, h=10)  # Forecasting the next 10 years
  return(forecasted)
}

# All species analysed and projected
all_forecasts <- list()
for (i in 1:nrow(data)) {
  species_data <- calculate_annual_counts(data$First_Year[i], data$Last_Year[i], data$Mean_growth_rate[i], data$N_Records[i])
  forecasted <- forecast_species_trend(species_data)
  all_forecasts[[data$Species[i]]] <- forecasted
}

# Plotting predictions
plot_forecast <- function(species_name, forecasted) {
  p <- autoplot(forecasted) +
    ggtitle(paste("Forecast for", species_name)) +
    xlab("Year") +
    ylab("Estimated Number of Records") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.title.x = element_text(face = "bold"), 
          axis.title.y = element_text(face = "bold"))
  print(p)
  ggsave(filename = paste0("Forecast_", species_name, ".pdf"), plot = p, width = 12, height = 8, dpi = 300)
}


library(stringr)


# Plotting predictions using the first species as an example
plot_forecast(names(all_forecasts)[1], all_forecasts[[1]])

# Species with the highest number of statistical records
top_species <- data[order(-data$N_Records), ][1:10, ]  # Take the top 10 species with the highest number of records
# Species showing the highest number of records
print(top_species)

# Mapping of trends in the number of species with the highest number of records
plot_top_species_trends <- function(top_species, annual_counts_data) {
  top_species_names <- top_species$Species
  for (species in top_species_names) {
    species_data <- filter(annual_counts_data, Species == species)
    p <- ggplot(species_data, aes(x=Year, y=Count)) +
      geom_line(color="green") +
      geom_point(color="orange") +
      geom_smooth(method="lm", col="red") +
      ggtitle(paste("Trend for", species)) +
      xlab("Year") +
      ylab("Estimated Number of Records") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5), 
            axis.title.x = element_text(face = "bold"), 
            axis.title.y = element_text(face = "bold"))
    print(p)
    ggsave(filename = paste0("Top_Species_Trend_", species, ".pdf"), plot = p, width = 12, height = 8, dpi = 300)
  }
}

# :: Plotting trends in the populations of the species with the highest number of records
plot_top_species_trends(top_species, annual_counts_data)




# Calculate total volume per year
total_counts_data <- annual_counts_data %>%
  group_by(Year) %>%
  summarize(Total_Count = sum(Count))

# Mapping trends in overall volume changes
total_counts_data <- annual_counts_data %>%
  group_by(Year) %>%
  summarize(Total_Count = sum(Count))

plot_total_counts_trend <- function(total_counts_data) {
  p <- ggplot(total_counts_data, aes(x=Year, y=Total_Count)) +
    geom_line(color="blue") +
    geom_point(color="red") +
    ggtitle("Total Species Counts Over Time") +
    xlab("Year") +
    ylab("Total Number of Records") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), 
          axis.title.x = element_text(face = "bold"), 
          axis.title.y = element_text(face = "bold"))
  print(p)
  ggsave(filename = "Total_Species_Counts_Over_Time.pdf", plot = p, width = 12, height = 8, dpi = 300)
}

# Mapping trends in overall volume changes
plot_total_counts_trend(total_counts_data)

# Install and load the required R packages

library(ggplot2)
library(dplyr)
library(readr)

# Define the data catalogue
data_dir <- "D:/BaiduNetdiskDownload/1970-2015/SUMMARY_TABLES"

# Get a list of all CSV files
files <- list.files(data_dir, pattern = "*.csv", full.names = TRUE)

# Define a function to process each file and generate charts
process_file <- function(file_path) {
  # Read the data
  redata <- read_csv(file_path)
  
  # Extract species name as part of filename
  species_name <- basename(file_path)
  species_name <- gsub("AnnualSpeciesOccupancy_SummaryTable_", "", species_name)
  species_name <- gsub(".csv", "", species_name)
  
  # Mapping changes in the location of species in different regions
  plot <- ggplot(redata, aes(x=Year, y=Mean, color=Region)) +
    geom_line() +
    geom_point() +
    facet_wrap(~ Species, scales = "free") +
    ggtitle(paste("Species Distribution Over Years and Regions:", species_name)) +
    xlab("Year") +
    ylab("Mean Occupancy") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Save charts as high-resolution PDFs
  ggsave(filename = paste0("pdf_plots/", species_name, "_distribution.pdf"), plot = plot, width = 12, height = 8, dpi = 300)
}

# Create a directory to save charts
if (!dir.exists("pdf_plots")) {
  dir.create("pdf_plots")
}
# Iterate over all documents and generate charts
lapply(files, process_file)







###----new six_groups files
library(ggplot2)
library(dplyr)
library(readr)

# Define the data catalogue
data_dir <- "D:/BaiduNetdiskDownload/1970-2015/SUMMARY_TABLES"

# Get a list of all CSV files
files <- list.files(data_dir, pattern = "*.csv", full.names = TRUE)

# Define a function to extract the group name
get_group_name <- function(file_path) {
  species_name <- basename(file_path)
  species_name <- gsub("AnnualSpeciesOccupancy_SummaryTable_", "", species_name)
  group_name <- unlist(strsplit(species_name, "_"))[1]
  return(group_name)
}
# Define groups of interest
groups_of_interest <- c("AquaticBugs", "Caddisflies", "Dragonflies", "Mayflies", "NonmarineMolluscs", "Stoneflies")
# Create a list to store all the data
all_data <- list()
# Read all files and extract group names
for (file in files) {
  group_name <- get_group_name(file)
  if (group_name %in% groups_of_interest) {
    redata <- read_csv(file)
    redata$Group <- group_name
    all_data[[length(all_data) + 1]] <- redata
  }
}
# Merge all data
combined_data <- bind_rows(all_data)

# Extract data for the six groups of interest from the merged data
filtered_data <- combined_data %>% filter(Group %in% groups_of_interest)

# Calculate the mean of different species in the same year in each broad category
grouped_data <- filtered_data %>%
  group_by(Group, Year, Region) %>%
  summarize(Mean_Occupancy = mean(Mean, na.rm = TRUE)) %>%
  ungroup()
write.csv(filtered_data,file = 'six_group.csv')
# Traverse each group and generate charts
unique_groups <- unique(grouped_data$Group)
for (group in unique_groups) {
  group_data <- grouped_data %>% filter(Group == group)
  
  # Ensure that year data are integers and sorted by region and year
  group_data <- group_data %>% mutate(Year = as.integer(Year)) %>% arrange(Region, Year)
  
  # Plotting the distribution of groups over time in different regions
  plot <- ggplot(group_data, aes(x = Year, y = Mean_Occupancy, color = Region)) +
    geom_line() +
    geom_point() +  
    ggtitle(paste("Group Distribution Over Years by Region:", group)) +
    xlab("Year") +
    ylab("Mean Occupancy") +
    theme_minimal() +
    theme(legend.position = "none") 
  
  # save
  ggsave(filename = paste0("pdf_plots/", group, "_distribution_by_region2.pdf"), plot = plot, width = 12, height = 8, dpi = 300)
}
getwd()


