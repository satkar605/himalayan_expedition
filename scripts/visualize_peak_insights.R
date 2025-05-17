# Himalayan Expeditions Dashboard - Peak Insights Visualization
# This script creates the base visualizations for the Peak Insights module

# Set working directory to dataset location
setwd("/Users/satkarkarki/Desktop/Data_Analytics_Portfolio/himalyan_expedition/dataset")

# Install required packages if not already installed
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("leaflet")) install.packages("leaflet")
if (!require("sf")) install.packages("sf")
if (!require("tmap")) install.packages("tmap")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("bslib")) install.packages("bslib")

# Load required libraries
library(tidyverse)  # includes ggplot2, dplyr, tidyr, readr, and other tidyverse packages
library(leaflet)
library(sf)
library(tmap)
library(shinydashboard)
library(bslib)

# Load data
peaks <- read.csv("peaks.csv")
exped <- read.csv("exped.csv")
members <- read.csv("members.csv")
refer <- read.csv("refer.csv")

# Inspect dataset structure
cat("\nPeaks dataset columns:\n")
colnames(peaks)
cat("\nExpedition dataset columns:\n")
colnames(exped)
cat("\nMembers dataset columns:\n")
colnames(members)
cat("\nReferences dataset columns:\n")
colnames(refer)

# Data cleaning and preparation
# Remove rows with missing peak IDs and heights
peaks_clean <- peaks %>%
  filter(!is.na(peakid), !is.na(heightm))

# Join peaks with expeditions to get climb counts
peak_climbs <- exped %>%
  group_by(peakid) %>%
  summarise(climb_count = n()) %>%
  left_join(peaks_clean, by = "peakid") %>%
  arrange(desc(climb_count))

# 1. Top 10 Most Climbed Peaks
top_peaks_plot <- ggplot(head(peak_climbs, 10), 
                        aes(x = reorder(pkname, climb_count), y = climb_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Most Climbed Peaks",
       x = "Peak Name",
       y = "Number of Expeditions") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# 2. First Ascent Timeline
first_ascents <- exped %>%
  filter(success1 == TRUE) %>%  # Using success1 instead of success
  group_by(year) %>%  # Using year instead of pyear
  summarise(first_ascents = n()) %>%
  arrange(year)

first_ascents_plot <- ggplot(first_ascents, aes(x = year, y = first_ascents)) +
  geom_line(color = "darkgreen") +
  geom_point(color = "darkgreen") +
  labs(title = "Timeline of First Ascents",
       x = "Year",
       y = "Number of First Ascents") +
  theme_minimal()

# 3. Peak Height Distribution
height_dist_plot <- ggplot(peaks_clean, aes(x = heightm)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Peak Heights",
       x = "Height (meters)",
       y = "Count") +
  theme_minimal()

# 4. Peak Status Summary
status_summary <- peaks_clean %>%
  group_by(pstatus) %>%  # Using pstatus instead of status
  summarise(count = n()) %>%
  mutate(pstatus = factor(pstatus, 
                        levels = c("open", "restricted", "trekking", "unlisted")))

status_plot <- ggplot(status_summary, aes(x = pstatus, y = count)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Peak Status Distribution",
       x = "Status",
       y = "Number of Peaks") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save plots (optional)
# ggsave("top_peaks.png", top_peaks_plot)
# ggsave("first_ascents.png", first_ascents_plot)
# ggsave("height_dist.png", height_dist_plot)
# ggsave("status_summary.png", status_plot)

# Print summary statistics
cat("\nSummary Statistics:\n")
cat("Total number of peaks:", nrow(peaks_clean), "\n")
cat("Total number of expeditions:", nrow(exped), "\n")
cat("Total number of climbers:", nrow(members), "\n")

# Display plots
print(top_peaks_plot)
print(first_ascents_plot)
print(height_dist_plot)
print(status_plot) 