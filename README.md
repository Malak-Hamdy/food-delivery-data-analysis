# Food Delivery Data Analysis

An interactive R Shiny application for exploring food-delivery data through data cleaning, exploratory data analysis, visualization, association-rule mining, and K-means clustering.

## Overview

This project explores food-delivery patterns and factors associated with delivery efficiency. It provides an interactive interface for examining the dataset, visualizing relationships between delivery-related variables, exploring association rules, and grouping orders using unsupervised learning.

The application originated as a learning project and has since received bug fixes and code cleanup to improve data handling and reproducibility.

## Features

- **Data cleaning:** Handles blank text values, missing-value filling, and delivery-time outlier filtering.
- **Exploratory data analysis:** Visualizes relationships between delivery time, distance, preparation time, traffic, weather, and other available variables.
- **Association-rule mining:** Uses the Apriori algorithm to explore relationships among categorized delivery conditions.
- **K-means clustering:** Groups orders using selected numerical features and visualizes the resulting clusters.
- **Interactive interface:** Built with R Shiny, allowing users to explore the data through an application interface.
- **CSV input:** Supports uploading a dataset with the required columns.

## Technologies

- R
- Shiny
- bslib
- ggplot2
- arules
- arulesViz

## Project Files

- `APP.R` — Main Shiny application.
- `Food_Delivery_Times.csv` — Dataset used by the application, if redistribution is permitted.

## Getting Started

### Prerequisites

Install R and the required R packages.

```r
install.packages(c(
  "shiny",
  "bslib",
  "ggplot2",
  "arules",
  "arulesViz"
))
```

### Run the application

1. Download or clone this repository.
2. Place `APP.R` and `Food_Delivery_Times.csv` in the same directory.
3. Open R or RStudio in that directory.
4. Run:

```r
shiny::runApp()
```

The application expects a CSV containing the columns required by its data-cleaning and analysis functions. Check the source code for the exact column names.

## Methodology

The application uses exploratory visualizations to examine delivery-related variables, the Apriori algorithm to discover association rules, and K-means clustering to group orders based on numerical features.

K-means is an unsupervised clustering method; it does not, by itself, predict whether a future order will be late.

## Limitations

- Results depend on the quality and suitability of the input dataset.
- The current implementation uses predefined delivery-time thresholds and selected clustering features.
- The association-rule workflow and complete browser interface should be tested in the target R environment before the application is considered fully verified.
- The project is intended for educational and exploratory analysis, not operational delivery-time prediction.

## Learning Objectives

This project provided practical experience with R, data preparation, exploratory data analysis, interactive Shiny applications, association-rule mining, and unsupervised machine learning.
