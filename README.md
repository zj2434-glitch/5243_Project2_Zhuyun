# Project 2: Interactive Data Cleaning, Feature Engineering, and EDA Studio

**Authors:** Zhuyun Jin (`zj2434`) and Megan Wang (`mw3856`)

## Overview

This project is an interactive **R Shiny** application for loading, cleaning, transforming, exploring, and exporting datasets through a user-friendly web interface. The app is designed for users who want a complete mini data workflow without writing code.

The application supports multiple file formats, built-in sample datasets, interactive preprocessing tools, feature engineering utilities, exploratory data analysis, and downloadable output.

## Live Application

Deployed app: <https://zhuyunj.shinyapps.io/5243_project2/>

## GitHub Repository

Repository: <https://github.com/zj2434-glitch/5243_Project2_Zhuyun>

## Main Features

### 1. Data Loading
The app allows users to load datasets in several ways:

- Upload **CSV**
- Upload **Excel (.xlsx / .xls)**
- Upload **JSON**
- Upload **RDS**
- Load built-in datasets:
  - `iris`
  - `mtcars`

After loading data, the app displays:

- number of rows
- number of columns
- total missing values
- column type summary
- preview of the dataset

### 2. Data Cleaning and Preprocessing
The preprocessing tab includes several interactive tools:

- Remove duplicate rows
- Standardize column names
- Handle missing values:
  - drop missing rows
  - impute numeric columns using mean
  - impute numeric columns using median
- Scale numeric variables using z-score standardization
- Convert character columns to factors
- One-hot encode selected categorical variables
- Convert a selected column to:
  - factor
  - numeric
- Remove outliers from selected numeric columns using the IQR rule

The processed dataset preview and summary update dynamically after transformations.

### 3. Feature Engineering
The feature engineering tab allows users to:

- Create a new variable from two numeric columns using:
  - sum
  - difference
  - product
  - ratio
- Rename existing columns
- Remove selected columns

The engineered data preview updates automatically so users can inspect the results of each change.

### 4. Exploratory Data Analysis
The EDA tab supports several interactive plot types:

- Histogram
- Boxplot
- Scatterplot
- Correlation heatmap

Users can dynamically choose variables for visualization and optionally apply filters before plotting:

- numeric filters through value ranges
- categorical filters through selected levels

A matching text summary is shown below the plots to report descriptive statistics or correlation information.

### 5. Export
The final transformed dataset can be downloaded as a **CSV** file.

---

## File Structure

This repository contains the following main files:

- `app.R` — final Shiny application file
- `README.md` — project documentation

**Note:** `app.R` is the final version that should be used to run the application locally.

---

## Packages Required

The application uses the following R packages:

- `shiny`
- `bslib`
- `DT`
- `readr`
- `readxl`
- `jsonlite`
- `dplyr`
- `tidyr`
- `ggplot2`
- `janitor`
- `scales`
- `stringr`

If any package is missing, install it in R before running the app.

## How to Run the App Locally

### Option 1: Open in RStudio
1. Clone or download this repository.
2. Open `app.R` in RStudio.
3. Install any missing packages.
4. Click **Run App**.

### Option 2: Run from the R Console
```r
install.packages(c(
  "shiny", "bslib", "DT", "readr", "readxl", "jsonlite",
  "dplyr", "tidyr", "ggplot2", "janitor", "scales", "stringr"
))
source("app.R")