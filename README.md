# Morphology_melanarius

This repository contains the R scripts and dataset used to analyze the morphological traits (body size, body shape, and allometry) of *melanarius* across an urbanization gradient. 

The analytical pipeline processes morphometric data, performs Principal Component Analysis (PCA) to derive size and shape metrics, models responses to environmental drivers using Generalized Additive Models (GAMs), and evaluates sexual dimorphism using Reduced Major Axis (RMA) regression.

## Repository Contents

*   **`dataset.xlsx`**: The raw morphometric dataset.
*   **`Uploading excel dataset.R`**: Initial data loading, cleaning, and factor processing.
*   **`PCA loading.R`**: Dimensionality reduction to calculate body size (PC1) and body shape (PC2).
*   **`GAM.R`**: Main statistical modeling of environmental drivers and spatial variation.
*   **`RMA II.R`**: Reduced Major Axis regression to compare male and female morphological scaling.

## Dataset Structure (`dataset.xlsx`)

The `6_melanarius_data` sheet contains the following key columns:
*   **Location Metadata:** `Region`, `Admin`, `Site`, `Habitat.type`, `Y` (Latitude), `X` (Longitude)
*   **Environmental Variables:** `Anthropogen`, `Isolation`
*   **Morphometric Traits:** `Elytra.length`, `Elytra.width`, `Pronotum.length`, `Pronotum.width`, `Head.length`, `Eye.distance`
*   **Sex Variables:** `Sex` (M/F), `Predicted.sex` (Random Forest probability used as model weights)

## Prerequisites & Dependencies

To run the scripts in this repository, you will need **R** and the following packages installed:

```R
install.packages(c("readxl", "stringr", "dplyr", "tidyr", "mgcv", "gratia", "ggeffects", "ggplot2", "lmodel2"))
