# Shiny Resilience Dashboard 

This repository contains the code and data for my Master's Thesis project's interactive dashboard. The dashboard visually represents the resilience index and sub-indices I developed for Berlin's Neukölln.

## Overview

The project employs geovisualization tools to enhance scientific communication and critical thinking. It provides an interactive geovisualization tool to explore resilience indices, sub-indices, and individual indicators. The tool facilitates decision-making for public health authorities, offering a baseline for assessing resilience to emergencies, enabling location comparisons, identifying specific resilience factors in neighborhoods, and tracking improvements over time.

## Getting started
### Prerequisites

Make sure you have the following R packages installed. You can install them using the following commands in your R console:

```R
# Install required packages
install.packages("shiny")
install.packages("bslib")
install.packages("sf")
install.packages("leaflet")
install.packages("DT")
install.packages("ggplot2")
install.packages("tidyquant")
install.packages("tools")
install.packages("rgdal")
install.packages("RColorBrewer")
devtools::install_github("ricardo-bion/ggradar", 
                          dependencies = TRUE)
```

### Running the App
To run the app, either open the project in RStudio and press `Run App` or run it in CLI:
```bash
Rscript app.R
```

## Data
The data used in this project was obtained from publicly available sources and has been aggregated and processed. 




