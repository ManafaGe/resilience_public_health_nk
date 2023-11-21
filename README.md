# Shiny Resilience GeoViz Tool 

This repository contains the code and data for the geovisualization tool developed for my Master's thesis. The tool visually represents the resilience index and sub-indices for the district Neukölln in Berlin in the context of public health emergencies.

## Overview

The project provides an interactive geovisualization tool to examine resilience indices, sub-indices, and individual indicators.  The tool can be used for exploration or to facilitate decision-making by health authorities. It can serve as an initial baseline to examine existing health-related resilience capacities, compare places of interest to one another, and determine specific factors of resilience across the neighborhoods.

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




