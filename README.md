# Synergy HTX Plate Reader Analysis Pipeline

A comprehensive R package for analyzing growth curve data from BioTek
Synergy HTX plate readers, supporting both Excel (.xlsx) and CSV file
formats with single or multi-wavelength measurements.

## Features

-   **Universal File Support:** Process both Excel (.xlsx, .xls) and CSV
    files

-   **Multi-Wavelength Analysis:** Automatic detection and processing of
    multiple wavelengths (e.g., 600nm, 420nm)

-   **Automated Corrections:** Built-in broth blank and nanoparticle
    interference corrections

-   **96-Well Plate Layout:** Pre-configured experimental layouts with
    biological replicates

-   **Quality Control:** QC plots and data validation

-   **Flexible Export:** Multiple export formats for downstream analysis

-   **Rich Visualizations:** Growth curves, plate heatmaps, and
    comparative plots

## Experimental Layout

```{=html}
<pre>
     01  02  03  04  05  06  07  08  09  10  11  12
 A   B   S1  S2  S3  S4  S5  S6  S7  S8  S9  S10 B
 B   B   S1  S2  S3  S4  S5  S6  S7  S8  S9  S10 B  
 C   B   S1  S2  S3  S4  S5  S6  S7  S8  S9  S10 B
 D   B   N1  N2  N3  N4  N5  N6  N7  N8  N9  N10 B
 E   B   BL  BL  BL  BL  BL  BL  BL  BL  BL  BL  B
 F   B   BL  BL  BL  BL  BL  BL  BL  BL  BL  BL  B
 G   B   BL  BL  BL  BL  BL  BL  BL  BL  BL  BL  B
 H   B   U1  U2  U3  U4  U5  U6  U7  U8  U9  U10 B
</pre>
```

## Installation Prerequisites

Install required R packages:

``` r
install.packages(c( 
  "tidyverse", "lubridate", "readxl", "janitor", 
  "patchwork", "writexl", "viridis" )) 
```

## Setup

1.  Clone or download this repository

2.  Source the main script:

``` R
source("BioTek_Synergy_HTX_Processing.R")
```

3.  Set up your workspace (optional):

``` R
# Creates a project directory on your Desktop 
workspace <- setup_workspace()
```

## Quick Start

**Basic Analysis**

``` r
# Analyze your data file (Excel or CSV)
results <- analyze_growth_curves("your_data_file.xlsx")

# For CSV files, it works exactly the same
results <- analyze_growth_curves("your_data_file.csv")

# Get basic data summary
quick_check(results)

# Check data quality
show_qc_plots(results)
```

**Multi-Wavelength Data**

``` r
# Multi-wavelength analysis
results <- analyze_growth_curves("multi_wavelength_data.csv")

# Access individual wavelengths
show_growth_curves(results, wavelength = "600")
show_growth_curves(results, wavelength = "420")

# View plate layouts
show_plate_endpoint(results, wavelength = "600")
show_plate_endpoint(results, wavelength = "420")
```

**Visualization Examples**

``` r
# Main growth curves
show_growth_curves(results)

# Individual concentrations
show_concentration_plot(results, "Conc_5")

# Multiple concentrations comparison
show_multiple_concentrations(results, c("Conc_1", "Conc_5", "Conc_10"))

# All concentrations panel
show_concentrations_panel(results)

# Final plate heatmap
show_plate_endpoint(results)

# Initial conditions
show_plate_initial_condition(results)

# Before/after correction comparison
show_plate_composite(results)

# All QC plots
show_qc_plots(results)

# Layout validation
show_layout_validation(results)
```

**Data Export**

``` r
# Export in wide format for Excel/GraphPad
colleague_data <- get_colleague_format(results)

# Export to CSV files
colleague_data <- get_colleague_format(results, export_to_csv = TRUE)

# Multi-wavelength export
colleague_data_600 <- get_colleague_format(results, wavelength = "600")
colleague_data_420 <- get_colleague_format(results, wavelength = "420")

# Get specific concentration data
conc5_data <- get_concentration_data(results, "Conc_5")

# Multiple concentrations
multi_conc <- get_multiple_concentrations(results, c("Conc_1", "Conc_5", "Conc_10"))

# Raw data access
raw_data <- get_raw_data(results)
corrected_data <- get_corrected_data(results)
```
