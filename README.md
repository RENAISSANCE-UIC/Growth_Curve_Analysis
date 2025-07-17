## Microbial Growth Curve Analysis

A comprehensive R package for analyzing growth curve data from 96-well plate readers, with support for multiple plate reader formats and data correction methods, as well as custom plate designs.

## Features

-   **Multi-format support**: Current version works with Synergy HTX (Excel/CSV) and Cerillo CSV files
-   **Automatic plate reader detection**: Automatically identifies file format
-   **Multiple correction methods**: Traditional, robust, and standard contamination-safe corrections
-   **Custom plate layouts**: Design layouts with interactive Shiny interface or CSV import
-   **Multi-wavelength analysis**: Process data from one or two wavelengths simultaneously (Synergy HTX output)
-   **Comprehensive visualization**: Growth curves, plate heatmaps, and quality control plots
-   **Data export**: Multiple formats for downstream analysis

## Dependencies

``` r
# Core packages
library(tidyverse)  
library(lubridate)  
library(readxl)  
library(janitor)  
library(patchwork)    

# Optional (for enhanced features)  
library(writexl)  # For Excel export  
library(shiny)    # For interactive layout editor  
library(readr)    # For CSV operations  
```

## Quick Start

### Basic Analysis

``` r
# Set up workspace (optional)  
setup_workspace()    

# Run analysis with default settings  
results <- analyze_growth_curves(\"your_data_file.xlsx\")    

# View results  
show_growth_curves(results)  
show_plate_endpoint(results)  
```

### With Custom Layout

``` r
# Create custom layout interactively  
layout_data <- launch_plate_editor()    

# Or load from CSV  
results <- analyze_growth_curves("data.xlsx", layout_csv = "custom_layout.csv") 
```

## Configuration

### Set Working Directory

``` r
# Default location  
setup_workspace()    

# Custom location  
setup_workspace("/path/to/your/project")    

# Different project name  
setup_workspace(project_subdir = "My_Growth_Curves")
```

## Examples

### Complete Workflow

``` r
# 1. Set up workspace  
setup_workspace()    

# 2. Run analysis  
results <- run_standard_analysis("experiment_data.xlsx")    

# 3. View results  
show_growth_curves(results)  
show_plate_endpoint(results)    

# 4. Export data  
colleague_data <- get_colleague_format(results, export_to_csv = TRUE)    

# 5. Quality control  
show_qc_plots(results
```

### Custom Layout Workflow

``` r
# 1. Create custom layout  
layout_data <- launch_plate_editor()    

# 2. Run analysis with custom layout  
results <- analyze_growth_curves("data.xlsx", layout_csv = "plate_layout.csv")   

# 3. Verify layout  verify_experimental_layout(results$processed_data)
```

### Compare Correction Methods

``` r
# Compare all correction methods  
all_methods <- compare_correction_methods("data.xlsx") 

# Access specific method results  
show_growth_curves(all_methods$standard)  
show_growth_curves(all_methods$robust)  
```

## Correction Methods

### Standard Method (Recommended)

-   **Contamination-safe**: Detects and excludes contaminated broth controls
-   **Time-matched NP correction**: Accounts for nanoparticle kinetics
-   **Robust statistics**: Uses median-based corrections

``` r
results <- run_standard_analysis("data.xlsx")
```

### Robust Method

-   **Outlier detection**: Identifies and excludes problematic wells
-   **Median-based corrections**: Less sensitive to extreme values
-   **Enhanced quality control**: Multiple validation steps

``` r
results <- run_robust_analysis("data.xlsx")
```

### Traditional Method

-   **Simple approach**: Single broth well correction
-   **Raw NP values**: Direct nanoparticle subtraction
-   **No thresholding**: Allows negative values

``` r
results <- run_traditional_analysis("data.xlsx")
```

## Data Export

### For Colleagues

``` r
# Export in wide format    
colleague_data <- get_colleague_format(results)    
write_csv(colleague_data$samples, "samples.csv")    
write_csv(colleague_data$untreated, "untreated.csv")
```

### Specific Concentrations

``` r
# Extract specific concentration  data  
conc_data <- get_concentration_data(results, "Conc_5")  
plot_concentration(results, "Conc_5", style = "with_ribbons")
```

### Multiple Formats

``` r
# Wide format for analysis  
wide_data <- export_wide_format(results)    

# Raw data  
raw_data <- get_raw_data(results)    

# Corrected data  
corrected_data <- get_corrected_data(results)
```

## Visualization

### Growth Curves

``` r
# Basic growth curves  
show_growth_curves(results)    

# Specific concentrations  
show_concentrations_panel(results)  
plot_concentration(results, "Conc_10", style = "separated")
```

### Plate Layouts

``` r
# Final plate layout
show_plate_endpoint(results)    

# Composite comparison  
show_plate_composite(results)    

# Initial conditions  
show_plate_initial_condition(results)
```

### Quality Control

``` r
# QC plots  
show_qc_plots(results)    

# Layout verification  
verify_experimental_layout(results$processed_data)
```

## Multi-Wavelength Analysis (HTX Output Only)

``` r
# Process multi-wavelength data  
multi_results <- analyze_growth_curves_multi("data.xlsx")    

# Access specific wavelengths  
show_growth_curves(multi_results, wavelength = "600")  
show_growth_curves(multi_results, wavelength = "420")    

# Export specific wavelength  
data_600nm <- get_colleague_format(multi_results, wavelength = "600")
```

## Interactive Layout Editor

``` r
# Launch interactive editor  l
ayout_data <- launch_plate_editor()   

# Use captured layout  
results <- analyze_growth_curves("data.xlsx", layout_csv = "captured_layout.csv")
```

## File Format Support

### Synergy HTX

-   **Excel files**: `.xlsx`, `.xls`
-   **CSV files**: Exported from BioTek Synergy software
-   **Multi-wavelength**: Automatic detection of wavelength sections

### Cerillo

-   **CSV format**: Direct export from Cerillo software
-   **Single wavelength**: 600nm optical density (assumed)
-   **Timestamp support**: Unix timestamps and formatted dates

## Output Files

The package can generate several output files: - **Growth curve data**: CSV files with processed data - **Summary reports**: Excel files with comprehensive statistics - **Plot exports**: PNG files of visualizations - **Layout files**: CSV files defining plate layouts

## Support

For issues with specific file formats or analysis methods, use the diagnostic functions: - `detect_plate_reader_with_preview()` for file format issues - `diagnose_data_issues()` for data quality problems - `verify_experimental_layout()` for layout verification

## Additional Features

## Default Experimental Layout

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

### Summaries and Diagnostics

``` r
# Full analysis with summaries  
results <- analyze_growth_curves_with_summaries("data.xlsx")    

# View specific summary tables  
view_summary_table(results$summaries, "concentration_effects")  
view_summary_table(results$summaries, "quality_metrics")
```

### Custom Workflows

``` r
# Custom analysis with specific method  
results <- run_analysis("data.xlsx", method = "standard", export_summaries = TRUE)    

# Manual override for difficult files  
results <- force_plate_reader_type("data.csv", "cerillo")
```

### Data Exploration

``` r
# Quick data check  
quick_check(results)    

# Browse concentrations  
browse_concentrations(results)    

# Check replicate structure  
check_replicate_structure(results)
```

## Troubleshooting

### File Format Issues

``` r
# Check file format  
detect_plate_reader_with_preview("data.csv")    

# Force specific format if needed  
results <- force_plate_reader_type("data.csv", "cerillo")
```

### Layout Problems

``` r
# Verify layout  
verify_experimental_layout(results$processed_data)    

# Validate custom layout  
validate_layout_file("custom_layout.csv")
```

### Data Quality

``` r
# Run diagnostics 
diagnose_data_issues(results$corrected_results)    

# Explore data structure  
explore_data(results$corrected_results)
```
