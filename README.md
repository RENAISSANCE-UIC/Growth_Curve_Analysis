
# Synergy HTX Plate Reader Analysis Pipeline

A comprehensive R package for analyzing growth curve data from BioTek Synergy HTX plate readers, supporting both Excel (.xlsx) and CSV file formats with single or multi-wavelength measurements.

## Features

Universal File Support: Process both Excel (.xlsx, .xls) and CSV files
Multi-Wavelength Analysis: Automatic detection and processing of multiple wavelengths (e.g., 600nm, 420nm)
Automated Corrections: Built-in broth blank and nanoparticle interference corrections
96-Well Plate Layout: Pre-configured experimental layouts with biological replicates
Quality Control: Comprehensive QC plots and data validation
Flexible Export: Multiple export formats for downstream analysis
Rich Visualizations: Growth curves, plate heatmaps, and comparative plots

## Experimental Layout
The pipeline assumes a standard 96-well plate layout:
Copy    01  02  03  04  05  06  07  08  09  10  11  12
A   B   S1  S2  S3  S4  S5  S6  S7  S8  S9  S10 B
B   B   S1  S2  S3  S4  S5  S6  S7  S8  S9  S10 B  
C   B   S1  S2  S3  S4  S5  S6  S7  S8  S9  S10 B
D   B   N1  N2  N3  N4  N5  N6  N7  N8  N9  N10 B
E   B   BL  BL  BL  BL  BL  BL  BL  BL  BL  BL  B
F   B   BL  BL  BL  BL  BL  BL  BL  BL  BL  BL  B
G   B   BL  BL  BL  BL  BL  BL  BL  BL  BL  BL  B
H   B   U1  U2  U3  U4  U5  U6  U7  U8  U9  U10 B
Legend:

B: Broth blank controls
S1-S10: Sample concentrations (3 biological replicates: A, B, C)
N1-N10: Nanoparticle-only controls
U1-U10: Untreated controls
BL: Blank controls

🛠️ Installation
Prerequisites
Install required R packages:
rCopyinstall.packages(c(
  "tidyverse", "lubridate", "readxl", "janitor", 
  "patchwork", "writexl", "viridis"
))
Setup

Clone or download this repository
Source the main script:

rCopysource("BioTek_Synergy_HTX_Processing.R")

Set up your workspace (optional):

rCopy# Creates a project directory on your Desktop
workspace <- setup_workspace()
📊 Quick Start
Basic Analysis
rCopy# Analyze your data file (Excel or CSV)
results <- analyze_growth_curves("your_data_file.xlsx")

# For CSV files, it works exactly the same
results <- analyze_growth_curves("your_data_file.csv")
Multi-Wavelength Data
The pipeline automatically detects multiple wavelengths:
rCopy# Multi-wavelength analysis
results <- analyze_growth_curves("multi_wavelength_data.csv")

# Access individual wavelengths
show_growth_curves(results, wavelength = "600")
show_growth_curves(results, wavelength = "420")

# View plate layouts
show_plate_endpoint(results, wavelength = "600")
show_plate_endpoint(results, wavelength = "420")
Quick Data Check
rCopy# Get basic data summary
quick_check(results)

# Check data quality
show_qc_plots(results)
📈 Visualization Examples
Growth Curves
rCopy# Main growth curves
show_growth_curves(results)

# Individual concentrations
show_concentration_plot(results, "Conc_5")

# Multiple concentrations comparison
show_multiple_concentrations(results, c("Conc_1", "Conc_5", "Conc_10"))

# All concentrations panel
show_concentrations_panel(results)
Plate Layouts
rCopy# Final plate heatmap
show_plate_endpoint(results)

# Initial conditions
show_plate_initial_condition(results)

# Before/after correction comparison
show_plate_composite(results)
Quality Control
rCopy# All QC plots
show_qc_plots(results)

# Layout validation
show_layout_validation(results)
📤 Data Export
For Collaborators
rCopy# Export in wide format for Excel/GraphPad
colleague_data <- get_colleague_format(results)

# Export to CSV files
colleague_data <- get_colleague_format(results, export_to_csv = TRUE)

# Multi-wavelength export
colleague_data_600 <- get_colleague_format(results, wavelength = "600")
colleague_data_420 <- get_colleague_format(results, wavelength = "420")
Custom Data Extraction
rCopy# Get specific concentration data
conc5_data <- get_concentration_data(results, "Conc_5")

# Multiple concentrations
multi_conc <- get_multiple_concentrations(results, c("Conc_1", "Conc_5", "Conc_10"))

# Raw data access
raw_data <- get_raw_data(results)
corrected_data <- get_corrected_data(results)
🔧 Advanced Usage
Custom Analysis Pipeline
rCopy# Full analysis with summaries and diagnostics
results <- analyze_growth_curves(
  "data.xlsx",
  export_summaries = TRUE,
  run_diagnostics = TRUE
)

# Manual step-by-step processing
processed_data <- process_synergy_file("data.xlsx")
corrected_results <- process_with_corrections(processed_data)
plots <- create_all_plots(corrected_results)
Troubleshooting
rCopy# Examine raw data structure
examine_raw_kinetic_data("data.xlsx")

# Check replicate structure
check_replicate_structure(results)

# Diagnose data issues
diagnose_data_issues(results$corrected_results)

# Verify experimental layout
verify_experimental_layout(results$processed_data)
📋 File Format Requirements
CSV Files

Time can be in HH:MM format or decimal hours
Wavelength sections marked with numeric values (e.g., "600", "420")
Standard 96-well plate column headers (A01, A02, ..., H12)

Excel Files

Compatible with BioTek Synergy HTX export format
Automatic handling of Excel date/time formatting
Multi-sheet support

🎯 Key Functions Reference
FunctionPurposeanalyze_growth_curves()Main analysis pipelinequick_check()Basic data summaryshow_growth_curves()Display growth curvesshow_plate_endpoint()Final plate heatmapshow_qc_plots()Quality control plotsget_colleague_format()Export data for collaboratorsplot_concentration()Individual concentration plotsshow_concentrations_panel()Multi-concentration comparison
🔍 Data Corrections Applied

Broth Blank Correction: Subtracts average broth control OD from all wells
Nanoparticle Interference Correction: Removes nanoparticle absorption from sample wells
Negative Value Protection: Ensures corrected values don't go below zero
Replicate Statistics: Calculates means, standard deviations, and standard errors

📊 Output Structure
rCopyresults$
├── processed_data/          # Raw processed data with layout
├── corrected_results/       # Data after corrections applied
├── plots/                   # All visualization objects
├── summaries/              # Statistical summaries (optional)
└── diagnostics/            # Data quality diagnostics (optional)

# Multi-wavelength results
results$
├── wavelength_600/         # 600nm analysis results
├── wavelength_420/         # 420nm analysis results
└── ...
🤝 Contributing

Fork the repository
Create a feature branch (git checkout -b feature/amazing-feature)
Commit your changes (git commit -m 'Add amazing feature')
Push to the branch (git push origin feature/amazing-feature)
Open a Pull Request

📄 License
This project is licensed under the MIT License - see the LICENSE file for details.
🙋 Support

Issues: Report bugs or request features via GitHub Issues
Documentation: Check the function documentation with ?function_name
Examples: See the examples/ directory for sample data and workflows

🔗 Related Projects

BioTek Synergy HTX Documentation
Plate Reader Data Analysis Best Practices

📝 Citation
If you use this pipeline in your research, please cite:
CopySynergy HTX Plate Reader Analysis Pipeline
https://github.com/yourusername/synergy-htx-analysis

Maintained by: [Your Name]
Last Updated: [Current Date]
Version: 1.0.0
