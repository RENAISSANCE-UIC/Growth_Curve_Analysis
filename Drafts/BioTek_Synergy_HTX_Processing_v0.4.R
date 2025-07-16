# ===== LIBRARIES =====
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(patchwork)

# ===== CONFIGURATION =====

#' Set Up the Working Directory for a Project
#'
#' This function sets the working directory for a project. If no directory is provided,
#' it defaults to a subdirectory on the user's Desktop named "Microbial_Growth_Curves_Analysis".
#' It ensures cross-platform compatibility by expanding the user's home directory.
#'
#' @param working_directory Optional. A character string specifying the full path to the desired working directory.
#' If `NULL`, the function constructs a default path using the user's home directory and the `project_subdir`.
#' @param project_subdir Optional. A character string specifying the name of the project subdirectory to use
#' when `working_directory` is not provided. Defaults to `"Microbial_Growth_Curves_Analysis"`.
#'
#' @return A character string representing the path to the working directory that was set.
#' @examples
#' \dontrun{
#' setup_workspace()
#' setup_workspace("/path/to/my/project")
#' setup_workspace(project_subdir = "My_Other_Project")
#' }
#' @export
setup_workspace <- function(working_directory = NULL, 
                            project_subdir = "Microbial_Growth_Curves_Analysis") 
  {
  if (is.null(working_directory)) {
    # Use the user's home directory as a base
    home_dir <- path.expand("~")
    working_directory <- file.path(home_dir, "Desktop", project_subdir)
  }
  
  if (!dir.exists(working_directory)) {
    stop("The specified working directory does not exist: ", working_directory)
  }
  
  setwd(working_directory)
  return(getwd())
}

# ===== CORE DATA READING FUNCTIONS =====

#' Read Synergy HTX Excel file and extract kinetic data
read_synergy_htx_file <- function(file_path, sheet_name = NULL) {
  
  cat("📖 Reading Synergy HTX file:", basename(file_path), "\n")
  
  # Read entire sheet - suppress Excel reading warnings
  all_data <- suppressMessages(suppressWarnings(
    if (is.null(sheet_name)) {
      read_excel(file_path, col_names = FALSE)
    } else {
      read_excel(file_path, sheet = sheet_name, col_names = FALSE)
    }
  ))
  
  # Extract metadata (rows 1-25)
  metadata <- extract_metadata(all_data[1:25, 1:10])
  
  # Find data boundaries
  data_boundaries <- find_data_boundaries(all_data)
  
  # Read kinetic data
  kinetic_data <- read_kinetic_data(file_path, sheet_name, data_boundaries)
  
  # Process and clean kinetic data
  kinetic_long <- process_kinetic_data(kinetic_data, metadata)
  
  cat("✅ Successfully read", nrow(kinetic_long), "data points from", 
      length(unique(kinetic_long$well_id)), "wells\n")
  
  return(list(
    metadata = metadata,
    kinetic_data = kinetic_long,
    experiment_start = create_experiment_datetime(metadata)
  ))
}

#' Create experiment datetime - SUPPRESSED WARNINGS
create_experiment_datetime <- function(metadata) {
  experiment_date <- metadata["Date"]
  experiment_time <- metadata["Time"]
  
  if (!is.na(experiment_date) && !is.na(experiment_time)) {
    suppressWarnings({
      tryCatch(
        ymd_hms(paste(experiment_date, experiment_time)),
        error = function(e) {
          # Only warn if this is really problematic
          cat("⚠️  Could not parse experiment datetime, using default\n")
          ymd_hms("2025-01-01 00:00:00")
        }
      )
    })
  } else {
    # Only warn if this might be problematic
    cat("ℹ️  Using default experiment start time\n")
    ymd_hms("2025-01-01 00:00:00")
  }
}

#' Extract and clean metadata from header rows
extract_metadata <- function(metadata_raw) {
  metadata <- tibble()
  
  for (i in 1:nrow(metadata_raw)) {
    row_data <- metadata_raw[i, ] %>% 
      unlist() %>% 
      na.omit() %>% 
      as.character()
    
    if (length(row_data) >= 2 && str_detect(row_data[1], "[A-Za-z]")) {
      param_name <- str_remove(row_data[1], ":$")
      param_value <- row_data[2]
      metadata <- bind_rows(metadata, 
                            tibble(parameter = param_name, value = param_value))
    }
  }
  
  # Clean metadata values (dates, times, etc.)
  metadata <- metadata %>%
    mutate(value = clean_metadata_values(parameter, value))
  
  # Convert to named list
  metadata_list <- metadata$value
  names(metadata_list) <- metadata$parameter
  
  return(metadata_list)
}

#' Clean metadata values (handle Excel date/time formatting)
clean_metadata_values <- function(parameter, value) {
  case_when(
    parameter == "Date" & str_detect(value, "^\\d+$") ~ {
      # Suppress expected warnings about date conversion
      suppressWarnings({
        tryCatch(
          as.character(as.Date(as.numeric(value), origin = "1899-12-30")),
          error = function(e) value
        )
      })
    },
    parameter == "Time" & str_detect(value, "^0\\.") ~ {
      # Suppress expected warnings about time conversion
      suppressWarnings({
        tryCatch({
          time_decimal <- as.numeric(value)
          hours <- floor(time_decimal * 24)
          minutes <- floor((time_decimal * 24 - hours) * 60)
          seconds <- round(((time_decimal * 24 - hours) * 60 - minutes) * 60)
          sprintf("%02d:%02d:%02d", hours, minutes, seconds)
        }, error = function(e) value)
      })
    },
    TRUE ~ value
  )
}

#' Find start and end rows of kinetic data
find_data_boundaries <- function(all_data) {
  # Find start of numeric data
  data_start_row <- NA
  for (i in 35:50) {
    if (i <= nrow(all_data)) {
      first_cell <- all_data[[i, 1]]
      if (!is.na(first_cell)) {
        numeric_val <- suppressWarnings(as.numeric(first_cell))
        if (!is.na(numeric_val) && numeric_val > 0 && numeric_val < 50) {
          data_start_row <- i
          break
        }
      }
    }
  }
  
  if (is.na(data_start_row)) {
    data_start_row <- 40
    warning("Could not find numeric data start, using default row 40")
  }
  
  # Find end of data
  data_end_row <- nrow(all_data)
  for (i in (data_start_row + 50):nrow(all_data)) {
    key_columns <- all_data[i, 1:3] %>% unlist()
    non_empty_count <- sum(!is.na(key_columns) & key_columns != "" & key_columns != "NA")
    
    if (non_empty_count == 0) {
      data_end_row <- i - 1
      break
    }
  }
  
  return(list(start = data_start_row, end = data_end_row))
}

#' Function to examine the raw data structure before processing
examine_raw_kinetic_data <- function(file_path) {
  
  cat("• EXAMINING RAW KINETIC DATA STRUCTURE\n")
  
  # Read the file around row 40 where data should start
  preview_data <- suppressMessages(suppressWarnings(
    read_excel(file_path, col_names = FALSE, skip = 38, n_max = 5)
  ))
  
  cat("• Raw data around row 40:\n")
  for (i in 1:min(5, nrow(preview_data))) {
    row_vals <- preview_data[i, 1:min(5, ncol(preview_data))] %>% 
      unlist() %>% 
      as.character()
    cat("  Row", i + 38, ":", paste(row_vals, collapse = " | "), "\n")
  }
  
  return(preview_data)
}

#' Read kinetic data section from Excel file
read_kinetic_data <- function(file_path, sheet_name, boundaries) {
  suppressMessages(suppressWarnings(
    read_excel(
      file_path, 
      sheet = sheet_name,
      skip = boundaries$start - 1,
      n_max = boundaries$end - boundaries$start + 1,
      col_names = FALSE,
      col_types = "text"
    )
  ))
}

#' Process raw kinetic data into long format
process_kinetic_data <- function(kinetic_data, metadata) {
  
  # Set column names
  n_cols <- ncol(kinetic_data)
  col_names <- c("time_hrs", "time_formatted", "temperature", 
                 paste0(rep(LETTERS[1:8], each = 12), 
                        sprintf("%02d", rep(1:12, 8))))
  col_names <- col_names[1:n_cols]
  names(kinetic_data) <- col_names
  
  # Create experiment start datetime
  experiment_start <- create_experiment_datetime(metadata)
  
  # Process to long format - suppress expected warnings
  suppressWarnings({
    kinetic_long <- kinetic_data %>%
      mutate(
        time_hrs_clean = as.numeric(time_hrs),           # Expected NAs here
        temperature_clean = as.numeric(temperature)      # Expected NAs here
      ) %>%
      filter(!is.na(time_hrs_clean)) %>%
      mutate(datetime = experiment_start + seconds(time_hrs_clean * 3600)) %>%
      select(time_hrs_clean, temperature_clean, datetime, A01:H12) %>%
      pivot_longer(
        cols = A01:H12,
        names_to = "well_id",
        values_to = "od600_raw"
      ) %>%
      mutate(od600 = as.numeric(od600_raw)) %>%        # Expected NAs here too
      filter(!is.na(od600)) %>%
      rename(
        time_hrs = time_hrs_clean,
        temperature = temperature_clean
      ) %>%
      arrange(datetime, well_id) %>%
      group_by(well_id) %>%
      mutate(
        time_point = row_number(),
        time_elapsed = time_hrs - min(time_hrs, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      select(time_hrs, datetime, well_id, od600, time_point, temperature, time_elapsed)
  })
  
  return(kinetic_long)
}

#' Find all wavelength sections in the data
#' @param all_data The complete Excel data
#' @return List of wavelength information with boundaries
find_wavelength_sections <- function(all_data) {
  
  wavelength_sections <- list()
  
  # Look for wavelength markers
  for (i in 1:nrow(all_data)) {
    first_cell <- all_data[[i, 1]]
    if (!is.na(first_cell)) {
      # Check if it's a wavelength marker
      if (str_detect(as.character(first_cell), "^(420|600|\\d{3})$")) {
        wavelength <- as.character(first_cell)
        
        cat("• Found wavelength section:", wavelength, "nm at row", i, "\n")
        
        # Look for the header row (should contain "Time", "T°", A1, A2, etc.)
        header_row <- NA
        data_start_row <- NA
        
        for (j in (i + 1):(i + 5)) {  # Check next few rows
          if (j <= nrow(all_data)) {
            # Check if this looks like a header row
            row_content <- all_data[j, ] %>% unlist() %>% na.omit() %>% as.character()
            
            # Look for Time, temperature, and well identifiers
            has_time <- any(str_detect(row_content, "Time"))
            has_temp <- any(str_detect(row_content, "T°|temp"))
            has_wells <- any(str_detect(row_content, "^[A-H][0-9]+$"))  # A1, B1, etc.
            
            if (has_time && has_temp && has_wells) {
              header_row <- j
              data_start_row <- j + 1
              cat("  - Found header at row", header_row, "\n")
              break
            }
          }
        }
        
        if (is.na(data_start_row)) {
          cat("• Warning: Could not find header/data start for wavelength", wavelength, "\n")
          # Try to find data start without header
          for (j in (i + 1):(i + 10)) {
            if (j <= nrow(all_data)) {
              first_data_cell <- all_data[[j, 1]]
              if (!is.na(first_data_cell)) {
                # Check if it looks like time data (number between 0 and 50)
                numeric_val <- suppressWarnings(as.numeric(first_data_cell))
                if (!is.na(numeric_val) && numeric_val >= 0 && numeric_val < 50) {
                  data_start_row <- j
                  cat("  - Found data start (no header) at row", data_start_row, "\n")
                  break
                }
              }
            }
          }
        }
        
        if (is.na(data_start_row)) {
          cat("• Skipping wavelength", wavelength, "- could not find data\n")
          next
        }
        
        # Find end of data for this wavelength
        data_end_row <- nrow(all_data)
        for (k in (data_start_row + 10):nrow(all_data)) {
          if (k <= nrow(all_data)) {
            # Check if we hit another wavelength marker
            next_cell <- all_data[[k, 1]]
            if (!is.na(next_cell) && str_detect(as.character(next_cell), "^(420|600|\\d{3})$")) {
              data_end_row <- k - 1
              break
            }
            
            # Check if we hit empty rows (multiple empty cells in key columns)
            key_columns <- all_data[k, 1:5] %>% unlist()
            non_empty_count <- sum(!is.na(key_columns) & key_columns != "" & key_columns != "NA")
            
            if (non_empty_count == 0) {
              data_end_row <- k - 1
              break
            }
          }
        }
        
        wavelength_sections[[paste0("wavelength_", wavelength)]] <- list(
          wavelength = wavelength,
          marker_row = i,
          header_row = header_row,
          data_start = data_start_row,
          data_end = data_end_row
        )
        
        cat("  - Data rows:", data_start_row, "to", data_end_row, "\n")
      }
    }
  }
  
  if (length(wavelength_sections) == 0) {
    # Fallback: assume 600nm data starts around row 40
    cat("• No wavelength markers found, assuming 600nm data from row 40\n")
    data_boundaries <- find_data_boundaries(all_data)
    wavelength_sections[["wavelength_600"]] <- list(
      wavelength = "600",
      marker_row = NA,
      header_row = NA,
      data_start = data_boundaries$start,
      data_end = data_boundaries$end
    )
  }
  
  return(wavelength_sections)
}

#' Read kinetic data for a specific wavelength
read_wavelength_kinetic_data <- function(file_path, sheet_name, wavelength_info) {
  
  cat("• Reading", wavelength_info$wavelength, "nm data (rows", 
      wavelength_info$data_start, "to", wavelength_info$data_end, ")\n")
  
  suppressMessages(suppressWarnings(
    read_excel(
      file_path, 
      sheet = sheet_name,
      skip = wavelength_info$data_start - 1,
      n_max = wavelength_info$data_end - wavelength_info$data_start + 1,
      col_names = FALSE,
      col_types = "text"
    )
  ))
}

process_wavelength_kinetic_data <- function(kinetic_data, metadata, wavelength) {
  
  n_cols <- ncol(kinetic_data)
  cat("  - Processing", n_cols, "columns for", wavelength, "nm\n")
  
  # Check if first row is header
  first_row <- kinetic_data[1, ] %>% unlist() %>% na.omit() %>% as.character()
  has_header <- any(str_detect(first_row, "Time|T°"))
  
  if (has_header) {
    # Remove header row
    kinetic_data <- kinetic_data[-1, ]
    cat("  - Removed header row\n")
  }
  
  # Expected format: Time, T° wavelength, A1-H12 (98 columns total)
  if (n_cols >= 98) {
    # Full 96-well format with A1-H12 naming
    col_names <- c("time_hrs", "temperature", 
                   paste0(rep(LETTERS[1:8], each = 12), 1:12))
    col_names <- col_names[1:n_cols]
    
    # Well columns (A1-H12)
    well_cols <- col_names[3:98]  # Skip time and temperature
    
  } else if (n_cols >= 14) {
    # Partial data or different format
    col_names <- c("time_hrs", "temperature", 
                   paste0("Col_", 3:n_cols))
    well_cols <- col_names[3:length(col_names)]
    
  } else {
    stop("Unexpected number of columns: ", n_cols, ". Expected ~98 for full plate.")
  }
  
  names(kinetic_data) <- col_names
  
  cat("  - Well columns identified:", length(well_cols), "\n")
  cat("  - Sample wells:", paste(head(well_cols, 5), collapse = ", "), "...\n")
  
  # Create experiment start datetime
  experiment_start <- create_experiment_datetime(metadata)
  
  # Process to long format
  suppressWarnings({
    # Clean time and temperature - ENSURE NUMERIC CONVERSION
    kinetic_clean <- kinetic_data %>%
      mutate(
        time_hrs_clean = as.numeric(time_hrs),
        temperature_clean = as.numeric(temperature)
      ) %>%
      filter(!is.na(time_hrs_clean))
    
    # Check sample data before processing
    sample_well_data <- kinetic_clean[[well_cols[1]]][1:3]
    cat("  - Sample well data:", paste(sample_well_data, collapse = ", "), "\n")
    
    # Convert to long format
    kinetic_long <- kinetic_clean %>%
      mutate(datetime = experiment_start + seconds(time_hrs_clean * 3600)) %>%
      select(time_hrs_clean, temperature_clean, datetime, all_of(well_cols)) %>%
      pivot_longer(
        cols = all_of(well_cols),
        names_to = "well_id_raw",
        values_to = paste0("od", wavelength, "_raw")
      ) %>%
      mutate(
        # ENSURE NUMERIC CONVERSION AND CLEAN DATA
        !!paste0("od", wavelength, "_raw") := as.numeric(.data[[paste0("od", wavelength, "_raw")]]),
        !!paste0("od", wavelength) := .data[[paste0("od", wavelength, "_raw")]],
        # Convert A1-H12 format to A01-H12 format for consistency
        well_id = case_when(
          str_detect(well_id_raw, "^[A-H]\\d{1}$") ~ str_replace(well_id_raw, "^([A-H])(\\d)$", "\\10\\2"),
          str_detect(well_id_raw, "^[A-H]\\d{2}$") ~ well_id_raw,
          TRUE ~ well_id_raw
        )
      ) %>%
      # FILTER OUT NON-NUMERIC DATA
      filter(!is.na(.data[[paste0("od", wavelength, "_raw")]]),
             is.finite(.data[[paste0("od", wavelength, "_raw")]])) %>%
      rename(
        time_hrs = time_hrs_clean,
        temperature = temperature_clean
      ) %>%
      select(-well_id_raw) %>%
      arrange(datetime, well_id) %>%
      group_by(well_id) %>%
      mutate(
        time_point = row_number(),
        time_elapsed = time_hrs - min(time_hrs, na.rm = TRUE),
        wavelength = wavelength
      ) %>%
      ungroup() %>%
      select(time_hrs, datetime, well_id, starts_with(paste0("od", wavelength)), 
             time_point, temperature, time_elapsed, wavelength)
  })
  
  cat("  - Processed", nrow(kinetic_long), "data points\n")
  cat("  - Wells found:", length(unique(kinetic_long$well_id)), "\n")
  cat("  - Sample wells:", paste(head(sort(unique(kinetic_long$well_id)), 5), collapse = ", "), "...\n")
  
  # Check final data types
  od_col <- paste0("od", wavelength, "_raw")
  sample_final_data <- kinetic_long[[od_col]][1:5]
  cat("  - Final sample OD values:", paste(sample_final_data, collapse = ", "), "\n")
  cat("  - Final OD data type:", class(kinetic_long[[od_col]]), "\n")
  
  return(kinetic_long)
}
#' Convert HH:MM:SS time format to decimal hours
convert_time_to_hours <- function(time_strings) {
  
  # Convert time strings like "0:03:38" to decimal hours
  sapply(time_strings, function(time_str) {
    if (is.na(time_str) || time_str == "") return(NA)
    
    # Split by colon
    parts <- str_split(time_str, ":")[[1]]
    
    if (length(parts) == 3) {
      hours <- as.numeric(parts[1])
      minutes <- as.numeric(parts[2])
      seconds <- as.numeric(parts[3])
      
      # Convert to decimal hours
      decimal_hours <- hours + minutes/60 + seconds/3600
      return(decimal_hours)
    } else {
      return(NA)
    }
  }, USE.NAMES = FALSE)
}

#' Enhanced read Synergy HTX file with multiple wavelengths
read_synergy_htx_file_multi <- function(file_path, sheet_name = NULL) {
  
  cat("• Reading Synergy HTX file:", basename(file_path), "\n")
  
  # Read entire sheet
  all_data <- suppressMessages(suppressWarnings(
    if (is.null(sheet_name)) {
      read_excel(file_path, col_names = FALSE)
    } else {
      read_excel(file_path, sheet = sheet_name, col_names = FALSE)
    }
  ))
  
  # Extract metadata (rows 1-25)
  metadata <- extract_metadata(all_data[1:25, 1:10])
  
  # Find all wavelength sections
  wavelength_sections <- find_wavelength_sections(all_data)
  
  # Read and process each wavelength
  wavelength_data <- list()
  experiment_start <- create_experiment_datetime(metadata)
  
  for (section_name in names(wavelength_sections)) {
    wavelength_info <- wavelength_sections[[section_name]]
    wavelength <- wavelength_info$wavelength
    
    # Read kinetic data for this wavelength
    kinetic_data <- read_wavelength_kinetic_data(file_path, sheet_name, wavelength_info)
    
    # Process and clean kinetic data
    kinetic_long <- process_wavelength_kinetic_data(kinetic_data, metadata, wavelength)
    
    wavelength_data[[section_name]] <- kinetic_long
    
    cat("  ✓ Processed", nrow(kinetic_long), "data points for", wavelength, "nm\n")
  }
  
  cat("• Successfully read", length(wavelength_data), "wavelength(s):", 
      paste(sapply(wavelength_sections, function(x) paste0(x$wavelength, "nm")), collapse = ", "), "\n")
  
  return(list(
    metadata = metadata,
    wavelength_data = wavelength_data,
    wavelength_sections = wavelength_sections,
    experiment_start = experiment_start
  ))
}

#' Updated diagnostic function
examine_file_structure <- function(file_path, sheet_name = NULL) {
  
  cat("• EXAMINING FILE STRUCTURE\n")
  cat("• File:", basename(file_path), "\n\n")
  
  # Read first 50 rows to examine structure
  preview_data <- suppressMessages(suppressWarnings(
    if (is.null(sheet_name)) {
      read_excel(file_path, col_names = FALSE, n_max = 50)
    } else {
      read_excel(file_path, sheet = sheet_name, col_names = FALSE, n_max = 50)
    }
  ))
  
  cat("• Total columns in sheet:", ncol(preview_data), "\n")
  
  # Look for wavelength markers
  for (i in 1:nrow(preview_data)) {
    first_cell <- preview_data[[i, 1]]
    if (!is.na(first_cell)) {
      if (str_detect(as.character(first_cell), "^(420|600|\\d{3})$")) {
        cat("• Wavelength marker found:", first_cell, "at row", i, "\n")
        
        # Show header structure
        for (j in (i+1):min(i+3, nrow(preview_data))) {
          row_content <- preview_data[j, 1:min(10, ncol(preview_data))] %>% 
            unlist() %>% 
            as.character()
          row_content[is.na(row_content)] <- "NA"
          cat("  Row", j, ":", paste(row_content, collapse = " | "), "\n")
        }
        cat("\n")
      }
    }
  }
  
  return(invisible(preview_data))
}

# ===== EXPERIMENTAL LAYOUT FUNCTIONS =====

#' Create experimental layout with explicit replicate structure
create_experiment_layout <- function(layout_type = "default") {
  
  if (layout_type != "default") {
    stop("Only 'default' layout currently supported")
  }
  
  layout <- expand_grid(
    row = LETTERS[1:8],
    col = 1:12
  ) %>%
    mutate(
      well_id = paste0(row, sprintf("%02d", col)),
      condition = case_when(
        col %in% c(1, 12) ~ "broth_control",                    # Columns 1 & 12 = broth
        row %in% c("A", "B", "C") & col %in% 2:11 ~ "SAMPLE",   # Rows A,B,C cols 2-11 = samples
        row == "D" & col %in% 2:11 ~ "NP_only",                 # Row D cols 2-11 = NP controls
        row %in% c("E", "F", "G") & col %in% 2:11 ~ "blank",    # Rows E,F,G cols 2-11 = blanks
        row %in% c("E", "F", "G") & col %in% c(1, 12) ~ "broth_control", # E,F,G edge = broth
        row == "H" & col %in% 2:11 ~ "UNT",                     # Row H cols 2-11 = untreated
        row == "H" & col %in% c(1, 12) ~ "broth_control",       # Row H edge = broth
        TRUE ~ "unknown"
      ),
      concentration = case_when(
        condition == "SAMPLE" ~ paste0("Conc_", col - 1),
        condition == "NP_only" ~ paste0("Conc_", col - 1),
        condition == "UNT" ~ paste0("Conc_", col - 1),
        condition == "blank" ~ paste0("Blank_", col - 1),        # Blanks also have "concentrations"
        TRUE ~ NA_character_
      ),
      sample_type = case_when(
        condition == "SAMPLE" ~ "sample",
        condition == "NP_only" ~ "np_control",
        condition == "UNT" ~ "untreated_control",
        condition == "broth_control" ~ "broth",                  # Broth controls
        condition == "blank" ~ "blank",                          # Blank controls
        TRUE ~ "unknown"
      ),
      replicate_id = case_when(
        row == "A" & col %in% 2:11 ~ "Rep_A",
        row == "B" & col %in% 2:11 ~ "Rep_B", 
        row == "C" & col %in% 2:11 ~ "Rep_C",
        row == "H" & col %in% 2:11 ~ "Rep_UNT",
        row == "D" & col %in% 2:11 ~ "NP_Control",
        row == "E" & col %in% 2:11 ~ "Blank_E",                  # Blank replicates
        row == "F" & col %in% 2:11 ~ "Blank_F",
        row == "G" & col %in% 2:11 ~ "Blank_G",
        TRUE ~ NA_character_
      )
    )
  
  # Create summary
  layout_summary <- layout %>%
    filter(!is.na(concentration)) %>%
    group_by(concentration, sample_type) %>%
    summarise(
      wells = paste(well_id, collapse = ", "),
      n_replicates = n(),
      .groups = "drop"
    )
  
  print_layout_summary()
  
  return(list(
    layout = layout,
    summary = layout_summary
  ))
}

create_custom_layout <- function(sample_wells, np_control_wells, blank_wells, 
                                 untreated_wells = NULL, concentrations = 1:10) {
  
  # Validate inputs
  all_wells <- c(sample_wells, np_control_wells, blank_wells, untreated_wells)
  if (length(unique(all_wells)) != length(all_wells)) {
    stop("Duplicate wells detected in layout specification")
  }
  
  # Create base layout
  layout <- expand_grid(
    row = LETTERS[1:8],
    col = 1:12
  ) %>%
    mutate(well_id = paste0(row, sprintf("%02d", col)))
  
  # User would specify something like:
  # sample_wells = list(
  #   "Conc_1" = c("A2", "B2", "C2"),
  #   "Conc_2" = c("A3", "B3", "C3"),
  #   # ... etc
  # )
  # np_control_wells = list(
  #   "Conc_1" = "D2",
  #   "Conc_2" = "D3",
  #   # ... etc
  # )
  
  # Implementation would go here...
  # This is a placeholder for future development
  
  stop("Custom layout function not yet implemented. Use create_experiment_layout('default') for now.")
}

#' Print layout summary to console
print_layout_summary <- function() {
  cat("=== EXPERIMENTAL LAYOUT ===\n")
  cat("Samples (A, B, C rows): 3 biological replicates per concentration\n")
  cat("- Columns 2-11 = Concentrations 1-10\n")
  cat("- A2,B2,C2 = Conc 1 | A3,B3,C3 = Conc 2 | etc.\n")
  cat("NP Controls (D row): D2-D11 = NP-only for each concentration\n")
  cat("Untreated (H row): H2-H11 = Untreated controls\n")
  cat("Blanks: Columns 1,12 + rows E,F,G\n\n")
}

# ===== DATA PROCESSING FUNCTIONS =====

#' Main data processing function
process_synergy_file <- function(file_path, sheet_name = NULL, layout_type = "default") {
  
  # Read raw data
  synergy_data <- read_synergy_htx_file(file_path, sheet_name)
  
  # Create layout
  layout_info <- create_experiment_layout(layout_type)
  
  # Merge data with layout
  complete_data <- synergy_data$kinetic_data %>%
    left_join(layout_info$layout, by = "well_id") %>%
    mutate(
      row_letter = str_extract(well_id, "^[A-H]"),
      col_number = as.numeric(str_extract(well_id, "\\d+$"))
    )
  
  # Create summary
  data_summary <- create_data_summary(synergy_data, complete_data, layout_info)
  
  return(list(
    metadata = synergy_data$metadata,
    data = complete_data,
    layout = layout_info$layout,
    summary = data_summary
  ))
}

#' Process multi-wavelength data with layout
process_synergy_file_multi <- function(file_path, sheet_name = NULL, layout_type = "default") {
  
  # Read raw data with multiple wavelengths
  synergy_data <- read_synergy_htx_file_multi(file_path, sheet_name)
  
  # Create layout (same for all wavelengths)
  layout_info <- create_experiment_layout(layout_type)
  
  # Process each wavelength
  processed_wavelengths <- list()
  
  for (wavelength_name in names(synergy_data$wavelength_data)) {
    
    wavelength_kinetic <- synergy_data$wavelength_data[[wavelength_name]]
    wavelength <- unique(wavelength_kinetic$wavelength)[1]
    
    cat("• Processing layout for", wavelength, "nm...\n")
    
    # Merge data with layout
    complete_data <- wavelength_kinetic %>%
      left_join(layout_info$layout, by = "well_id") %>%
      mutate(
        row_letter = str_extract(well_id, "^[A-H]"),
        col_number = as.numeric(str_extract(well_id, "\\d+$"))
      )
    
    # Create summary for this wavelength
    data_summary <- create_wavelength_data_summary(synergy_data, complete_data, layout_info, wavelength)
    
    processed_wavelengths[[wavelength_name]] <- list(
      metadata = synergy_data$metadata,
      data = complete_data,
      layout = layout_info$layout,
      summary = data_summary,
      wavelength = wavelength
    )
  }
  
  return(list(
    wavelength_data = processed_wavelengths,
    metadata = synergy_data$metadata,
    layout = layout_info$layout,
    wavelength_sections = synergy_data$wavelength_sections
  ))
}

#' Create data summary statistics
create_data_summary <- function(synergy_data, complete_data, layout_info) {
  list(
    experiment_start = synergy_data$experiment_start,
    experiment_date = synergy_data$metadata["Date"],
    experiment_time = synergy_data$metadata["Time"],
    n_timepoints = length(unique(complete_data$time_point)),
    time_range_hrs = range(complete_data$time_hrs, na.rm = TRUE),
    datetime_range = range(complete_data$datetime, na.rm = TRUE),
    n_wells = length(unique(complete_data$well_id)),
    temp_range = range(complete_data$temperature, na.rm = TRUE),
    sample_distribution = complete_data %>% 
      distinct(well_id, sample_type) %>% 
      count(sample_type, name = "n_wells"),
    layout_summary = layout_info$summary
  )
}

# ===== DATA CORRECTION FUNCTIONS =====

#' Apply broth and nanoparticle corrections to data
process_with_corrections <- function(processed_data) {
  
  data <- processed_data$data
  
  # Apply broth correction
  broth_correction <- calculate_broth_correction(data)
  data_broth_corrected <- apply_broth_correction(data, broth_correction)
  
  # Apply nanoparticle correction
  np_corrections <- calculate_np_corrections(data_broth_corrected)
  data_fully_corrected <- apply_np_corrections(data_broth_corrected, np_corrections)
  
  # Calculate replicate statistics
  replicate_stats <- calculate_replicate_statistics(data_fully_corrected)
  
  cat("✅ Corrections applied successfully\n")
  cat("   📊 Broth correction:", round(broth_correction, 4), "OD units\n")
  cat("   🧪 NP corrections calculated for", 
      length(unique(np_corrections$concentration)), "concentrations\n")
  
  return(list(
    raw_data = data,
    corrected_data = data_fully_corrected,
    replicate_stats = replicate_stats,
    corrections = list(
      broth_correction = broth_correction,
      np_corrections = np_corrections
    )
  ))
}

#' Apply corrections for a specific wavelength
process_with_corrections_multi <- function(wavelength_processed, wavelength) {
  
  data <- wavelength_processed$data
  
  cat("  - Applying corrections for", wavelength, "nm data\n")
  cat("  - Available columns:", paste(names(data)[str_detect(names(data), paste0("od", wavelength))], collapse = ", "), "\n")
  
  # Determine which OD columns exist
  od_base_column <- paste0("od", wavelength)
  od_raw_column <- paste0("od", wavelength, "_raw")
  od_broth_corrected_column <- paste0("od", wavelength, "_broth_corrected")
  od_final_column <- paste0("od", wavelength, "_final")
  
  # Find the main OD data column and ensure it's numeric
  if (od_raw_column %in% names(data)) {
    main_od_column <- od_raw_column
    # Ensure it's numeric
    data <- data %>%
      mutate(!!main_od_column := as.numeric(.data[[main_od_column]]))
  } else if (od_base_column %in% names(data)) {
    main_od_column <- od_base_column
    # Ensure it's numeric and rename it to _raw for consistency
    data <- data %>%
      mutate(!!od_base_column := as.numeric(.data[[od_base_column]])) %>%
      rename(!!od_raw_column := !!od_base_column)
    main_od_column <- od_raw_column
  } else {
    stop("Could not find OD data column for wavelength ", wavelength)
  }
  
  cat("  - Using", main_od_column, "as source data\n")
  
  # Check data types and values
  sample_values <- data[[main_od_column]][1:5]
  cat("  - Sample values:", paste(sample_values, collapse = ", "), "\n")
  cat("  - Data type:", class(data[[main_od_column]]), "\n")
  
  # Filter out any non-numeric or NA values
  data <- data %>%
    filter(!is.na(.data[[main_od_column]]), 
           is.finite(.data[[main_od_column]]))
  
  cat("  - Data points after cleaning:", nrow(data), "\n")
  
  # Apply broth correction
  broth_correction <- data %>%
    filter(sample_type == "broth") %>%
    summarise(broth_od = mean(.data[[main_od_column]], na.rm = TRUE)) %>%
    pull(broth_od)
  
  if (is.na(broth_correction) || !is.finite(broth_correction)) {
    cat("  - Warning: Could not calculate broth correction, using 0\n")
    broth_correction <- 0
  }
  
  data_broth_corrected <- data %>%
    mutate(
      !!od_broth_corrected_column := pmax(.data[[main_od_column]] - broth_correction, 0)
    )
  
  # Apply nanoparticle correction
  np_corrections <- data_broth_corrected %>%
    filter(sample_type == "np_control") %>%
    select(time_hrs, time_point, concentration, !!od_broth_corrected_column) %>%
    group_by(time_hrs, time_point, concentration) %>%
    summarise(np_correction = mean(.data[[od_broth_corrected_column]], na.rm = TRUE), .groups = "drop")
  
  data_fully_corrected <- data_broth_corrected %>%
    left_join(np_corrections, by = c("time_hrs", "time_point", "concentration")) %>%
    mutate(
      np_correction = ifelse(is.na(np_correction), 0, np_correction),
      !!od_final_column := case_when(
        sample_type %in% c("sample", "untreated_control") ~ 
          pmax(.data[[od_broth_corrected_column]] - np_correction, 0),
        TRUE ~ .data[[od_broth_corrected_column]]
      )
    )
  
  # Calculate replicate statistics
  replicate_stats <- data_fully_corrected %>%
    filter(sample_type %in% c("sample", "untreated_control")) %>%
    group_by(sample_type, concentration, time_hrs, time_point) %>%
    summarise(
      n_replicates = n(),
      mean_od = mean(.data[[od_final_column]], na.rm = TRUE),
      sd_od = sd(.data[[od_final_column]], na.rm = TRUE),
      se_od = sd_od / sqrt(n_replicates),
      min_od = min(.data[[od_final_column]], na.rm = TRUE),
      max_od = max(.data[[od_final_column]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      se_od = ifelse(is.na(se_od), 0, se_od),
      wavelength = wavelength
    )
  
  cat("  ✓ Corrections applied for", wavelength, "nm\n")
  cat("    - Broth correction:", round(broth_correction, 4), "OD units\n")
  cat("    - NP corrections calculated for", 
      length(unique(np_corrections$concentration)), "concentrations\n")
  
  return(list(
    raw_data = data,
    corrected_data = data_fully_corrected,
    replicate_stats = replicate_stats,
    corrections = list(
      broth_correction = broth_correction,
      np_corrections = np_corrections
    ),
    wavelength = wavelength
  ))
}

#' Calculate broth correction value
calculate_broth_correction <- function(data) {
  data %>%
    filter(sample_type == "broth") %>%  # Only broth controls
    summarise(broth_od = mean(od600, na.rm = TRUE)) %>%
    pull(broth_od)
}

#' Calculate broth correction for specific wavelength
calculate_broth_correction_multi <- function(data, od_column) {
  # Check which OD column exists
  od_raw_column <- paste0(str_extract(od_column, "\\d+"), "_raw")
  
  if (paste0("od", str_extract(od_column, "\\d+"), "_raw") %in% names(data)) {
    # Use the raw column if it exists
    od_col_to_use <- paste0("od", str_extract(od_column, "\\d+"), "_raw")
  } else {
    # Use the main OD column
    od_col_to_use <- od_column
  }
  
  data %>%
    filter(sample_type == "broth") %>%
    summarise(broth_od = mean(.data[[od_col_to_use]], na.rm = TRUE)) %>%
    pull(broth_od)
}

#' Apply broth correction to all wells
apply_broth_correction <- function(data, broth_correction) {
  data %>%
    mutate(
      od600_raw = od600,
      od600_broth_corrected = pmax(od600 - broth_correction, 0)
    )
}

#' Apply broth correction for specific wavelength
apply_broth_correction_multi <- function(data, broth_correction, wavelength) {
  od_column <- paste0("od", wavelength)
  od_raw_column <- paste0("od", wavelength, "_raw")
  od_broth_corrected_column <- paste0("od", wavelength, "_broth_corrected")
  
  # Check if the raw column already exists
  if (od_raw_column %in% names(data)) {
    # Raw column already exists, just add the broth corrected column
    data %>%
      mutate(
        !!od_broth_corrected_column := pmax(.data[[od_raw_column]] - broth_correction, 0)
      )
  } else {
    # Need to rename the main OD column to raw first
    data %>%
      rename(!!od_raw_column := !!od_column) %>%
      mutate(
        !!od_broth_corrected_column := pmax(.data[[od_raw_column]] - broth_correction, 0)
      )
  }
}

#' Calculate nanoparticle corrections by concentration and time
calculate_np_corrections <- function(data_broth_corrected) {
  data_broth_corrected %>%
    filter(sample_type == "np_control") %>%
    select(time_hrs, time_point, concentration, od600_broth_corrected) %>%
    group_by(time_hrs, time_point, concentration) %>%
    summarise(np_correction = mean(od600_broth_corrected, na.rm = TRUE), .groups = "drop")
}

#' Calculate nanoparticle corrections for specific wavelength
calculate_np_corrections_multi <- function(data_broth_corrected, wavelength) {
  od_broth_corrected_column <- paste0("od", wavelength, "_broth_corrected")
  
  data_broth_corrected %>%
    filter(sample_type == "np_control") %>%
    select(time_hrs, time_point, concentration, !!od_broth_corrected_column) %>%
    group_by(time_hrs, time_point, concentration) %>%
    summarise(np_correction = mean(.data[[od_broth_corrected_column]], na.rm = TRUE), .groups = "drop")
}

#' Apply nanoparticle corrections to samples
apply_np_corrections <- function(data_broth_corrected, np_corrections) {
  data_broth_corrected %>%
    left_join(np_corrections, by = c("time_hrs", "time_point", "concentration")) %>%
    mutate(
      np_correction = ifelse(is.na(np_correction), 0, np_correction),
      od600_final = case_when(
        sample_type %in% c("sample", "untreated_control") ~ 
          pmax(od600_broth_corrected - np_correction, 0),
        TRUE ~ od600_broth_corrected
      )
    )
}

#' Apply nanoparticle corrections for specific wavelength
apply_np_corrections_multi <- function(data_broth_corrected, np_corrections, wavelength) {
  od_broth_corrected_column <- paste0("od", wavelength, "_broth_corrected")
  od_final_column <- paste0("od", wavelength, "_final")
  
  data_broth_corrected %>%
    left_join(np_corrections, by = c("time_hrs", "time_point", "concentration")) %>%
    mutate(
      np_correction = ifelse(is.na(np_correction), 0, np_correction),
      !!od_final_column := case_when(
        sample_type %in% c("sample", "untreated_control") ~ 
          pmax(.data[[od_broth_corrected_column]] - np_correction, 0),
        TRUE ~ .data[[od_broth_corrected_column]]
      )
    )
}

#' Calculate replicate statistics
calculate_replicate_statistics <- function(data_fully_corrected) {
  data_fully_corrected %>%
    filter(sample_type %in% c("sample", "untreated_control")) %>%
    group_by(sample_type, concentration, time_hrs, time_point) %>%
    summarise(
      n_replicates = n(),
      mean_od = mean(od600_final, na.rm = TRUE),
      sd_od = sd(od600_final, na.rm = TRUE),
      se_od = sd_od / sqrt(n_replicates),
      min_od = min(od600_final, na.rm = TRUE),
      max_od = max(od600_final, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(se_od = ifelse(is.na(se_od), 0, se_od))
}

#' Calculate replicate statistics for specific wavelength
calculate_replicate_statistics_multi <- function(data_fully_corrected, wavelength) {
  od_final_column <- paste0("od", wavelength, "_final")
  
  data_fully_corrected %>%
    filter(sample_type %in% c("sample", "untreated_control")) %>%
    group_by(sample_type, concentration, time_hrs, time_point) %>%
    summarise(
      n_replicates = n(),
      mean_od = mean(.data[[od_final_column]], na.rm = TRUE),
      sd_od = sd(.data[[od_final_column]], na.rm = TRUE),
      se_od = sd_od / sqrt(n_replicates),
      min_od = min(.data[[od_final_column]], na.rm = TRUE),
      max_od = max(.data[[od_final_column]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      se_od = ifelse(is.na(se_od), 0, se_od),
      wavelength = wavelength
    )
}


# ===== SUMMARY FUNCTIONS =====

#' Create comprehensive summary of replicate results
summarize_replicate_results <- function(corrected_results) {
  
  cat("📊 Calculating experiment metrics...\n")
  
  # Suppress dplyr grouping messages throughout
  suppressMessages({
    # Overall experiment summary
    experiment_summary <- create_experiment_summary(corrected_results)
    
    # Growth metrics by condition
    growth_metrics <- calculate_growth_metrics_summary(corrected_results)
    
    # Time-based summaries
    time_summaries <- create_time_summaries(corrected_results)
    
    # Statistical summaries
    statistical_summaries <- create_statistical_summaries(corrected_results)
    
    # Quality metrics
    quality_metrics <- calculate_quality_metrics(corrected_results)
  })
  
  cat("✅ Summary calculations complete\n")
  
  return(list(
    experiment = experiment_summary,
    growth_metrics = growth_metrics,
    time_summaries = time_summaries,
    statistics = statistical_summaries,
    quality = quality_metrics
  ))
}

#' Overall experiment summary
create_experiment_summary <- function(corrected_results) {
  
  data <- corrected_results$corrected_data
  
  # Basic experiment info
  basic_info <- data %>%
    summarise(
      total_wells = n_distinct(well_id),
      total_timepoints = n_distinct(time_point),
      experiment_duration_hrs = max(time_hrs) - min(time_hrs),
      sampling_interval_hrs = round(mean(diff(sort(unique(time_hrs)))), 2),
      temperature_range = paste(round(min(temperature, na.rm = TRUE), 1), "-", 
                                round(max(temperature, na.rm = TRUE), 1), "°C")
    )
  
  # Sample distribution
  sample_distribution <- data %>%
    distinct(well_id, sample_type, concentration) %>%
    count(sample_type, name = "n_wells") %>%
    mutate(percentage = round(n_wells / sum(n_wells) * 100, 1))
  
  # Concentration distribution
  concentration_distribution <- data %>%
    filter(!is.na(concentration)) %>%
    distinct(well_id, concentration, sample_type) %>%
    count(concentration, sample_type) %>%
    pivot_wider(names_from = sample_type, values_from = n, values_fill = 0)
  
  return(list(
    basic_info = basic_info,
    sample_distribution = sample_distribution,
    concentration_distribution = concentration_distribution
  ))
}

#' Calculate growth metrics for each condition
calculate_growth_metrics_summary <- function(corrected_results) {
  
  # Per-well metrics
  well_metrics <- corrected_results$corrected_data %>%
    filter(sample_type %in% c("sample", "untreated_control")) %>%
    group_by(well_id, sample_type, concentration, replicate_id) %>%
    summarise(
      initial_od = first(od600_final),
      final_od = last(od600_final),
      max_od = max(od600_final, na.rm = TRUE),
      time_to_max_hrs = time_hrs[which.max(od600_final)],
      total_growth = final_od - initial_od,
      max_growth = max_od - initial_od,
      auc = sum(od600_final * mean(diff(time_hrs), na.rm = TRUE), na.rm = TRUE),
      .groups = "drop"
    )
  
  # Summary metrics by condition
  condition_metrics <- well_metrics %>%
    group_by(sample_type, concentration) %>%
    summarise(
      n_replicates = n(),
      
      # Initial OD stats
      mean_initial_od = mean(initial_od, na.rm = TRUE),
      sd_initial_od = sd(initial_od, na.rm = TRUE),
      
      # Final OD stats  
      mean_final_od = mean(final_od, na.rm = TRUE),
      sd_final_od = sd(final_od, na.rm = TRUE),
      se_final_od = sd_final_od / sqrt(n_replicates),
      
      # Max OD stats
      mean_max_od = mean(max_od, na.rm = TRUE),
      sd_max_od = sd(max_od, na.rm = TRUE),
      
      # Growth stats
      mean_total_growth = mean(total_growth, na.rm = TRUE),
      sd_total_growth = sd(total_growth, na.rm = TRUE),
      se_total_growth = sd_total_growth / sqrt(n_replicates),
      
      # AUC stats
      mean_auc = mean(auc, na.rm = TRUE),
      sd_auc = sd(auc, na.rm = TRUE),
      se_auc = sd_auc / sqrt(n_replicates),
      
      # Time to max
      mean_time_to_max = mean(time_to_max_hrs, na.rm = TRUE),
      sd_time_to_max = sd(time_to_max_hrs, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    mutate(
      cv_final_od = sd_final_od / mean_final_od * 100,
      cv_total_growth = sd_total_growth / abs(mean_total_growth) * 100
    )
  
  return(list(
    well_metrics = well_metrics,
    condition_metrics = condition_metrics
  ))
}

#' Create time-based summaries
create_time_summaries <- function(corrected_results) {
  
  # Growth at specific timepoints
  key_timepoints <- corrected_results$replicate_stats %>%
    filter(sample_type == "sample") %>%
    pull(time_hrs) %>%
    quantile(c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE) %>%
    round(1)
  
  timepoint_summary <- corrected_results$replicate_stats %>%
    filter(time_hrs %in% key_timepoints) %>%
    select(sample_type, concentration, time_hrs, mean_od, se_od) %>%
    pivot_wider(
      names_from = time_hrs,
      values_from = c(mean_od, se_od),
      names_sep = "_hrs_"
    )
  
  # Growth phases (early, mid, late)
  time_range <- range(corrected_results$replicate_stats$time_hrs)
  
  phase_summary <- corrected_results$replicate_stats %>%
    mutate(
      growth_phase = case_when(
        time_hrs <= time_range[1] + (time_range[2] - time_range[1]) * 0.33 ~ "early",
        time_hrs <= time_range[1] + (time_range[2] - time_range[1]) * 0.67 ~ "mid",
        TRUE ~ "late"
      )
    ) %>%
    group_by(sample_type, concentration, growth_phase) %>%
    summarise(
      mean_od_phase = mean(mean_od, na.rm = TRUE),
      max_od_phase = max(mean_od, na.rm = TRUE),
      n_timepoints = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = growth_phase,
      values_from = c(mean_od_phase, max_od_phase),
      names_sep = "_"
    )
  
  return(list(
    key_timepoints = timepoint_summary,
    growth_phases = phase_summary
  ))
}

#' Create statistical summaries and comparisons
create_statistical_summaries <- function(corrected_results) {
  
  # Variability metrics
  variability_summary <- corrected_results$replicate_stats %>%
    filter(mean_od > 0, se_od > 0) %>%
    group_by(sample_type, concentration) %>%
    summarise(
      mean_cv = mean(se_od / mean_od * 100, na.rm = TRUE),
      max_cv = max(se_od / mean_od * 100, na.rm = TRUE),
      mean_se_od = mean(se_od, na.rm = TRUE),
      max_se_od = max(se_od, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      mean_cv = pmin(mean_cv, 200),
      max_cv = pmin(max_cv, 200)
    )
  
  # Concentration comparison
  concentration_comparison <- corrected_results$replicate_stats %>%
    filter(sample_type %in% c("sample", "untreated_control")) %>%
    filter(time_point == max(time_point)) %>%
    select(sample_type, concentration, mean_od) %>%
    pivot_wider(names_from = sample_type, values_from = mean_od, values_fill = 0) %>%
    rename(sample_final_od = sample, untreated_final_od = untreated_control) %>%
    filter(!is.na(sample_final_od), !is.na(untreated_final_od)) %>%
    mutate(
      untreated_mean = mean(untreated_final_od, na.rm = TRUE),
      mean_inhibition_pct = case_when(
        untreated_mean > 0.001 ~ (1 - sample_final_od / untreated_mean) * 100,
        TRUE ~ NA_real_
      ),
      mean_fold_change = case_when(
        untreated_mean > 0.001 ~ sample_final_od / untreated_mean,
        TRUE ~ NA_real_
      ),
      effect_category = case_when(
        is.na(mean_inhibition_pct) ~ "No data",
        mean_inhibition_pct > 50 ~ "Strong inhibition",
        mean_inhibition_pct > 20 ~ "Moderate inhibition", 
        mean_inhibition_pct > -20 ~ "Minimal effect",
        TRUE ~ "Enhanced growth"
      )
    ) %>%
    arrange(desc(mean_inhibition_pct)) %>%
    select(concentration, sample_final_od, untreated_final_od,
           mean_inhibition_pct, mean_fold_change, effect_category)
  
  # Final endpoint comparison
  endpoint_comparison <- corrected_results$replicate_stats %>%
    filter(time_point == max(time_point)) %>%
    select(sample_type, concentration, mean_od, se_od, n_replicates) %>%
    mutate(
      cv_pct = case_when(
        mean_od > 0 ~ se_od / mean_od * sqrt(n_replicates) / mean_od * 100,
        TRUE ~ NA_real_
      )
    ) %>%
    arrange(sample_type, concentration)
  
  return(list(
    variability = variability_summary,
    concentration_effects = concentration_comparison,
    endpoint_comparison = endpoint_comparison
  ))
}

#' Calculate quality control metrics
calculate_quality_metrics <- function(corrected_results) {
  
  # Broth stability (columns 1 & 12)
  broth_stability <- corrected_results$corrected_data %>%
    filter(sample_type == "broth") %>%
    group_by(time_hrs) %>%
    summarise(mean_broth_od = mean(od600_raw, na.rm = TRUE), .groups = "drop") %>%
    summarise(
      broth_mean = mean(mean_broth_od, na.rm = TRUE),
      broth_drift = max(mean_broth_od) - min(mean_broth_od),
      broth_cv = sd(mean_broth_od) / mean(mean_broth_od) * 100,
      broth_range = paste(round(min(mean_broth_od), 4), "-", round(max(mean_broth_od), 4)),
      broth_stability_rating = case_when(
        broth_cv < 5 ~ "Excellent",
        broth_cv < 10 ~ "Good", 
        broth_cv < 20 ~ "Acceptable",
        TRUE ~ "Poor"
      )
    )
  
  # Blank stability (rows E, F, G)
  blank_stability <- corrected_results$corrected_data %>%
    filter(sample_type == "blank") %>%
    group_by(time_hrs) %>%
    summarise(mean_blank_od = mean(od600_raw, na.rm = TRUE), .groups = "drop") %>%
    summarise(
      blank_mean = mean(mean_blank_od, na.rm = TRUE),
      blank_drift = max(mean_blank_od) - min(mean_blank_od),
      blank_cv = sd(mean_blank_od) / mean(mean_blank_od) * 100,
      blank_range = paste(round(min(mean_blank_od), 4), "-", round(max(mean_blank_od), 4)),
      blank_stability_rating = case_when(
        blank_cv < 5 ~ "Excellent",
        blank_cv < 10 ~ "Good", 
        blank_cv < 20 ~ "Acceptable",
        TRUE ~ "Poor"
      )
    )
  
  # [Rest of the function stays the same for replicate_consistency and np_control_summary]
  
  # Replicate consistency
  replicate_consistency <- corrected_results$corrected_data %>%
    filter(sample_type == "sample", od600_final > 0) %>%
    group_by(concentration, time_hrs) %>%
    summarise(
      n_reps = n(),
      mean_od = mean(od600_final, na.rm = TRUE),
      sd_od = sd(od600_final, na.rm = TRUE),
      replicate_cv = case_when(
        mean_od > 0 & n_reps > 1 ~ sd_od / mean_od * 100,
        TRUE ~ NA_real_
      ),
      .groups = "drop"
    ) %>%
    filter(!is.na(replicate_cv), !is.infinite(replicate_cv)) %>%
    group_by(concentration) %>%
    summarise(
      mean_cv = mean(replicate_cv, na.rm = TRUE),
      max_cv = max(replicate_cv, na.rm = TRUE),
      median_cv = median(replicate_cv, na.rm = TRUE),
      consistency_rating = case_when(
        mean_cv < 10 ~ "Excellent",
        mean_cv < 20 ~ "Good",
        mean_cv < 35 ~ "Acceptable", 
        TRUE ~ "Poor"
      ),
      .groups = "drop"
    )
  
  # NP control effectiveness
  np_control_summary <- corrected_results$corrected_data %>%
    filter(sample_type == "np_control") %>%
    group_by(concentration) %>%
    summarise(
      mean_np_od = mean(od600_broth_corrected, na.rm = TRUE),
      max_np_od = max(od600_broth_corrected, na.rm = TRUE),
      np_drift = max(od600_broth_corrected) - min(od600_broth_corrected),
      np_correction_magnitude = case_when(
        mean_np_od < 0.05 ~ "Low",
        mean_np_od < 0.1 ~ "Moderate",
        TRUE ~ "High"
      ),
      .groups = "drop"
    )
  
  return(list(
    broth_stability = broth_stability,
    blank_stability = blank_stability,     # New!
    replicate_consistency = replicate_consistency,
    np_control_summary = np_control_summary
  ))
}

#' Create a formatted summary report
create_summary_report <- function(summaries) {
  
  cat("\n")
  cat("==========================================\n")
  cat("         REPLICATE RESULTS SUMMARY        \n")
  cat("==========================================\n\n")
  
  # Experiment overview
  cat("EXPERIMENT OVERVIEW:\n")
  cat("-------------------\n")
  exp_info <- summaries$experiment$basic_info
  cat("Total wells:", exp_info$total_wells, "\n")
  cat("Duration:", round(exp_info$experiment_duration_hrs, 1), "hours\n")
  cat("Sampling interval:", exp_info$sampling_interval_hrs, "hours\n")
  cat("Temperature:", exp_info$temperature_range, "\n\n")
  
  # Sample distribution
  cat("SAMPLE DISTRIBUTION:\n")
  cat("-------------------\n")
  print(summaries$experiment$sample_distribution)
  cat("\n")
  
  # Growth metrics highlights
  cat("GROWTH METRICS SUMMARY:\n")
  cat("----------------------\n")
  growth_highlights <- summaries$growth_metrics$condition_metrics %>%
    filter(sample_type == "sample") %>%
    arrange(desc(mean_final_od)) %>%
    select(concentration, mean_final_od, se_final_od, mean_total_growth, cv_final_od) %>%
    slice_head(n = 5)
  
  cat("Top 5 concentrations by final OD:\n")
  print(growth_highlights, digits = 3)
  cat("\n")
  
  # Quality metrics
  cat("QUALITY CONTROL:\n")
  cat("---------------\n")
  cat("Blank drift:", round(summaries$quality$blank_stability$blank_drift, 4), "OD units\n")
  cat("Blank CV:", round(summaries$quality$blank_stability$blank_cv, 2), "%\n")
  
  rep_quality <- summaries$quality$replicate_consistency %>%
    summarise(
      overall_mean_cv = mean(mean_cv, na.rm = TRUE),
      worst_cv = max(max_cv, na.rm = TRUE)
    )
  cat("Average replicate CV:", round(rep_quality$overall_mean_cv, 1), "%\n")
  cat("Worst replicate CV:", round(rep_quality$worst_cv, 1), "%\n\n")
  
  # Concentration effects
  cat("CONCENTRATION EFFECTS:\n")
  cat("---------------------\n")
  
  conc_effects_data <- summaries$statistics$concentration_effects
  
  if (is.null(conc_effects_data) || nrow(conc_effects_data) == 0) {
    cat("No concentration effect data available\n")
  } else {
    valid_effects <- conc_effects_data %>%
      filter(!is.na(mean_inhibition_pct), 
             !is.infinite(mean_inhibition_pct),
             !is.na(mean_fold_change),
             !is.infinite(mean_fold_change)) %>%
      arrange(desc(mean_inhibition_pct)) %>%
      select(concentration, mean_inhibition_pct, mean_fold_change, effect_category) %>%
      slice_head(n = 5)
    
    if (nrow(valid_effects) > 0) {
      cat("Top 5 inhibitory concentrations:\n")
      print(valid_effects, digits = 2)
      
      cat("\nEffect summary:\n")
      effect_summary <- conc_effects_data %>%
        filter(!is.na(effect_category)) %>%
        count(effect_category, name = "n_concentrations") %>%
        arrange(desc(n_concentrations))
      print(effect_summary)
    } else {
      cat("No valid concentration effects could be calculated\n")
      cat("This may indicate issues with untreated control values\n")
    }
  }
  
  cat("\n==========================================\n")
}

#' Create data summary for a specific wavelength
create_wavelength_data_summary <- function(synergy_data, complete_data, layout_info, wavelength) {
  list(
    wavelength = wavelength,
    experiment_start = synergy_data$experiment_start,
    experiment_date = synergy_data$metadata["Date"],
    experiment_time = synergy_data$metadata["Time"],
    n_timepoints = length(unique(complete_data$time_point)),
    time_range_hrs = range(complete_data$time_hrs, na.rm = TRUE),
    datetime_range = range(complete_data$datetime, na.rm = TRUE),
    n_wells = length(unique(complete_data$well_id)),
    temp_range = range(complete_data$temperature, na.rm = TRUE),
    sample_distribution = complete_data %>% 
      distinct(well_id, sample_type) %>% 
      count(sample_type, name = "n_wells"),
    layout_summary = layout_info$summary
  )
}

#' Quick diagnostic function to identify data issues
diagnose_data_issues <- function(corrected_results) {
  
  cat("=== DATA DIAGNOSTIC REPORT ===\n\n")
  
  issues <- list()
  
  # Check untreated control values
  untreated_summary <- corrected_results$replicate_stats %>%
    filter(sample_type == "untreated_control") %>%
    group_by(concentration) %>%
    summarise(
      mean_final_od = mean(mean_od[time_point == max(time_point)], na.rm = TRUE),
      .groups = "drop"
    )
  
  cat("📊 UNTREATED CONTROL SUMMARY:\n")
  print(untreated_summary)
  cat("\n")
  
  low_controls <- untreated_summary %>%
    filter(mean_final_od < 0.1)
  
  if (nrow(low_controls) > 0) {
    cat("⚠️  LOW UNTREATED CONTROL VALUES (< 0.1 OD):\n")
    print(low_controls)
    cat("This may affect inhibition calculations\n\n")
    issues$low_controls <- low_controls
  }
  
  # Check for negative values
  negative_ods <- corrected_results$corrected_data %>%
    filter(od600_final < 0) %>%
    count(sample_type, concentration) %>%
    arrange(desc(n))
  
  if (nrow(negative_ods) > 0) {
    cat("⚠️  NEGATIVE OD VALUES DETECTED:\n")
    print(negative_ods)
    cat("\n")
    issues$negative_ods <- negative_ods
  }
  
  # High variability
  high_cv <- corrected_results$replicate_stats %>%
    filter(sample_type == "sample") %>%
    mutate(cv = se_od / mean_od * 100) %>%
    filter(cv > 50, !is.na(cv), !is.infinite(cv)) %>%
    distinct(concentration) %>%
    arrange(concentration)
  
  if (nrow(high_cv) > 0) {
    cat("⚠️  HIGH VARIABILITY CONCENTRATIONS (CV > 50%):\n")
    print(high_cv)
    cat("\n")
    issues$high_variability <- high_cv
  }
  
  if (length(issues) == 0) {
    cat("✅ No major data issues detected!\n")
  }
  
  cat("==============================\n\n")
  
  return(issues)
}

#' Detailed layout verification function
verify_experimental_layout <- function(processed_data) {
  
  cat("=== EXPERIMENTAL LAYOUT VERIFICATION ===\n\n")
  
  # Generate the visual layout
  layout_validation <- validate_experiment_layout(processed_data)
  
  # Display the plot
  print(layout_validation$plot)
  
  layout_data <- processed_data$data %>%
    distinct(well_id, sample_type, concentration, condition, replicate_id) %>%
    arrange(well_id)
  
  cat("\n📋 DETAILED WELL ASSIGNMENTS:\n")
  
  # Group by sample type for easier verification
  for (stype in c("blank", "sample", "np_control", "untreated_control")) {
    
    wells_of_type <- layout_data %>%
      filter(sample_type == stype) %>%
      arrange(well_id)
    
    if (nrow(wells_of_type) > 0) {
      cat("\n", toupper(stype), "WELLS:\n")
      
      if (stype == "blank") {
        cat("Wells:", paste(wells_of_type$well_id, collapse = ", "), "\n")
      } else {
        # Show by concentration
        conc_summary <- wells_of_type %>%
          group_by(concentration) %>%
          summarise(
            wells = paste(well_id, collapse = ", "),
            n_replicates = n(),
            .groups = "drop"
          ) %>%
          arrange(concentration)
        
        for (i in 1:nrow(conc_summary)) {
          cat(conc_summary$concentration[i], ":", 
              conc_summary$wells[i], 
              "(", conc_summary$n_replicates[i], "replicates)\n")
        }
      }
    }
  }
  
  # Summary statistics
  cat("\n📊 LAYOUT SUMMARY:\n")
  summary_stats <- layout_data %>%
    count(sample_type, name = "n_wells") %>%
    mutate(percentage = round(n_wells / sum(n_wells) * 100, 1))
  
  print(summary_stats)
  
  # Check for expected replicates
  cat("\n🔍 REPLICATE CHECK:\n")
  replicate_check <- layout_data %>%
    filter(sample_type %in% c("sample", "untreated_control")) %>%
    count(sample_type, concentration, name = "n_replicates") %>%
    arrange(sample_type, concentration)
  
  print(replicate_check)
  
  # Flag any potential issues
  cat("\n⚠️  POTENTIAL LAYOUT ISSUES:\n")
  issues_found <- FALSE
  
  # Check for missing sample types
  expected_types <- c("blank", "sample", "np_control", "untreated_control")
  missing_types <- setdiff(expected_types, unique(layout_data$sample_type))
  if (length(missing_types) > 0) {
    cat("- Missing sample types:", paste(missing_types, collapse = ", "), "\n")
    issues_found <- TRUE
  }
  
  # Check for uneven replicates
  uneven_reps <- replicate_check %>%
    filter(n_replicates != 3)  # Expecting 3 replicates for samples
  
  if (nrow(uneven_reps) > 0) {
    cat("- Uneven replicates detected:\n")
    print(uneven_reps)
    issues_found <- TRUE
  }
  
  if (!issues_found) {
    cat("- No layout issues detected ✅\n")
  }
  
  cat("\n=== VERIFICATION COMPLETE ===\n")
  cat("Please confirm this matches your actual experimental setup!\n\n")
  
  return(list(
    layout_data = layout_data,
    validation_plot = layout_validation$plot,
    replicates_summary = layout_validation$replicates
  ))
}

#' Export summaries to Excel
export_summaries_to_excel <- function(summaries, 
                                      filename = "growth_curve_summaries.xlsx") {
  
  if (!require(writexl, quietly = TRUE)) {
    warning("writexl package not available. Install with: install.packages('writexl')")
    return(NULL)
  }
  
  # Prepare data for export
  export_list <- list(
    "Experiment_Overview" = summaries$experiment$basic_info,
    "Sample_Distribution" = summaries$experiment$sample_distribution,
    "Concentration_Layout" = summaries$experiment$concentration_distribution,
    "Well_Metrics" = summaries$growth_metrics$well_metrics,
    "Condition_Summary" = summaries$growth_metrics$condition_metrics,
    "Endpoint_Comparison" = summaries$statistics$endpoint_comparison,
    "Quality_Metrics" = bind_rows(
      summaries$quality$blank_stability %>% mutate(metric_type = "blank_stability"),
      summaries$quality$replicate_consistency %>% mutate(metric_type = "replicate_consistency")
    )
  )
  
  # Remove any list columns that might cause issues
  export_list <- map(export_list, ~ {
    if (is.data.frame(.x)) {
      select(.x, where(~ !is.list(.x)))
    } else {
      .x
    }
  })
  
  writexl::write_xlsx(export_list, filename)
  cat("Summaries exported to:", filename, "\n")
  
  return(filename)
}

#' Display summaries in a formatted way
display_summaries <- function(summaries) {
  
  cat("\n=== EXPERIMENT SUMMARIES ===\n\n")
  
  # 1. EXPERIMENTAL LAYOUT - VERIFY YOUR SETUP
  cat("🔍 EXPERIMENTAL LAYOUT VERIFICATION:\n")
  cat("Please verify this matches your actual plate setup!\n")
  cat(strrep("=", 50), "\n")  # Fixed this line
  
  # Show the layout in a plate-like format
  display_plate_layout(summaries)
  
  # Layout summary table
  cat("\n📋 LAYOUT SUMMARY TABLE:\n")
  if (!is.null(summaries$experiment$concentration_distribution)) {
    print(summaries$experiment$concentration_distribution)
  }
  cat("\n")
  
  # 2. Basic experiment info
  cat("📊 EXPERIMENT OVERVIEW:\n")
  print(summaries$experiment$basic_info)
  cat("\n")
  
  # 3. Sample distribution
  cat("🧪 SAMPLE DISTRIBUTION:\n")
  print(summaries$experiment$sample_distribution)
  cat("\n")
  
  # 4. Top growth performers
  cat("📈 TOP GROWTH PERFORMERS:\n")
  top_growth <- summaries$growth_metrics$condition_metrics %>%
    filter(sample_type == "sample") %>%
    arrange(desc(mean_final_od)) %>%
    select(concentration, mean_final_od, se_final_od, cv_final_od) %>%
    slice_head(n = 10)
  print(top_growth, digits = 3)
  cat("\n")
  
  # 5. Concentration effects (if available)
  cat("🎯 CONCENTRATION EFFECTS:\n")
  if (!is.null(summaries$statistics$concentration_effects) && 
      nrow(summaries$statistics$concentration_effects) > 0) {
    
    effects <- summaries$statistics$concentration_effects %>%
      filter(!is.na(mean_inhibition_pct), !is.infinite(mean_inhibition_pct)) %>%
      arrange(desc(mean_inhibition_pct)) %>%
      select(concentration, mean_inhibition_pct, mean_fold_change, effect_category)
    
    if (nrow(effects) > 0) {
      print(effects, digits = 2)
      
      # Effect summary
      cat("\nEffect Categories:\n")
      effect_counts <- effects %>%
        count(effect_category) %>%
        arrange(desc(n))
      print(effect_counts)
    } else {
      cat("No valid concentration effects calculated\n")
    }
  } else {
    cat("No concentration effects data available\n")
  }
  cat("\n")
  
  # 6. Quality metrics
  cat("✅ QUALITY CONTROL:\n")
  cat("Blank stability:", summaries$quality$blank_stability$blank_stability_rating, "\n")
  cat("Blank CV:", round(summaries$quality$blank_stability$blank_cv, 2), "%\n")
  cat("Blank drift:", round(summaries$quality$blank_stability$blank_drift, 4), "OD units\n")
  
  # Replicate consistency summary
  rep_summary <- summaries$quality$replicate_consistency %>%
    summarise(
      avg_cv = mean(mean_cv, na.rm = TRUE),
      worst_cv = max(max_cv, na.rm = TRUE),
      excellent_count = sum(consistency_rating == "Excellent", na.rm = TRUE),
      good_count = sum(consistency_rating == "Good", na.rm = TRUE),
      acceptable_count = sum(consistency_rating == "Acceptable", na.rm = TRUE),
      poor_count = sum(consistency_rating == "Poor", na.rm = TRUE)
    )
  
  cat("Average replicate CV:", round(rep_summary$avg_cv, 1), "%\n")
  cat("Replicate quality: Excellent(", rep_summary$excellent_count, 
      ") Good(", rep_summary$good_count, 
      ") Acceptable(", rep_summary$acceptable_count,
      ") Poor(", rep_summary$poor_count, ")\n")
  
  cat("\n=== END SUMMARIES ===\n")
}

#' Display specific summary tables
view_summary_table <- function(summaries, table_name) {
  
  available_tables <- c(
    "experiment_basic", "sample_distribution", "concentration_layout",
    "well_metrics", "condition_metrics", "concentration_effects", 
    "endpoint_comparison", "blank_stability", "replicate_consistency",
    "np_control_summary"
  )
  
  if (!table_name %in% available_tables) {
    cat("Available tables:\n")
    cat(paste(available_tables, collapse = "\n"))
    return(invisible())
  }
  
  result <- switch(table_name,
                   "experiment_basic" = summaries$experiment$basic_info,
                   "sample_distribution" = summaries$experiment$sample_distribution,
                   "concentration_layout" = summaries$experiment$concentration_distribution,
                   "well_metrics" = summaries$growth_metrics$well_metrics,
                   "condition_metrics" = summaries$growth_metrics$condition_metrics,
                   "concentration_effects" = summaries$statistics$concentration_effects,
                   "endpoint_comparison" = summaries$statistics$endpoint_comparison,
                   "blank_stability" = summaries$quality$blank_stability,
                   "replicate_consistency" = summaries$quality$replicate_consistency,
                   "np_control_summary" = summaries$quality$np_control_summary
  )
  
  cat("=== ", toupper(gsub("_", " ", table_name)), " ===\n")
  print(result)
  cat("\n")
  
  return(result)
}

#' Quick data explorer
explore_data <- function(corrected_results) {
  
  data <- corrected_results$corrected_data
  
  cat("=== DATA EXPLORATION ===\n\n")
  
  # Basic data info
  cat("📊 DATA DIMENSIONS:\n")
  cat("Total rows:", nrow(data), "\n")
  cat("Total wells:", length(unique(data$well_id)), "\n")
  cat("Timepoints:", length(unique(data$time_point)), "\n")
  cat("Time range:", round(min(data$time_hrs), 2), "to", round(max(data$time_hrs), 2), "hours\n")
  cat("\n")
  
  # Sample type breakdown
  cat("🧪 SAMPLE TYPES:\n")
  sample_summary <- data %>%
    distinct(well_id, sample_type, concentration) %>%
    count(sample_type, name = "n_wells")
  print(sample_summary)
  cat("\n")
  
  # OD value ranges by sample type
  cat("📈 OD RANGES BY SAMPLE TYPE:\n")
  od_ranges <- data %>%
    group_by(sample_type) %>%
    summarise(
      min_od = round(min(od600_final, na.rm = TRUE), 4),
      max_od = round(max(od600_final, na.rm = TRUE), 4),
      mean_od = round(mean(od600_final, na.rm = TRUE), 4),
      .groups = "drop"
    )
  print(od_ranges)
  cat("\n")
  
  # Check for potential issues
  cat("⚠️  POTENTIAL ISSUES:\n")
  
  # Negative values
  neg_count <- sum(data$od600_final < 0, na.rm = TRUE)
  if (neg_count > 0) {
    cat("- Negative OD values:", neg_count, "\n")
  }
  
  # Very high OD values (might indicate saturation)
  high_count <- sum(data$od600_final > 2, na.rm = TRUE)
  if (high_count > 0) {
    cat("- Very high OD values (>2):", high_count, "\n")
  }
  
  # Missing data
  missing_count <- sum(is.na(data$od600_final))
  if (missing_count > 0) {
    cat("- Missing OD values:", missing_count, "\n")
  }
  
  if (neg_count == 0 && high_count == 0 && missing_count == 0) {
    cat("- No obvious data issues detected ✅\n")
  }
  
  cat("\n=== END EXPLORATION ===\n")
}

# ===== VISUALIZATION FUNCTIONS =====

#' Create main growth curves plot with error bars
plot_growth_curves <- function(corrected_results) {
  corrected_results$replicate_stats %>%
    ggplot(aes(x = time_hrs, y = mean_od, color = concentration)) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = mean_od - se_od, ymax = mean_od + se_od, fill = concentration),
                alpha = 0.2, color = NA) +
    facet_wrap(~sample_type, scales = "free_y") +
    labs(
      title = "Growth Curves (Corrected)",
      subtitle = "Mean ± SE of biological replicates",
      x = "Time (hours)",
      y = "OD600 (corrected)",
      color = "Concentration",
      fill = "Concentration"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    ) +
    scale_color_viridis_d() +
    scale_fill_viridis_d()
}

#' Create growth curves plot for specific wavelength
plot_growth_curves_multi <- function(corrected_results, wavelength) {
  corrected_results$replicate_stats %>%
    ggplot(aes(x = time_hrs, y = mean_od, color = concentration)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = mean_od - se_od, ymax = mean_od + se_od, fill = concentration),
                alpha = 0.2, color = NA) +
    facet_wrap(~sample_type, scales = "free_y") +
    labs(
      title = paste("Growth Curves (Corrected) -", wavelength, "nm"),
      subtitle = "Mean ± SE of biological replicates",
      x = "Time (hours)",
      y = paste0("OD", wavelength, " (corrected)"),
      color = "Concentration",
      fill = "Concentration"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    ) +
    scale_color_viridis_d() +
    scale_fill_viridis_d()
}


#' Create single plate heatmap
plot_plate_heatmap <- function(corrected_results, timepoint = "last", 
                               data_type = "corrected") {
  
  # Determine timepoint
  if (timepoint == "first") {
    target_timepoint <- 1
    time_label <- "Initial"
  } else if (timepoint == "last") {
    target_timepoint <- max(corrected_results$corrected_data$time_point)
    time_label <- "Final"
  } else {
    target_timepoint <- timepoint
    time_label <- paste("Timepoint", timepoint)
  }
  
  # Select data column
  od_column <- case_when(
    data_type == "raw" ~ "od600_raw",
    data_type == "broth_corrected" ~ "od600_broth_corrected", 
    data_type == "corrected" ~ "od600_final",
    TRUE ~ "od600_final"
  )
  
  # Prepare data
  plot_data <- corrected_results$corrected_data %>%
    filter(time_point == target_timepoint) %>%
    mutate(
      row_num = as.numeric(factor(row_letter, levels = LETTERS[1:8])),
      col_num = col_number,
      od_value = .data[[od_column]],
      actual_time = round(first(time_hrs), 2)
    )
  
  # Create heatmap
  plot_data %>%
    ggplot(aes(x = col_num, y = 9 - row_num)) +
    geom_tile(aes(fill = od_value), color = "white", linewidth = 0.5) +
    geom_text(aes(label = round(od_value, 3)), size = 2.2, color = "white") +
    scale_x_continuous(breaks = 1:12, expand = c(0,0)) +
    scale_y_continuous(breaks = 1:8, labels = LETTERS[8:1], expand = c(0,0)) +
    scale_fill_viridis_c(name = "OD600", option = "magma") +
    labs(
      title = paste("Plate Layout -", time_label),
      subtitle = paste("Time:", plot_data$actual_time[1], "hours |", 
                       str_to_title(str_replace_all(data_type, "_", " ")), "Data"),
      x = "Column",
      y = "Row"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_blank()
    ) +
    coord_fixed()
}

#' Create plate heatmap for specific wavelength
plot_plate_heatmap_multi <- function(corrected_results, timepoint = "last", 
                                     data_type = "corrected", wavelength) {
  
  # Determine timepoint
  if (timepoint == "first") {
    target_timepoint <- 1
    time_label <- "Initial"
  } else if (timepoint == "last") {
    target_timepoint <- max(corrected_results$corrected_data$time_point)
    time_label <- "Final"
  } else {
    target_timepoint <- timepoint
    time_label <- paste("Timepoint", timepoint)
  }
  
  # Select data column based on wavelength and data type
  od_column <- case_when(
    data_type == "raw" ~ paste0("od", wavelength, "_raw"),
    data_type == "broth_corrected" ~ paste0("od", wavelength, "_broth_corrected"),
    data_type == "corrected" ~ paste0("od", wavelength, "_final"),
    TRUE ~ paste0("od", wavelength, "_final")
  )
  
  # Prepare data
  plot_data <- corrected_results$corrected_data %>%
    filter(time_point == target_timepoint) %>%
    mutate(
      row_num = as.numeric(factor(row_letter, levels = LETTERS[1:8])),
      col_num = col_number,
      od_value = .data[[od_column]],
      actual_time = round(first(time_hrs), 2)
    )
  
  # Create heatmap
  plot_data %>%
    ggplot(aes(x = col_num, y = 9 - row_num)) +
    geom_tile(aes(fill = od_value), color = "white", size = 0.5) +
    geom_text(aes(label = round(od_value, 3)), size = 2.2, color = "white") +
    scale_x_continuous(breaks = 1:12, expand = c(0,0)) +
    scale_y_continuous(breaks = 1:8, labels = LETTERS[8:1], expand = c(0,0)) +
    scale_fill_viridis_c(name = paste0("OD", wavelength), option = "magma") +
    labs(
      title = paste("Plate Layout -", time_label, "-", wavelength, "nm"),
      subtitle = paste("Time:", plot_data$actual_time[1], "hours |", 
                       str_to_title(str_replace_all(data_type, "_", " ")), "Data"),
      x = "Column",
      y = "Row"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_blank()
    ) +
    coord_fixed()
}


#' Create composite plate heatmap with raw vs corrected comparison
#' @param corrected_results Output from process_with_corrections()
#' @return List of individual plots and combined 2x2 layout
plot_plate_heatmap_composite <- function(corrected_results) {
  
  # Create all four heatmaps using the current plot_plate_heatmap function
  initial_raw <- plot_plate_heatmap(corrected_results, "first", "raw") +
    labs(title = "Initial Plate Layout - Raw Data")
  
  initial_corrected <- plot_plate_heatmap(corrected_results, "first", "corrected") +
    labs(title = "Initial Plate Layout - Corrected Data")
  
  final_raw <- plot_plate_heatmap(corrected_results, "last", "raw") +
    labs(title = "Final Plate Layout - Raw Data")
  
  final_corrected <- plot_plate_heatmap(corrected_results, "last", "corrected") +
    labs(title = "Final Plate Layout - Corrected Data")
  
  # Combine in 2x2 layout using patchwork
  combined_2x2 <- (initial_raw | final_raw) / 
    (initial_corrected | final_corrected)
  
  # Add overall title
  combined_2x2 <- combined_2x2 + 
    plot_annotation(
      title = "Plate Layout Comparison: Raw vs Corrected Data",
      subtitle = "Top row: Raw data | Bottom row: Corrected data | Left: Initial | Right: Final",
      theme = theme(plot.title = element_text(size = 16, face = "bold"),
                    plot.subtitle = element_text(size = 12))
    )
  
  return(list(
    initial_raw = initial_raw,
    initial_corrected = initial_corrected,
    final_raw = final_raw,
    final_corrected = final_corrected,
    combined_2x2 = combined_2x2
  ))
}

#' Create composite plate heatmap for specific wavelength
plot_plate_heatmap_composite_multi <- function(corrected_results, wavelength) {
  
  # Create all four heatmaps
  initial_raw <- plot_plate_heatmap_multi(corrected_results, "first", "raw", wavelength) +
    labs(title = paste("Initial -", wavelength, "nm - Raw Data"))
  
  initial_corrected <- plot_plate_heatmap_multi(corrected_results, "first", "corrected", wavelength) +
    labs(title = paste("Initial -", wavelength, "nm - Corrected Data"))
  
  final_raw <- plot_plate_heatmap_multi(corrected_results, "last", "raw", wavelength) +
    labs(title = paste("Final -", wavelength, "nm - Raw Data"))
  
  final_corrected <- plot_plate_heatmap_multi(corrected_results, "last", "corrected", wavelength) +
    labs(title = paste("Final -", wavelength, "nm - Corrected Data"))
  
  # Combine in 2x2 layout using patchwork
  combined_2x2 <- (initial_raw | final_raw) / 
    (initial_corrected | final_corrected)
  
  # Add overall title
  combined_2x2 <- combined_2x2 + 
    plot_annotation(
      title = paste("Plate Layout Comparison:", wavelength, "nm - Raw vs Corrected Data"),
      subtitle = "Top row: Raw data | Bottom row: Corrected data | Left: Initial | Right: Final",
      theme = theme(plot.title = element_text(size = 16, face = "bold"),
                    plot.subtitle = element_text(size = 12))
    )
  
  return(list(
    initial_raw = initial_raw,
    initial_corrected = initial_corrected,
    final_raw = final_raw,
    final_corrected = final_corrected,
    combined_2x2 = combined_2x2
  ))
}

#' Quick plot for a specific concentration
plot_concentration <- function(results, concentration, 
                               include_untreated = TRUE,
                               data_type = "corrected",
                               style = "jittered") {
  
  sample_types <- if (include_untreated) c("sample", "untreated_control") else "sample"
  
  plot_data <- get_concentration_data(
    results = results,
    concentration = concentration,
    sample_type = "both",
    data_type = data_type,
    format = "long"
  )
  
  if (is.null(plot_data)) return(NULL)
  
  # Create base plot
  if (style == "separated") {
    # Use facets to separate sample types
    p <- plot_data %>%
      ggplot(aes(x = time_hrs, y = od600, color = replicate_id)) +
      geom_line(linewidth = 1, alpha = 0.8) +
      geom_point(size = 2.5, alpha = 0.9) +
      facet_wrap(~sample_type, scales = "free_y", 
                 labeller = labeller(sample_type = c(
                   "sample" = "Sample",
                   "untreated_control" = "Untreated Control"
                 ))) +
      scale_color_viridis_d(name = "Replicate", option = "plasma") +
      labs(
        title = paste("Growth Curve -", concentration),
        subtitle = paste("Data type:", str_to_title(str_replace_all(data_type, "_", " "))),
        x = "Time (hours)",
        y = "OD600"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 11),
        strip.background = element_rect(fill = "gray90"),
        panel.grid.minor = element_blank()
      )
    
  } else if (style == "jittered") {
    # Use position jittering to separate overlapping points
    p <- plot_data %>%
      ggplot(aes(x = time_hrs, y = od600, 
                 color = interaction(sample_type, replicate_id),
                 shape = sample_type)) +
      geom_line(aes(group = interaction(sample_type, replicate_id)), 
                linewidth = 1, alpha = 0.7,
                position = position_jitter(width = 0.01, height = 0)) +
      geom_point(size = 3, alpha = 0.9,
                 position = position_jitter(width = 0.01, height = 0)) +
      scale_shape_manual(values = c("sample" = 16, "untreated_control" = 17),
                         name = "Sample Type") +
      scale_color_brewer(type = "qual", palette = "Set2", name = "Sample + Replicate") +
      labs(
        title = paste("Growth Curve -", concentration),
        subtitle = paste("Data type:", str_to_title(str_replace_all(data_type, "_", " "))),
        x = "Time (hours)",
        y = "OD600"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
  } else if (style == "with_ribbons") {
    # Show individual points plus mean with error ribbons
    summary_data <- plot_data %>%
      group_by(sample_type, time_hrs) %>%
      summarise(
        mean_od = mean(od600, na.rm = TRUE),
        se_od = sd(od600, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    p <- ggplot() +
      # Individual points
      geom_point(data = plot_data,
                 aes(x = time_hrs, y = od600, 
                     color = sample_type, shape = replicate_id),
                 size = 2, alpha = 0.6) +
      # Mean line with error ribbon
      geom_ribbon(data = summary_data,
                  aes(x = time_hrs, 
                      ymin = mean_od - se_od, 
                      ymax = mean_od + se_od,
                      fill = sample_type),
                  alpha = 0.2) +
      geom_line(data = summary_data,
                aes(x = time_hrs, y = mean_od, color = sample_type),
                linewidth = 1.5) +
      scale_color_manual(values = c("sample" = "#2E86AB", "untreated_control" = "#A23B72"),
                         name = "Sample Type") +
      scale_fill_manual(values = c("sample" = "#2E86AB", "untreated_control" = "#A23B72"),
                        name = "Sample Type") +
      scale_shape_manual(values = c("Rep_A" = 16, "Rep_B" = 17, "Rep_C" = 15, "Rep_UNT" = 18),
                         name = "Replicate") +
      labs(
        title = paste("Growth Curve -", concentration),
        subtitle = paste("Individual replicates + Mean ± SE |", 
                         str_to_title(str_replace_all(data_type, "_", " "))),
        x = "Time (hours)",
        y = "OD600"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
  } else {
    # Default style - improved colors and shapes
    p <- plot_data %>%
      ggplot(aes(x = time_hrs, y = od600, 
                 color = sample_type, 
                 shape = replicate_id,
                 linetype = sample_type)) +
      geom_line(aes(group = interaction(sample_type, replicate_id)), 
                linewidth = 1.2, alpha = 0.8) +
      geom_point(size = 3, alpha = 0.9, stroke = 1.2) +
      scale_color_manual(values = c("sample" = "#1f77b4", "untreated_control" = "#ff7f0e"),
                         name = "Sample Type") +
      scale_shape_manual(values = c("Rep_A" = 16, "Rep_B" = 17, "Rep_C" = 15, "Rep_UNT" = 18),
                         name = "Replicate") +
      scale_linetype_manual(values = c("sample" = "solid", "untreated_control" = "dashed"),
                            name = "Sample Type") +
      labs(
        title = paste("Growth Curve -", concentration),
        subtitle = paste("Data type:", str_to_title(str_replace_all(data_type, "_", " "))),
        x = "Time (hours)",
        y = "OD600"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
  }
  
  return(p)
}

#' Plot specific concentrations with proper ordering
plot_concentrations_panel <- function(corrected_results, concentrations = NULL, 
                                      sample_types = "sample", data_type = "corrected",
                                      free_scale = FALSE) {  # Add scale option
  
  # Select the appropriate OD column
  od_column <- case_when(
    data_type == "raw" ~ "od600_raw",
    data_type == "broth_corrected" ~ "od600_broth_corrected", 
    data_type == "corrected" ~ "od600_final",
    TRUE ~ "od600_final"
  )
  
  # Get available concentrations in numerical order
  available_concs <- corrected_results$corrected_data %>%
    filter(!is.na(concentration), sample_type %in% sample_types) %>%
    distinct(concentration) %>%
    pull(concentration)
  
  # Sort concentrations numerically
  conc_numbers <- as.numeric(str_extract(available_concs, "\\d+"))
  available_concs_sorted <- available_concs[order(conc_numbers)]
  
  # Use specified concentrations or all available
  if (is.null(concentrations)) {
    concentrations_to_plot <- available_concs_sorted
  } else {
    # Keep user order but warn about missing ones
    missing_concs <- setdiff(concentrations, available_concs)
    if (length(missing_concs) > 0) {
      cat("• Warning: Concentrations not found:", paste(missing_concs, collapse = ", "), "\n")
    }
    concentrations_to_plot <- intersect(concentrations, available_concs)
    
    # Re-order the user's selection numerically
    user_conc_numbers <- as.numeric(str_extract(concentrations_to_plot, "\\d+"))
    concentrations_to_plot <- concentrations_to_plot[order(user_conc_numbers)]
  }
  
  cat("• Plotting concentrations in order:", paste(concentrations_to_plot, collapse = ", "), "\n")
  
  # Get the data
  plot_data <- corrected_results$corrected_data %>%
    filter(sample_type %in% sample_types,
           concentration %in% concentrations_to_plot) %>%
    select(time_hrs, concentration, replicate_id, well_id, sample_type, !!sym(od_column)) %>%
    rename(od600 = !!sym(od_column))
  
  # Create factor with proper ordering for faceting
  plot_data <- plot_data %>%
    mutate(
      concentration_f = factor(concentration, levels = concentrations_to_plot)
    )
  
  # Set scale type
  scale_type <- if(free_scale) "free_y" else "fixed"
  scale_subtitle <- if(free_scale) "Free scale - optimized for individual trends" else "Fixed scale - for direct comparison"
  
  # Create the plot
  p <- plot_data %>%
    ggplot(aes(x = time_hrs, y = od600, color = sample_type)) +
    geom_line(aes(group = well_id), alpha = 0.6, linewidth = 0.5) +
    stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
    stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA,
                 aes(fill = sample_type)) +
    facet_wrap(~concentration_f, scales = scale_type) +
    labs(
      title = paste("Growth Curves by Concentration -", str_to_title(str_replace_all(data_type, "_", " "))),
      subtitle = paste("Sample types:", paste(sample_types, collapse = ", "), "|", scale_subtitle),
      x = "Time (hours)",
      y = "OD600",
      color = "Sample Type",
      fill = "Sample Type"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    ) +
    scale_color_viridis_d() +
    scale_fill_viridis_d()
  
  return(p)
}

plot_all_concentrations <- function(corrected_results, sample_types = "sample", 
                                    data_type = "corrected") {
  plot_concentrations_panel(corrected_results, NULL, sample_types, data_type)
}

plot_multiple_concentrations <- function(corrected_results, concentrations, 
                                         sample_types = "sample", 
                                         data_type = "corrected",
                                         separate_panels = TRUE,
                                         free_scale = FALSE) {  # Add scale option
  
  if (separate_panels) {
    return(plot_concentrations_panel(corrected_results, concentrations, sample_types, data_type, free_scale))
  }
  
  # Single panel version (unchanged)
  od_column <- case_when(
    data_type == "raw" ~ "od600_raw",
    data_type == "broth_corrected" ~ "od600_broth_corrected", 
    data_type == "corrected" ~ "od600_final",
    TRUE ~ "od600_final"
  )
  
  # Get the data and check concentrations exist
  available_concs <- corrected_results$corrected_data %>%
    filter(!is.na(concentration), sample_type %in% sample_types) %>%
    distinct(concentration) %>%
    pull(concentration)
  
  missing_concs <- setdiff(concentrations, available_concs)
  if (length(missing_concs) > 0) {
    cat("• Warning: Concentrations not found:", paste(missing_concs, collapse = ", "), "\n")
  }
  
  valid_concentrations <- intersect(concentrations, available_concs)
  
  plot_data <- corrected_results$corrected_data %>%
    filter(sample_type %in% sample_types,
           concentration %in% valid_concentrations) %>%
    select(time_hrs, concentration, replicate_id, well_id, sample_type, !!sym(od_column)) %>%
    rename(od600 = !!sym(od_column))
  
  # Order concentrations numerically for legend
  conc_numbers <- as.numeric(str_extract(valid_concentrations, "\\d+"))
  concentration_order <- valid_concentrations[order(conc_numbers)]
  
  plot_data <- plot_data %>%
    mutate(concentration_f = factor(concentration, levels = concentration_order))
  
  p <- plot_data %>%
    ggplot(aes(x = time_hrs, y = od600, color = concentration_f)) +
    stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
    stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA,
                 aes(fill = concentration_f)) +
    labs(
      title = paste("Selected Concentrations Comparison -", str_to_title(str_replace_all(data_type, "_", " "))),
      subtitle = paste("Concentrations:", paste(concentration_order, collapse = ", ")),
      x = "Time (hours)",
      y = "OD600",
      color = "Concentration",
      fill = "Concentration"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_viridis_d() +
    scale_fill_viridis_d()
  
  return(p)
}

#' Display plate layout in visual format
display_plate_layout <- function(summaries) {
  
  # Create a visual representation of the plate
  cat("    ", sprintf("%2d ", 1:12), "\n")
  cat("   ", strrep("-", 36), "\n")  # Fixed this line too
  
  rows <- LETTERS[1:8]
  
  for (i in 1:8) {
    row_letter <- rows[i]
    cat(row_letter, " |")
    
    for (j in 1:12) {
      well_id <- paste0(row_letter, sprintf("%02d", j))
      
      # Determine what's in this well based on our default layout
      content <- case_when(
        j %in% c(1, 12) ~ " B ",  # Broth
        i %in% 1:3 & j %in% 2:11 ~ " S ",  # Sample (A, B, C rows)
        i == 4 & j %in% 2:11 ~ " N ",  # NP control (D row)
        i %in% 5:7 ~ " B ",  # Broth (E, F, G rows)
        i == 8 & j %in% 2:11 ~ " U ",  # Untreated (H row)
        TRUE ~ " ? "
      )
      
      cat(content)
    }
    cat("|\n")
  }
  
  cat("   ", strrep("-", 36), "\n")  # Fixed this line too
  cat("\nLEGEND:\n")
  cat("B = Broth blank\n")
  cat("S = Sample (A=Rep1, B=Rep2, C=Rep3)\n") 
  cat("N = NP-only control\n")
  cat("U = Untreated control\n")
  cat("Columns 2-11 = Concentrations 1-10\n")
}

#' Create quality control plots
create_qc_plots <- function(corrected_results, processed_data = NULL) {
  
  qc_plots <- list(
    broth_stability = create_broth_stability_plot(corrected_results),
    blank_stability = create_blank_stability_plot(corrected_results),  # New!
    np_kinetics = create_np_kinetics_plot(corrected_results),
    before_after = create_before_after_plot(corrected_results)
  )
  
  # Add layout validation plot if processed_data is provided
  if (!is.null(processed_data)) {
    layout_validation <- validate_experiment_layout(processed_data)
    qc_plots$layout_validation = layout_validation$plot
  }
  
  return(qc_plots)
}

#' Create QC plots for specific wavelength
create_qc_plots_multi <- function(corrected_results, processed_data, wavelength) {
  
  qc_plots <- list(
    broth_stability = create_broth_stability_plot_multi(corrected_results, wavelength),
    blank_stability = create_blank_stability_plot_multi(corrected_results, wavelength),
    np_kinetics = create_np_kinetics_plot_multi(corrected_results, wavelength),
    before_after = create_before_after_plot_multi(corrected_results, wavelength)
  )
  
  # Add layout validation plot
  layout_validation <- validate_experiment_layout(processed_data)
  qc_plots$layout_validation = layout_validation$plot
  
  return(qc_plots)
}

create_broth_stability_plot <- function(corrected_results) {
  corrected_results$corrected_data %>%
    filter(sample_type == "broth") %>%  # Only broth controls
    ggplot(aes(x = time_hrs, y = od600_raw)) +
    geom_line(aes(group = well_id), alpha = 0.5) +
    geom_smooth(method = "loess", color = "firebrick", linewidth = 1) +
    labs(
      title = "QC: Broth Control Stability",
      subtitle = paste("Mean OD:", round(corrected_results$corrections$broth_correction, 4)),
      x = "Time (hours)",
      y = "Raw OD600"
    ) +
    theme_minimal()
}

#' Create broth stability plot for specific wavelength
create_broth_stability_plot_multi <- function(corrected_results, wavelength) {
  od_raw_column <- paste0("od", wavelength, "_raw")
  
  corrected_results$corrected_data %>%
    filter(sample_type == "broth") %>%
    ggplot(aes(x = time_hrs, y = .data[[od_raw_column]])) +
    geom_line(aes(group = well_id), alpha = 0.5) +
    geom_smooth(method = "loess", color = "firebrick", size = 1) +
    labs(
      title = paste("QC: Broth Control Stability -", wavelength, "nm"),
      subtitle = paste("Mean OD:", round(corrected_results$corrections$broth_correction, 4)),
      x = "Time (hours)",
      y = paste0("Raw OD", wavelength)
    ) +
    theme_minimal()
}

#' Create broth stability plot for specific wavelength
create_blank_stability_plot_multi <- function(corrected_results, wavelength) {
  od_raw_column <- paste0("od", wavelength, "_raw")
  
  # Get blank data and sort concentrations numerically
  blank_data <- corrected_results$corrected_data %>%
    filter(sample_type == "blank")
  
  if (nrow(blank_data) == 0) {
    cat("  • No blank control data found for", wavelength, "nm\n")
    return(ggplot() + 
             labs(title = paste("QC: No Blank Controls Found -", wavelength, "nm")) + 
             theme_minimal())
  }
  
  blank_concentrations <- unique(blank_data$concentration)
  blank_concentrations <- blank_concentrations[!is.na(blank_concentrations)]
  
  if (length(blank_concentrations) > 0) {
    conc_numbers <- as.numeric(str_extract(blank_concentrations, "\\d+"))
    blank_concentrations_sorted <- blank_concentrations[order(conc_numbers)]
    
    cat("  • Blank concentrations found:", paste(blank_concentrations_sorted, collapse = ", "), "\n")
    
    # Create plot with ordered concentrations
    blank_data %>%
      filter(!is.na(concentration)) %>%
      mutate(concentration_f = factor(concentration, levels = blank_concentrations_sorted)) %>%
      ggplot(aes(x = time_hrs, y = .data[[od_raw_column]])) +
      geom_line(aes(group = well_id), alpha = 0.5, color = "darkgreen") +
      geom_smooth(method = "loess", color = "darkgreen", size = 1) +
      facet_wrap(~concentration_f, scales = "fixed") +  # Fixed scale for comparison
      labs(
        title = paste("QC: Blank Control Stability -", wavelength, "nm"),
        subtitle = "Blank controls by position - Concentrations ordered numerically",
        x = "Time (hours)",
        y = paste0("Raw OD", wavelength)
      ) +
      theme_minimal() +
      theme(
        strip.text = element_text(face = "bold")
      )
  } else {
    # No concentration info for blanks - just show overall
    cat("  • Blank controls found but no concentration info\n")
    
    blank_data %>%
      ggplot(aes(x = time_hrs, y = .data[[od_raw_column]])) +
      geom_line(aes(group = well_id), alpha = 0.5, color = "darkgreen") +
      geom_smooth(method = "loess", color = "darkgreen", size = 1) +
      labs(
        title = paste("QC: Blank Control Stability -", wavelength, "nm"),
        subtitle = "Blank controls (should be near zero)",
        x = "Time (hours)",
        y = paste0("Raw OD", wavelength)
      ) +
      theme_minimal()
  }
}

create_blank_stability_plot <- function(corrected_results) {
  corrected_results$corrected_data %>%
    filter(sample_type == "blank") %>%  # Blank controls
    ggplot(aes(x = time_hrs, y = od600_raw)) +
    geom_line(aes(group = well_id), alpha = 0.5, color = "gray43") +
    geom_smooth(method = "loess", color = "gray43", linewidth = 1) +
    facet_wrap(~concentration, scales = "free_y") +
    labs(
      title = "QC: Blank Control Stability",
      subtitle = "Blank controls by position (should be near zero)",
      x = "Time (hours)",
      y = "Raw OD600"
    ) +
    theme_minimal()
}

create_blank_stability_plot_multi <- function(corrected_results, wavelength) {
  od_raw_column <- paste0("od", wavelength, "_raw")
  
  corrected_results$corrected_data %>%
    filter(sample_type == "blank") %>%
    ggplot(aes(x = time_hrs, y = .data[[od_raw_column]])) +
    geom_line(aes(group = well_id), alpha = 0.5, color = "darkgreen") +
    geom_smooth(method = "loess", color = "darkgreen", size = 1) +
    facet_wrap(~concentration, scales = "free_y") +
    labs(
      title = paste("QC: Blank Control Stability -", wavelength, "nm"),
      subtitle = "Blank controls by position (should be near zero)",
      x = "Time (hours)",
      y = paste0("Raw OD", wavelength)
    ) +
    theme_minimal()
}

create_np_kinetics_plot <- function(corrected_results) {
  
  # Get concentrations in proper numerical order
  np_data <- corrected_results$corrected_data %>%
    filter(sample_type == "np_control")
  
  # Extract and sort concentrations numerically
  np_concentrations <- unique(np_data$concentration)
  conc_numbers <- as.numeric(str_extract(np_concentrations, "\\d+"))
  np_concentrations_sorted <- np_concentrations[order(conc_numbers)]
  
  cat("• NP concentrations found:", paste(np_concentrations_sorted, collapse = ", "), "\n")
  
  # Create plot with ordered facets
  np_data %>%
    mutate(concentration_f = factor(concentration, levels = np_concentrations_sorted)) %>%
    ggplot(aes(x = time_hrs, y = od600_broth_corrected, color = concentration_f)) +
    geom_line(aes(group = well_id), alpha = 0.7) +
    stat_summary(fun = mean, geom = "line", linewidth = 1.2, color = "black") +
    facet_wrap(~concentration_f, scales = "fixed") +  # FIXED SCALE FOR COMPARISON
    labs(
      title = "QC: Nanoparticle Kinetics",
      subtitle = "Concentrations ordered numerically wiih fixed scale for comparison",
      x = "Time (hours)",
      y = "OD600 (broth corrected)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold")
    )
}

#' Create NP kinetics plot for specific wavelength
create_np_kinetics_plot_multi <- function(corrected_results, wavelength) {
  od_broth_corrected_column <- paste0("od", wavelength, "_broth_corrected")
  
  # Get concentrations in proper numerical order
  np_data <- corrected_results$corrected_data %>%
    filter(sample_type == "np_control")
  
  np_concentrations <- unique(np_data$concentration)
  conc_numbers <- as.numeric(str_extract(np_concentrations, "\\d+"))
  np_concentrations_sorted <- np_concentrations[order(conc_numbers)]
  
  # Create plot with ordered facets and fixed scale
  np_data %>%
    mutate(concentration_f = factor(concentration, levels = np_concentrations_sorted)) %>%
    ggplot(aes(x = time_hrs, y = .data[[od_broth_corrected_column]], color = concentration_f)) +
    geom_line(aes(group = well_id), alpha = 0.7) +
    stat_summary(fun = mean, geom = "line", linewidth = 1.2, color = "black") +
    facet_wrap(~concentration_f, scales = "fixed") +
    labs(
      title = paste("QC: Nanoparticle Kinetics -", wavelength, "nm"),
      subtitle = "Concentrations ordered numerically - Fixed scale for comparison",
      x = "Time (hours)",
      y = paste0("OD", wavelength, " (broth corrected)")
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold")
    ) +
    scale_color_viridis_d()
}

create_before_after_plot <- function(corrected_results) {
  
  # Get concentrations in proper numerical order
  sample_data <- corrected_results$corrected_data %>%
    filter(sample_type == "sample")
  
  sample_concentrations <- unique(sample_data$concentration)
  conc_numbers <- as.numeric(str_extract(sample_concentrations, "\\d+"))
  sample_concentrations_sorted <- sample_concentrations[order(conc_numbers)]
  
  cat("• Sample concentrations found:", paste(sample_concentrations_sorted, collapse = ", "), "\n")
  
  # Create the comparison data
  comparison_data <- sample_data %>%
    select(time_hrs, concentration, well_id, od600_raw, od600_final) %>%
    pivot_longer(cols = c(od600_raw, od600_final), 
                 names_to = "data_type", values_to = "od600") %>%
    mutate(
      data_type = case_when(
        data_type == "od600_raw" ~ "Raw",
        data_type == "od600_final" ~ "Corrected"
      ),
      concentration_f = factor(concentration, levels = sample_concentrations_sorted),
      data_type_f = factor(data_type, levels = c("Raw", "Corrected"))
    )
  
  # Create plot with fixed scale for comparison
  comparison_data %>%
    ggplot(aes(x = time_hrs, y = od600, color = data_type_f)) +
    stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
    stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA,
                 aes(fill = data_type_f)) +
    facet_wrap(~concentration_f, scales = "fixed") +  # Fixed scale for comparison
    labs(
      title = "QC: Raw vs Corrected Growth Curves",
      subtitle = "Concentrations ordered numerically - Fixed scale for comparison",
      x = "Time (hours)",
      y = "OD600",
      color = "Data Type",
      fill = "Data Type"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    ) +
    scale_color_manual(values = c("Raw" = "#E31A1C", "Corrected" = "#1F78B4")) +
    scale_fill_manual(values = c("Raw" = "#E31A1C", "Corrected" = "#1F78B4"))
}

create_before_after_plot_multi <- function(corrected_results, wavelength) {
  od_raw_column <- paste0("od", wavelength, "_raw")
  od_final_column <- paste0("od", wavelength, "_final")
  
  # Get concentrations in proper numerical order
  sample_data <- corrected_results$corrected_data %>%
    filter(sample_type == "sample")
  
  sample_concentrations <- unique(sample_data$concentration)
  conc_numbers <- as.numeric(str_extract(sample_concentrations, "\\d+"))
  sample_concentrations_sorted <- sample_concentrations[order(conc_numbers)]
  
  cat("  • Sample concentrations found:", paste(sample_concentrations_sorted, collapse = ", "), "\n")
  
  # Create the comparison data
  comparison_data <- sample_data %>%
    select(time_hrs, concentration, well_id, !!od_raw_column, !!od_final_column) %>%
    pivot_longer(cols = c(!!od_raw_column, !!od_final_column), 
                 names_to = "data_type", values_to = "od_value") %>%
    mutate(
      data_type = case_when(
        str_detect(data_type, "_raw") ~ "Raw",
        str_detect(data_type, "_final") ~ "Corrected"
      ),
      concentration_f = factor(concentration, levels = sample_concentrations_sorted),
      data_type_f = factor(data_type, levels = c("Raw", "Corrected"))
    )
  
  # Create plot with fixed scale for comparison
  comparison_data %>%
    ggplot(aes(x = time_hrs, y = od_value, color = data_type_f)) +
    stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
    stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA,
                 aes(fill = data_type_f)) +
    facet_wrap(~concentration_f, scales = "fixed") +  # Fixed scale for comparison
    labs(
      title = paste("QC: Raw vs Corrected Growth Curves -", wavelength, "nm"),
      subtitle = "Concentrations ordered numerically - Fixed scale for comparison",
      x = "Time (hours)",
      y = paste0("OD", wavelength),
      color = "Data Type",
      fill = "Data Type"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    ) +
    scale_color_manual(values = c("Raw" = "#E31A1C", "Corrected" = "#1F78B4")) +
    scale_fill_manual(values = c("Raw" = "#E31A1C", "Corrected" = "#1F78B4"))
}

#' Validate and visualize experimental layout
validate_experiment_layout <- function(processed_data) {
  
  layout_viz <- processed_data$data %>%
    distinct(well_id, sample_type, concentration, replicate_id, row_letter, col_number) %>%
    mutate(
      row_num = as.numeric(factor(row_letter, levels = LETTERS[1:8])),
      display_label = case_when(
        sample_type == "sample" ~ paste0("S", str_extract(concentration, "\\d+"), "\n", replicate_id),
        sample_type == "np_control" ~ paste0("NP", str_extract(concentration, "\\d+")),
        sample_type == "untreated_control" ~ paste0("UNT", str_extract(concentration, "\\d+")),
        sample_type == "blank" ~ paste0("BLK", str_extract(concentration, "\\d+")),  # Blank controls
        sample_type == "broth" ~ "BROTH",                                            # Broth controls
        TRUE ~ "?"
      )
    ) %>%
    ggplot(aes(x = col_number, y = 9 - row_num)) +
    geom_tile(aes(fill = sample_type), color = "white", linewidth = 1) +
    geom_text(aes(label = display_label), size = 2.5, color = "white", fontface = "bold") +
    scale_x_continuous(breaks = 1:12, expand = c(0,0)) +
    scale_y_continuous(breaks = 1:8, labels = LETTERS[8:1], expand = c(0,0)) +
    scale_fill_viridis_d(name = "Sample Type", option = "E",
                         begin = 0, end = 0.8) +
    labs(
      title = "Experimental Layout Validation",
      subtitle = "S# = Sample, NP# = NP control, UNT# = Untreated, BLK# = Blank, BROTH = Broth control",
      x = "Column",
      y = "Row",
      caption = "Rep_A, Rep_B, Rep_C = Biological replicates | Blank_E, Blank_F, Blank_G = Blank replicates"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_blank()
    ) +
    coord_fixed()
  
  # Print summary
  cat("=== LAYOUT VALIDATION ===\n")
  
  # Sample replicates
  cat("Sample replicates by concentration:\n")
  sample_summary <- processed_data$data %>%
    filter(sample_type == "sample") %>%
    distinct(concentration, replicate_id, well_id) %>%
    arrange(concentration, replicate_id)
  print(sample_summary)
  
  # Blank replicates
  cat("\nBlank replicates by 'concentration':\n")
  blank_summary <- processed_data$data %>%
    filter(sample_type == "blank") %>%
    distinct(concentration, replicate_id, well_id) %>%
    arrange(concentration, replicate_id)
  print(blank_summary)
  
  return(list(
    plot = layout_viz,
    replicates = sample_summary,
    blanks = blank_summary
  ))
}


# ===== ACCESSOR FUNCTIONS =====
#' Export data in wide format with proper replicate handling
#' @param results Results from analyze_growth_curves_with_summaries()
#' @param data_type Which data to export: "raw", "broth_corrected", "corrected"  
#' @param sample_types Which sample types to include: "sample", "untreated_control", or both
#' @return Wide format data frame with proper column ordering
export_wide_format <- function(results, data_type = "corrected", 
                               sample_types = "sample") {
  
  # Select the appropriate OD column
  od_column <- case_when(
    data_type == "raw" ~ "od600_raw",
    data_type == "broth_corrected" ~ "od600_broth_corrected", 
    data_type == "corrected" ~ "od600_final",
    TRUE ~ "od600_final"
  )
  
  # Get the data
  data <- results$corrected_results$corrected_data %>%
    filter(sample_type %in% sample_types) %>%
    select(time_hrs, concentration, replicate_id, well_id, sample_type, !!sym(od_column)) %>%
    rename(od600 = !!sym(od_column))
  
  # Get unique time points
  time_points <- sort(unique(data$time_hrs))
  
  # Get concentrations in proper numerical order
  conc_raw <- unique(data$concentration)
  conc_numbers <- as.numeric(str_extract(conc_raw, "\\d+"))
  conc_order <- order(conc_numbers)
  conc_sorted <- conc_raw[conc_order]
  
  # Initialize result with time column
  result <- tibble(time_hrs = time_points)
  
  # Handle different sample types differently
  if ("sample" %in% sample_types) {
    # Samples have Rep_A, Rep_B, Rep_C
    sample_data <- data %>% filter(sample_type == "sample")
    
    # Add Rep A columns
    for (conc in conc_sorted) {
      rep_data <- sample_data %>%
        filter(concentration == conc, replicate_id == "Rep_A") %>%
        select(time_hrs, od600) %>%
        arrange(time_hrs)
      
      col_name <- paste(conc, "Rep_A", sep = "_")
      result <- result %>%
        left_join(rep_data %>% rename(!!col_name := od600), by = "time_hrs")
    }
    
    # Add Rep B columns
    for (conc in conc_sorted) {
      rep_data <- sample_data %>%
        filter(concentration == conc, replicate_id == "Rep_B") %>%
        select(time_hrs, od600) %>%
        arrange(time_hrs)
      
      col_name <- paste(conc, "Rep_B", sep = "_")
      result <- result %>%
        left_join(rep_data %>% rename(!!col_name := od600), by = "time_hrs")
    }
    
    # Add Rep C columns
    for (conc in conc_sorted) {
      rep_data <- sample_data %>%
        filter(concentration == conc, replicate_id == "Rep_C") %>%
        select(time_hrs, od600) %>%
        arrange(time_hrs)
      
      col_name <- paste(conc, "Rep_C", sep = "_")
      result <- result %>%
        left_join(rep_data %>% rename(!!col_name := od600), by = "time_hrs")
    }
    
    # Add sample averages and SDs
    for (conc in conc_sorted) {
      avg_data <- sample_data %>%
        filter(concentration == conc) %>%
        group_by(time_hrs) %>%
        summarise(avg_od = mean(od600, na.rm = TRUE), .groups = "drop")
      
      col_name <- paste("AVERAGE", conc, sep = "_")
      result <- result %>%
        left_join(avg_data %>% rename(!!col_name := avg_od), by = "time_hrs")
    }
    
    for (conc in conc_sorted) {
      sd_data <- sample_data %>%
        filter(concentration == conc) %>%
        group_by(time_hrs) %>%
        summarise(sd_od = sd(od600, na.rm = TRUE), .groups = "drop")
      
      col_name <- paste("SD", conc, sep = "_")
      result <- result %>%
        left_join(sd_data %>% rename(!!col_name := sd_od), by = "time_hrs")
    }
  }
  
  if ("untreated_control" %in% sample_types) {
    # Untreated controls have Rep_UNT (single replicate)
    untreated_data <- data %>% filter(sample_type == "untreated_control")
    
    # Add untreated columns (they only have Rep_UNT)
    for (conc in conc_sorted) {
      rep_data <- untreated_data %>%
        filter(concentration == conc, replicate_id == "Rep_UNT") %>%
        select(time_hrs, od600) %>%
        arrange(time_hrs)
      
      col_name <- paste(conc, "Rep_UNT", sep = "_")
      result <- result %>%
        left_join(rep_data %>% rename(!!col_name := od600), by = "time_hrs")
    }
    
    # For untreated, AVERAGE = the single value, SD = NA
    for (conc in conc_sorted) {
      avg_data <- untreated_data %>%
        filter(concentration == conc) %>%
        select(time_hrs, od600) %>%
        arrange(time_hrs)
      
      col_name <- paste("AVERAGE", conc, sep = "_")
      result <- result %>%
        left_join(avg_data %>% rename(!!col_name := od600), by = "time_hrs")
    }
    
    # SD columns will be NA for untreated (single replicate)
    for (conc in conc_sorted) {
      result <- result %>%
        mutate(!!paste("SD", conc, sep = "_") := NA_real_)
    }
  }
  
  return(result)
}

#' Separate function for untreated controls (cleaner approach)
export_untreated_wide <- function(results, data_type = "corrected") {
  
  # Select the appropriate OD column
  od_column <- case_when(
    data_type == "raw" ~ "od600_raw",
    data_type == "broth_corrected" ~ "od600_broth_corrected", 
    data_type == "corrected" ~ "od600_final",
    TRUE ~ "od600_final"
  )
  
  # Get untreated data
  data <- results$corrected_results$corrected_data %>%
    filter(sample_type == "untreated_control") %>%
    select(time_hrs, concentration, !!sym(od_column)) %>%
    rename(od600 = !!sym(od_column))
  
  # Get unique time points
  time_points <- sort(unique(data$time_hrs))
  
  # Get concentrations in proper numerical order
  conc_raw <- unique(data$concentration)
  conc_numbers <- as.numeric(str_extract(conc_raw, "\\d+"))
  conc_order <- order(conc_numbers)
  conc_sorted <- conc_raw[conc_order]
  
  # Create wide format - simpler for untreated (single replicate)
  result <- data %>%
    pivot_wider(
      names_from = concentration,
      values_from = od600,
      id_cols = time_hrs
    ) %>%
    arrange(time_hrs)
  
  # Reorder columns
  time_col <- "time_hrs"
  conc_cols <- intersect(conc_sorted, names(result))
  result <- result %>%
    select(all_of(c(time_col, conc_cols)))
  
  return(result)
}

#' Quick diagnostic function to check replicate structure
check_replicate_structure <- function(results) {
  
  cat("=== REPLICATE STRUCTURE CHECK ===\n")
  
  replicate_summary <- results$corrected_results$corrected_data %>%
    distinct(sample_type, concentration, replicate_id, well_id) %>%
    arrange(sample_type, concentration, replicate_id)
  
  # Group by sample type
  for (stype in unique(replicate_summary$sample_type)) {
    cat("\n", toupper(stype), ":\n")
    
    type_data <- replicate_summary %>%
      filter(sample_type == stype)
    
    print(type_data)
  }
  
  cat("\n=== END CHECK ===\n")
  
  return(replicate_summary)
}

#' Get data in colleague's preferred format
#' @param results Results from main analysis
#' @param include_untreated Include untreated controls as well
#' @param export_to_csv Optionally export to CSV file
#' @return List with sample data and optionally untreated data
get_colleague_format <- function(results, include_untreated = TRUE, 
                                 export_to_csv = FALSE) {
  
  cat("=== EXPORTING DATA IN COLLEAGUE FORMAT ===\n")
  
  # Get sample data (has Rep_A, Rep_B, Rep_C structure)
  sample_data <- export_wide_format(results, data_type = "corrected", 
                                    sample_types = "sample")
  
  cat("Sample data exported:", nrow(sample_data), "timepoints x", 
      ncol(sample_data) - 1, "measurement columns\n")
  
  output <- list(samples = sample_data)
  
  # Get untreated data if requested (simpler structure)
  if (include_untreated) {
    untreated_data <- export_untreated_wide(results, data_type = "corrected")
    
    cat("Untreated data exported:", nrow(untreated_data), "timepoints x", 
        ncol(untreated_data) - 1, "concentration columns\n")
    
    output$untreated <- untreated_data
  }
  
  # Export to CSV if requested
  if (export_to_csv) {
    base_name <- "growth_curve_data"
    
    write_csv(sample_data, paste0(base_name, "_samples.csv"))
    cat("Samples exported to:", paste0(base_name, "_samples.csv"), "\n")
    
    if (include_untreated) {
      write_csv(untreated_data, paste0(base_name, "_untreated.csv"))
      cat("Untreated exported to:", paste0(base_name, "_untreated.csv"), "\n")
    }
  }
  
  cat("=== EXPORT COMPLETE ===\n")
  
  return(output)
}

#' Quick accessor for just the samples in wide format
get_samples_wide <- function(results) {
  export_wide_format(results, data_type = "corrected", sample_types = "sample")
}

#' Quick accessor for raw data (no corrections)
get_samples_wide_raw <- function(results) {
  export_wide_format(results, data_type = "raw", sample_types = "sample")
}

#' Preview the wide format structure
preview_wide_format <- function(results) {
  
  cat("=== WIDE FORMAT PREVIEW ===\n")
  
  sample_wide <- get_samples_wide(results)
  
  cat("Dimensions:", nrow(sample_wide), "rows x", ncol(sample_wide), "columns\n")
  cat("Time range:", round(min(sample_wide$time_hrs), 2), "to", 
      round(max(sample_wide$time_hrs), 2), "hours\n")
  
  cat("\nColumn structure:\n")
  col_names <- names(sample_wide)
  
  cat("Time: time_hrs\n")
  
  rep_a_cols <- col_names[str_detect(col_names, "_Rep_A$")]
  if (length(rep_a_cols) > 0) {
    cat("Replicate A columns:", length(rep_a_cols), 
        "(", paste(head(rep_a_cols, 3), collapse = ", "), 
        if(length(rep_a_cols) > 3) "..." else "", ")\n")
  }
  
  rep_b_cols <- col_names[str_detect(col_names, "_Rep_B$")]
  if (length(rep_b_cols) > 0) {
    cat("Replicate B columns:", length(rep_b_cols), 
        "(", paste(head(rep_b_cols, 3), collapse = ", "), 
        if(length(rep_b_cols) > 3) "..." else "", ")\n")
  }
  
  rep_c_cols <- col_names[str_detect(col_names, "_Rep_C$")]
  if (length(rep_c_cols) > 0) {
    cat("Replicate C columns:", length(rep_c_cols), 
        "(", paste(head(rep_c_cols, 3), collapse = ", "), 
        if(length(rep_c_cols) > 3) "..." else "", ")\n")
  }
  
  avg_cols <- col_names[str_detect(col_names, "^AVERAGE_")]
  if (length(avg_cols) > 0) {
    cat("Average columns:", length(avg_cols), 
        "(", paste(head(avg_cols, 3), collapse = ", "), 
        if(length(avg_cols) > 3) "..." else "", ")\n")
  }
  
  sd_cols <- col_names[str_detect(col_names, "^SD_")]
  if (length(sd_cols) > 0) {
    cat("SD columns:", length(sd_cols), 
        "(", paste(head(sd_cols, 3), collapse = ", "), 
        if(length(sd_cols) > 3) "..." else "", ")\n")
  }
  
  cat("\nFirst few rows:\n")
  print(head(sample_wide[, 1:min(6, ncol(sample_wide))], 3))
  
  cat("\n=== PREVIEW COMPLETE ===\n")
  
  return(invisible(sample_wide))
}

#' Get data for a specific concentration
#' @param results Results from main analysis
#' @param concentration Which concentration to extract (e.g., "Conc_5")
#' @param sample_type Which sample type: "sample", "untreated_control", or "both"
#' @param data_type Which data: "raw", "broth_corrected", "corrected"  
#' @param format Output format: "long" or "wide"
#' @return Data for the specified concentration
get_concentration_data <- function(results, concentration, 
                                   sample_type = "sample", 
                                   data_type = "corrected",
                                   format = "wide") {
  
  # Select the appropriate OD column
  od_column <- case_when(
    data_type == "raw" ~ "od600_raw",
    data_type == "broth_corrected" ~ "od600_broth_corrected", 
    data_type == "corrected" ~ "od600_final",
    TRUE ~ "od600_final"
  )
  
  # Determine sample types to include
  if (sample_type == "both") {
    sample_types <- c("sample", "untreated_control")
  } else {
    sample_types <- sample_type
  }
  
  # Get the data
  data <- results$corrected_results$corrected_data %>%
    filter(sample_type %in% sample_types,
           concentration == !!concentration) %>%
    select(time_hrs, datetime, well_id, sample_type, replicate_id, 
           concentration, !!sym(od_column)) %>%
    rename(od600 = !!sym(od_column)) %>%
    arrange(time_hrs, sample_type, replicate_id)
  
  if (nrow(data) == 0) {
    cat("No data found for concentration:", concentration, "\n")
    cat("Available concentrations:\n")
    available_concs <- unique(results$corrected_results$corrected_data$concentration)
    cat(paste(available_concs, collapse = ", "), "\n")
    return(NULL)
  }
  
  if (format == "wide") {
    # Convert to wide format
    data_wide <- data %>%
      select(time_hrs, sample_type, replicate_id, od600) %>%
      unite("sample_replicate", sample_type, replicate_id, sep = "_") %>%
      pivot_wider(names_from = sample_replicate, values_from = od600) %>%
      arrange(time_hrs)
    
    return(data_wide)
  } else {
    return(data)
  }
}

#' Get multiple concentrations at once
#' @param results Results from main analysis
#' @param concentrations Vector of concentrations (e.g., c("Conc_1", "Conc_5", "Conc_10"))
#' @param sample_type Which sample type: "sample", "untreated_control", or "both"
#' @param data_type Which data: "raw", "broth_corrected", "corrected"
#' @return List of data frames, one per concentration
get_multiple_concentrations <- function(results, concentrations,
                                        sample_type = "sample",
                                        data_type = "corrected") {
  
  conc_data <- map(concentrations, ~get_concentration_data(
    results = results,
    concentration = .x,
    sample_type = sample_type,
    data_type = data_type,
    format = "long"
  ))
  
  names(conc_data) <- concentrations
  
  # Remove any NULL entries (concentrations not found)
  conc_data <- conc_data[!map_lgl(conc_data, is.null)]
  
  return(conc_data)
}

#' Quick concentration extractor from wide format data
#' @param wide_data Wide format data from get_colleague_format()
#' @param concentration Which concentration to extract (e.g., "Conc_5")
#' @param include_stats Include AVERAGE and SD columns
#' @return Subset of wide data for that concentration
extract_concentration_from_wide <- function(wide_data, concentration, 
                                            include_stats = TRUE) {
  
  # Find columns for this concentration
  base_cols <- "time_hrs"
  rep_cols <- names(wide_data)[str_detect(names(wide_data), paste0("^", concentration, "_Rep_"))]
  
  cols_to_keep <- c(base_cols, rep_cols)
  
  if (include_stats) {
    avg_col <- paste("AVERAGE", concentration, sep = "_")
    sd_col <- paste("SD", concentration, sep = "_")
    
    if (avg_col %in% names(wide_data)) cols_to_keep <- c(cols_to_keep, avg_col)
    if (sd_col %in% names(wide_data)) cols_to_keep <- c(cols_to_keep, sd_col)
  }
  
  result <- wide_data %>%
    select(all_of(cols_to_keep))
  
  return(result)
}

#' Interactive concentration browser
browse_concentrations <- function(results) {
  
  cat("=== CONCENTRATION BROWSER ===\n")
  
  # Show available concentrations
  available_concs <- results$corrected_results$corrected_data %>%
    filter(!is.na(concentration)) %>%
    distinct(concentration) %>%
    arrange(concentration) %>%
    pull(concentration)
  
  cat("Available concentrations:\n")
  for (i in seq_along(available_concs)) {
    cat(sprintf("%2d. %s\n", i, available_concs[i]))
  }
  
  # Show sample counts per concentration
  cat("\nSample counts by concentration:\n")
  sample_counts <- results$corrected_results$corrected_data %>%
    filter(!is.na(concentration)) %>%
    distinct(concentration, sample_type, well_id) %>%
    count(concentration, sample_type) %>%
    pivot_wider(names_from = sample_type, values_from = n, values_fill = 0)
  
  print(sample_counts)
  
  cat("\n=== USAGE EXAMPLES ===\n")
  cat("# Get data for concentration 5:\n")
  cat('conc5_data <- get_concentration_data(results, "Conc_5")\n')
  cat("\n# Get multiple concentrations:\n")
  cat('multi_data <- get_multiple_concentrations(results, c("Conc_1", "Conc_5", "Conc_10"))\n')
  cat("\n# Extract from wide format:\n")
  cat('colleague_data <- get_colleague_format(results)\n')
  cat('conc5_wide <- extract_concentration_from_wide(colleague_data$samples, "Conc_5")\n')
  
  return(available_concs)
}

#' Display the composite plot
show_plate_composite <- function(results) {
  print(results$plots$plate_composite$combined_2x2)
}

#' Display the final plate layout
show_plate_endpoint <- function(results) {
  print(results$plots$final_plate)
}

#' Display the initial plate layout  
show_plate_initial_condition <- function(results) {
  print(results$plots$initial_plate)
}

#' Show main growth curves
show_growth_curves <- function(results) {
  print(results$plots$growth_curves)
}

#' Show experimental layout validation
show_layout_validation <- function(results) {
  print(results$plots$qc_plots$layout_validation)
}

#' Show all QC plots at once
show_qc_plots <- function(results) {
  cat("📊 Quality Control Plots:\n\n")
  
  cat("1. Broth Stability:\n")
  print(results$plots$qc_plots$broth_stability)
  
  cat("\n2. Blank Stability:\n") 
  print(results$plots$qc_plots$blank_stability)
  
  cat("\n3. NP Kinetics:\n")
  print(results$plots$qc_plots$np_kinetics)
  
  cat("\n4. Before vs After Correction:\n")
  print(results$plots$qc_plots$before_after)
}

#' Get the final corrected data in long format
get_corrected_data <- function(results) {
  results$corrected_results$corrected_data
}

#' Get the raw data (no corrections)
get_raw_data <- function(results) {
  results$corrected_results$raw_data
}

#' Get metadata from the experiment
get_experiment_metadata <- function(results) {
  results$processed_data$metadata
}

#' Show experiment overview
show_experiment_overview <- function(results) {
  if (!"summaries" %in% names(results)) {
    cat("⚠️  Summaries not available. Run analyze_growth_curves_with_summaries() first.\n")
    return(invisible())
  }
  
  cat("EXPERIMENT OVERVIEW\n")
  cat(strrep("=", 50), "\n") 
  
  overview <- results$summaries$experiment$basic_info
  cat("• Total wells:", overview$total_wells, "\n")
  cat("•️ Duration:", round(overview$experiment_duration_hrs, 1), "hours\n")
  cat("• Timepoints:", overview$total_timepoints, "\n")
  cat("•️ Temperature:", overview$temperature_range, "\n")
  
  cat("\n• Sample Distribution on Plate:\n")
  print(results$summaries$experiment$sample_distribution)
}

#' Accessor functions for concentration plots
show_concentration_plot <- function(results, concentration, include_untreated = TRUE) {
  p <- plot_concentration(results$corrected_results, concentration, include_untreated)
  if (!is.null(p)) print(p)
  return(p)
}

show_multiple_concentrations <- function(results, concentrations, 
                                         separate_panels = TRUE, 
                                         free_scale = FALSE) {
  p <- plot_multiple_concentrations(results$corrected_results, concentrations, 
                                    separate_panels = separate_panels, 
                                    free_scale = free_scale)
  print(p)
  return(p)
}

show_all_concentrations <- function(results, sample_types = "sample", 
                                    free_scale = FALSE) {
  p <- plot_concentrations_panel(results$corrected_results, NULL, sample_types, 
                                 "corrected", free_scale)
  print(p)
  return(p)
}

#' Function to save the composite plot
save_plate_composite <- function(results, filename = "plate_composite.png", 
                                 width = 16, height = 12, dpi = 300) {
  
  ggsave(filename, 
         plot = results$plots$plate_composite$combined_2x2,
         width = width, height = height, dpi = dpi)
  
  cat("Composite plate heatmap saved to:", filename, "\n")
}

#' Show available plotting styles
show_plot_styles <- function(results, concentration = NULL) {
  
  if (is.null(concentration)) {
    # Pick the first available concentration
    concentration <- results$corrected_results$corrected_data %>%
      filter(!is.na(concentration)) %>%
      pull(concentration) %>%
      unique() %>%
      sort() %>%
      first()
  }
  
  cat("=== AVAILABLE PLOT STYLES ===\n")
  cat("Showing examples for:", concentration, "\n\n")
  
  styles <- list(
    separated = plot_concentration(results, concentration, style = "separated"),
    jittered = plot_concentration(results, concentration, style = "jittered"),
    with_ribbons = plot_concentration(results, concentration, style = "with_ribbons"),
    default = plot_concentration(results, concentration, style = "default")
  )
  
  cat("Available styles:\n")
  cat("1. 'separated' - Uses facets to separate sample types\n")
  cat("2. 'jittered' - Adds small random offsets to separate overlapping points\n")  
  cat("3. 'with_ribbons' - Shows individual points plus mean with error bands\n")
  cat("4. 'default' - Improved colors, shapes, and line types\n\n")
  
  cat("Usage:\n")
  cat("plot_concentration(results, 'Conc_5', style = 'separated')\n")
  cat("plot_concentration(results, 'Conc_5', style = 'with_ribbons')\n\n")
  
  return(styles)
}

# ===== MAIN ANALYSIS FUNCTIONS =====

#' Complete analysis pipeline
analyze_growth_curves <- function(file_path, layout_type = "default") {
  
  cat("\n")
  cat("🧬 === SYNERGY HTX GROWTH CURVE ANALYSIS ===\n")
  cat("📁 File:", basename(file_path), "\n\n")
  
  # Process raw data
  cat("1️⃣  Processing raw data...\n")
  processed_data <- process_synergy_file(file_path, layout_type = layout_type)
  
  # Apply corrections
  cat("\n2️⃣  Applying corrections...\n")
  corrected_results <- process_with_corrections(processed_data)
  
  # Create visualizations
  cat("\n3️⃣  Creating visualizations...\n")
  plots <- list(
    growth_curves = plot_growth_curves(corrected_results),
    initial_plate = plot_plate_heatmap(corrected_results, "first", "corrected"),
    final_plate = plot_plate_heatmap(corrected_results, "last", "corrected"),
    plate_composite = plot_plate_heatmap_composite(corrected_results),
    qc_plots = create_qc_plots(corrected_results, processed_data)
  )
  
  cat("✅ Visualizations created successfully\n")
  
  cat("\n🎉 === ANALYSIS COMPLETE ===\n")
  cat("📊 Results available in returned object\n")
  cat("📈 Use print(results$plots$growth_curves) to view main plot\n")
  cat("🔍 Use print(results$plots$final_plate) to view final plate layout\n\n")
  
  return(list(
    processed_data = processed_data,
    corrected_results = corrected_results,
    plots = plots
  ))
}

#' Enhanced main analysis function for multiple wavelengths
analyze_growth_curves_multi <- function(file_path, layout_type = "default") {
  
  cat("\n")
  cat("• === SYNERGY HTX MULTI-WAVELENGTH ANALYSIS ===\n")
  cat("• File:", basename(file_path), "\n\n")
  
  # Process raw data with multiple wavelengths
  cat("• Step 1: Processing multi-wavelength data...\n")
  processed_data <- process_synergy_file_multi(file_path, layout_type = layout_type)
  
  # Process each wavelength separately
  results_by_wavelength <- list()
  
  for (wavelength_name in names(processed_data$wavelength_data)) {
    
    wavelength_processed <- processed_data$wavelength_data[[wavelength_name]]
    wavelength <- wavelength_processed$wavelength
    
    cat("• Step 2: Processing", wavelength, "nm data...\n")
    
    # Apply corrections for this wavelength
    corrected_results <- process_with_corrections_multi(wavelength_processed, wavelength)
    
    # Create visualizations for this wavelength
    cat("• Step 3: Creating", wavelength, "nm visualizations...\n")
    plots <- list(
      growth_curves = plot_growth_curves_multi(corrected_results, wavelength),
      initial_plate = plot_plate_heatmap_multi(corrected_results, "first", "corrected", wavelength),
      final_plate = plot_plate_heatmap_multi(corrected_results, "last", "corrected", wavelength),
      plate_composite = plot_plate_heatmap_composite_multi(corrected_results, wavelength),
      qc_plots = create_qc_plots_multi(corrected_results, wavelength_processed, wavelength)
    )
    
    results_by_wavelength[[wavelength_name]] <- list(
      processed_data = wavelength_processed,
      corrected_results = corrected_results,
      plots = plots,
      wavelength = wavelength
    )
    
    cat("  ✓", wavelength, "nm analysis complete\n")
  }
  
  cat("\n• === MULTI-WAVELENGTH ANALYSIS COMPLETE ===\n")
  cat("• Wavelengths processed:", length(results_by_wavelength), "\n")
  cat("• Access results: results$wavelength_600, results$wavelength_420, etc.\n\n")
  
  return(results_by_wavelength)
}

#' Simple wrapper that uses multi for everything and handles single wavelength appropriately
analyze_growth_curves_smart_v2 <- function(file_path, layout_type = "default") {
  
  cat("\n")
  cat("• === SMART GROWTH CURVE ANALYSIS ===\n")
  cat("• File:", basename(file_path), "\n\n")
  
  # Always use the multi-wavelength analysis (it handles single wavelength too)
  multi_results <- analyze_growth_curves_multi(file_path, layout_type)
  
  # Check how many wavelengths we got
  n_wavelengths <- length(multi_results)
  cat("• Detected", n_wavelengths, "wavelength(s)\n")
  
  if (n_wavelengths == 1) {
    # Single wavelength - return it in a format compatible with original functions
    wavelength_key <- names(multi_results)[1]
    single_result <- multi_results[[wavelength_key]]
    
    cat("• Single wavelength detected - providing compatible format\n")
    
    # Adapt for compatibility with original accessor functions
    adapted_result <- adapt_single_wavelength_result(single_result)
    
    return(adapted_result)
    
  } else {
    # Multiple wavelengths - return as-is
    cat("• Multiple wavelengths - returning multi-wavelength format\n")
    return(multi_results)
  }
}

#' Adapt single wavelength result for compatibility with original functions
adapt_single_wavelength_result <- function(single_result) {
  
  wavelength <- single_result$wavelength
  
  # Create column name mappings
  od_raw_col <- paste0("od", wavelength, "_raw")
  od_broth_col <- paste0("od", wavelength, "_broth_corrected")
  od_final_col <- paste0("od", wavelength, "_final")
  
  # Adapt the corrected data to have standard column names
  adapted_corrected_data <- single_result$corrected_results$corrected_data
  
  # Rename columns to standard format if they exist
  if (od_raw_col %in% names(adapted_corrected_data)) {
    adapted_corrected_data <- adapted_corrected_data %>%
      rename(od600_raw = !!od_raw_col)
  }
  
  if (od_broth_col %in% names(adapted_corrected_data)) {
    adapted_corrected_data <- adapted_corrected_data %>%
      rename(od600_broth_corrected = !!od_broth_col)
  }
  
  if (od_final_col %in% names(adapted_corrected_data)) {
    adapted_corrected_data <- adapted_corrected_data %>%
      rename(od600_final = !!od_final_col)
  }
  
  # Add od600 column if it doesn't exist
  if (!"od600" %in% names(adapted_corrected_data) && "od600_raw" %in% names(adapted_corrected_data)) {
    adapted_corrected_data <- adapted_corrected_data %>%
      mutate(od600 = od600_raw)
  }
  
  # Create adapted corrected results
  adapted_corrected_results <- single_result$corrected_results
  adapted_corrected_results$corrected_data <- adapted_corrected_data
  
  # Remove wavelength column from replicate stats if it exists
  if ("wavelength" %in% names(adapted_corrected_results$replicate_stats)) {
    adapted_corrected_results$replicate_stats <- adapted_corrected_results$replicate_stats %>%
      select(-wavelength)
  }
  
  return(list(
    processed_data = single_result$processed_data,
    corrected_results = adapted_corrected_results,
    plots = single_result$plots,
    wavelength = wavelength
  ))
}

#' Complete analysis with summaries and diagnostics
analyze_growth_curves_with_summaries <- function(file_path, layout_type = "default", 
                                                 export_summaries = FALSE, 
                                                 run_diagnostics = TRUE) {
  
  # Run main analysis
  results <- analyze_growth_curves(file_path, layout_type)
  
  # Run diagnostics first
  if (run_diagnostics) {
    cat("4️⃣  Running data diagnostics...\n")
    diagnostics <- diagnose_data_issues(results$corrected_results)
    results$diagnostics <- diagnostics
  }
  
  # Generate summaries
  cat("\n5️⃣  Generating summaries...\n")
  suppressMessages({  # Suppress dplyr grouping messages
    summaries <- summarize_replicate_results(results$corrected_results)
  })
  
  # Create report
  display_summaries(summaries)
  
  # Export if requested
  if (export_summaries) {
    export_filename <- paste0(tools::file_path_sans_ext(basename(file_path)), 
                              "_summaries.xlsx")
    cat("📋 Exporting summaries to Excel...\n")
    export_summaries_to_excel(summaries, export_filename)
  }
  
  results$summaries <- summaries
  
  cat("\n🎯 Analysis complete! Your results are ready.\n")
  
  return(results)
}

#' Complete analysis with layout verification
analyze_growth_curves_with_verification <- function(file_path, layout_type = "default", 
                                                    export_summaries = FALSE, 
                                                    run_diagnostics = TRUE) {
  
  # Run main analysis
  results <- analyze_growth_curves(file_path, layout_type)
  
  # VERIFY EXPERIMENTAL LAYOUT FIRST
  cat("\n4. Verifying experimental layout...\n")
  layout_verification <- verify_experimental_layout(results$processed_data)
  results$layout_verification <- layout_verification
  
  # Run diagnostics
  if (run_diagnostics) {
    cat("\n5. Running data diagnostics...\n")
    diagnostics <- diagnose_data_issues(results$corrected_results)
    results$diagnostics <- diagnostics
  }
  
  # Generate summaries
  cat("\n6. Generating summaries...\n")
  summaries <- summarize_replicate_results(results$corrected_results)
  
  # Create report with layout verification
  display_summaries(summaries)  # Now includes layout verification
  
  # Export if requested
  if (export_summaries) {
    export_filename <- paste0(tools::file_path_sans_ext(basename(file_path)), 
                              "_summaries.xlsx")
    export_summaries_to_excel(summaries, export_filename)
  }
  
  results$summaries <- summaries
  
  return(results)
}

#' Smart analysis function that handles both single and multi-wavelength data
analyze_growth_curves_smart <- function(file_path, layout_type = "default") {
  
  cat("\n")
  cat("• === SYNERGY HTX SMART ANALYSIS ===\n")
  cat("• File:", basename(file_path), "\n\n")
  
  # First, try to detect wavelengths
  cat("• Step 1: Detecting wavelength structure...\n")
  
  # Read file structure
  all_data <- suppressMessages(suppressWarnings(
    read_excel(file_path, col_names = FALSE)
  ))
  
  # Find wavelength sections
  wavelength_sections <- find_wavelength_sections(all_data)
  n_wavelengths <- length(wavelength_sections)
  
  cat("• Found", n_wavelengths, "wavelength section(s)\n")
  
  if (n_wavelengths > 1) {
    # Multiple wavelengths - use multi-wavelength analysis
    cat("• Using multi-wavelength analysis pipeline\n")
    results <- analyze_growth_curves_multi(file_path, layout_type)
    
  } else if (n_wavelengths == 1) {
    # Single wavelength - use original analysis but with wavelength structure
    wavelength_info <- wavelength_sections[[1]]
    wavelength <- wavelength_info$wavelength
    
    cat("• Single wavelength detected:", wavelength, "nm\n")
    cat("• Using adapted single-wavelength analysis\n")
    
    results <- analyze_single_wavelength_adapted(file_path, layout_type, wavelength_info)
    
  } else {
    # No wavelengths detected - use original legacy analysis
    cat("• No wavelength markers found - using legacy analysis\n")
    results <- analyze_growth_curves(file_path, layout_type)
  }
  
  cat("\n• === SMART ANALYSIS COMPLETE ===\n")
  
  return(results)
}

#' Adapted single wavelength analysis (uses multi functions but formats like original)
analyze_single_wavelength_adapted <- function(file_path, layout_type, wavelength_info) {
  
  wavelength <- wavelength_info$wavelength
  
  cat("• Processing", wavelength, "nm data using adapted pipeline...\n")
  
  # Use multi-wavelength reading but for single wavelength
  processed_data_multi <- process_synergy_file_multi(file_path, layout_type = layout_type)
  
  # Extract the single wavelength data
  wavelength_key <- paste0("wavelength_", wavelength)
  if (!wavelength_key %in% names(processed_data_multi$wavelength_data)) {
    stop("Expected wavelength ", wavelength, " not found in processed data")
  }
  
  processed_data <- processed_data_multi$wavelength_data[[wavelength_key]]
  
  # Apply corrections using multi functions but adapt column names for compatibility
  cat("• Applying corrections...\n")
  corrected_results_multi <- process_with_corrections_multi(processed_data, wavelength)
  
  # Adapt the corrected results to look like original single-wavelength format
  corrected_results <- adapt_multi_to_single_format(corrected_results_multi, wavelength)
  
  # Create visualizations using original functions
  cat("• Creating visualizations...\n")
  plots <- list(
    growth_curves = plot_growth_curves(corrected_results),
    initial_plate = plot_plate_heatmap(corrected_results, "first", "corrected"),
    final_plate = plot_plate_heatmap(corrected_results, "last", "corrected"),
    plate_composite = plot_plate_heatmap_composite(corrected_results),
    qc_plots = create_qc_plots(corrected_results, processed_data)
  )
  
  cat("• Single wavelength analysis complete for", wavelength, "nm\n")
  
  return(list(
    processed_data = processed_data,
    corrected_results = corrected_results,
    plots = plots,
    wavelength = wavelength
  ))
}

#' Adapt multi-wavelength format to single-wavelength format
#' Adapt multi-wavelength format to single-wavelength format (ROBUST VERSION)
adapt_multi_to_single_format <- function(corrected_results_multi, wavelength) {
  
  cat("  - Adapting multi-wavelength format to single-wavelength format\n")
  
  # Get the data
  data <- corrected_results_multi$corrected_data
  
  # Get column names for this wavelength
  od_raw_col <- paste0("od", wavelength, "_raw")
  od_broth_col <- paste0("od", wavelength, "_broth_corrected") 
  od_final_col <- paste0("od", wavelength, "_final")
  
  cat("  - Looking for columns:", od_raw_col, ",", od_broth_col, ",", od_final_col, "\n")
  
  # Create a mapping of new column names
  rename_mapping <- list()
  
  if (od_raw_col %in% names(data)) {
    rename_mapping[["od600_raw"]] <- od_raw_col
  }
  
  if (od_broth_col %in% names(data)) {
    rename_mapping[["od600_broth_corrected"]] <- od_broth_col
  }
  
  if (od_final_col %in% names(data)) {
    rename_mapping[["od600_final"]] <- od_final_col
  }
  
  cat("  - Renaming", length(rename_mapping), "columns\n")
  
  # Apply renames one by one to avoid conflicts
  for (new_name in names(rename_mapping)) {
    old_name <- rename_mapping[[new_name]]
    if (old_name %in% names(data) && old_name != new_name) {
      names(data)[names(data) == old_name] <- new_name
      cat("  - Renamed", old_name, "to", new_name, "\n")
    }
  }
  
  # Add od600 column as copy of od600_raw for compatibility (if it doesn't exist)
  if ("od600_raw" %in% names(data) && !"od600" %in% names(data)) {
    data <- data %>%
      mutate(od600 = od600_raw)
    cat("  - Added od600 column as copy of od600_raw\n")
  }
  
  # Final check
  od_columns <- names(data)[str_detect(names(data), "od600")]
  cat("  - Final OD columns:", paste(od_columns, collapse = ", "), "\n")
  
  # Adapt replicate stats (remove wavelength column)
  adapted_replicate_stats <- corrected_results_multi$replicate_stats
  if ("wavelength" %in% names(adapted_replicate_stats)) {
    adapted_replicate_stats <- adapted_replicate_stats %>%
      select(-wavelength)
  }
  
  return(list(
    raw_data = corrected_results_multi$raw_data,
    corrected_data = data,
    replicate_stats = adapted_replicate_stats,
    corrections = corrected_results_multi$corrections,
    wavelength = wavelength
  ))
}

#' Enhanced wrapper that provides both single and multi results for true multi-wavelength data
analyze_growth_curves_complete <- function(file_path, layout_type = "default") {
  
  cat("\n")
  cat("• === COMPLETE GROWTH CURVE ANALYSIS ===\n")
  
  # Use smart analysis
  results <- analyze_growth_curves_smart(file_path, layout_type)
  
  # If it's truly multi-wavelength data, also provide individual wavelength results
  if (is.list(results) && "wavelength_600" %in% names(results)) {
    
    cat("• Multi-wavelength data detected - creating individual wavelength summaries\n")
    
    # Create individual wavelength results that work with existing accessor functions
    individual_results <- list()
    
    for (wavelength_key in names(results)) {
      if (str_detect(wavelength_key, "^wavelength_\\d+$")) {
        wavelength <- str_extract(wavelength_key, "\\d+")
        cat("• Creating individual result for", wavelength, "nm\n")
        
        # Adapt each wavelength to single-wavelength format
        adapted_result <- list(
          processed_data = results[[wavelength_key]]$processed_data,
          corrected_results = adapt_multi_to_single_format(results[[wavelength_key]]$corrected_results, wavelength),
          plots = results[[wavelength_key]]$plots,
          wavelength = wavelength
        )
        
        individual_results[[wavelength_key]] <- adapted_result
      }
    }
    
    return(list(
      multi_wavelength_results = results,
      individual_wavelength_results = individual_results
    ))
    
  } else {
    # Single wavelength or legacy - return as is
    return(results)
  }
}

#' Convenience accessor for multi-wavelength data
get_wavelength_results <- function(complete_results, wavelength) {
  
  wavelength_key <- paste0("wavelength_", wavelength)
  
  # Check if it's complete results with individual wavelength results
  if ("individual_wavelength_results" %in% names(complete_results)) {
    if (wavelength_key %in% names(complete_results$individual_wavelength_results)) {
      return(complete_results$individual_wavelength_results[[wavelength_key]])
    }
  }
  
  # Check if it's direct multi-wavelength results
  if (wavelength_key %in% names(complete_results)) {
    return(complete_results[[wavelength_key]])
  }
  
  # Check if it's single wavelength results
  if ("wavelength" %in% names(complete_results) && complete_results$wavelength == wavelength) {
    return(complete_results)
  }
  
  stop("Wavelength ", wavelength, " not found in results")
}

#' Wrapper function for warnings control
run_analysis <- function(file_path, export_summaries = FALSE) {
  
  cat("🧬 Starting Growth Curve Analysis...\n")
  cat("🔇 Suppressing technical warnings for cleaner output\n\n")
  
  # Run the full analysis with controlled warnings
  results <- analyze_growth_curves_with_summaries(
    file_path = file_path,
    export_summaries = export_summaries,
    run_diagnostics = TRUE
  )
  
  cat("\n🎉 Analysis finished successfully!\n")
  cat("📊 View results with: show_growth_curves(results)\n")

  return(results)
}

# ===== Actual Data Processing ===== 

# Set working directory (adjust as needed)
setup_workspace("/home/william-ackerman/Desktop/Microbial_Growth_Curves_Analysis")

file <- "25-06-17_AgNP_cubes_DP_raw_plate_layout.xlsx" 
file <- "25-06-17_AgNP_cubes_DP_datacorrected.xlsx"

# Basic analysis
results$wavelength_600$plots
examine_raw_kinetic_data(file)
results <- analyze_growth_curves_multi(file)

results$wavelength_600$plots$final_plate
results$wavelength_420$plots$plate_composite

show_growth_curves <- function(results) {
  print(results$plots$growth_curves)
}

if(!is.null(results$wavelength_600)){
  
} else if (!is.null(results$wavelength_420)){
  
} else {
  print(results$plots$growth_curves)
  
}