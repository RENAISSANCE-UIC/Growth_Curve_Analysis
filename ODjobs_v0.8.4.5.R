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
  
  cat(" Reading Synergy HTX file:", basename(file_path), "\n")
  
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
  
  cat(" Successfully read", nrow(kinetic_long), "data points from", 
      length(unique(kinetic_long$well_id)), "wells\n")
  
  return(list(
    metadata = metadata,
    kinetic_data = kinetic_long,
    experiment_start = create_experiment_datetime(metadata)
  ))
}

#' Read Cerillo Excel file and extract kinetic data
read_cerillo_excel_file <- function(file_path) {
  
  cat(" Reading Cerillo Excel file:", basename(file_path), "\n")
  
  # Read entire Excel file
  all_data <- suppressMessages(suppressWarnings(
    read_excel(file_path, col_names = FALSE)
  ))
  
  # Convert all columns to character for consistent processing
  all_data <- all_data %>%
    mutate(across(everything(), as.character))
  
  # Extract metadata from header rows
  metadata <- extract_cerillo_metadata(all_data)
  
  # Find where the actual data starts (after the header section)
  data_start_row <- find_cerillo_data_start(all_data)
  
  # Read the data section with column names from the header row
  if (data_start_row > 1) {
    # Try to get column names from the row before data starts
    header_row <- data_start_row - 1
    col_names <- as.character(all_data[header_row, ])
    col_names[is.na(col_names)] <- paste0("Column_", seq_along(col_names))[is.na(col_names)]
    
    kinetic_data <- suppressMessages(suppressWarnings(
      read_excel(file_path, skip = data_start_row - 1, col_names = col_names)
    ))
  } else {
    # Read without header
    kinetic_data <- suppressMessages(suppressWarnings(
      read_excel(file_path, skip = data_start_row - 1)
    ))
  }
  
  cat("• Read", nrow(kinetic_data), "data rows with", ncol(kinetic_data), "columns\n")
  
  # Process into long format
  kinetic_long <- process_cerillo_kinetic_data(kinetic_data, metadata)
  
  cat(" Successfully read", nrow(kinetic_long), "data points from", 
      length(unique(kinetic_long$well_id)), "wells\n")
  
  return(list(
    metadata = metadata,
    kinetic_data = kinetic_long,
    experiment_start = create_cerillo_experiment_datetime(metadata)
  ))
}

#' Read Cerillo kinetic data section from Excel
read_cerillo_kinetic_data_excel <- function(file_path, data_start_row) {
  
  # Read from data start to end of file
  kinetic_data <- suppressMessages(suppressWarnings(
    read_excel(file_path, skip = data_start_row - 1)
  ))
  
  # Convert all columns to character for consistent processing
  kinetic_data <- kinetic_data %>%
    mutate(across(everything(), as.character))
  
  cat("• Read", nrow(kinetic_data), "data rows with", ncol(kinetic_data), "columns\n")
  
  return(kinetic_data)
}

#' Create experiment datetime - SUPPRESSED WARNINGS
create_experiment_datetime <- function(metadata) {
  experiment_date <- metadata["Date"]
  experiment_time <- metadata["Time"]
  
  if (!is.na(experiment_date) && !is.na(experiment_time)) {
    suppressWarnings({
      tryCatch({
        
        # CSV FORMAT: Handle "6/17/2025" + "3:56:30 PM"
        if (str_detect(experiment_date, "^\\d{1,2}/\\d{1,2}/\\d{4}$") && 
            str_detect(experiment_time, "AM|PM")) {
          
          datetime_str <- paste(experiment_date, experiment_time)
          parsed_datetime <- mdy_hms(datetime_str, quiet = TRUE)
          
          if (!is.na(parsed_datetime)) {
            cat(" CSV datetime parsed:", as.character(parsed_datetime), "\n")
            return(parsed_datetime)
          }
        }
        
        # CSV FORMAT: Handle "6/17/2025" + "15:56:30" (24-hour)
        if (str_detect(experiment_date, "^\\d{1,2}/\\d{1,2}/\\d{4}$") && 
            str_detect(experiment_time, "^\\d{1,2}:\\d{2}:\\d{2}$")) {
          
          datetime_str <- paste(experiment_date, experiment_time)
          parsed_datetime <- mdy_hms(datetime_str, quiet = TRUE)
          
          if (!is.na(parsed_datetime)) {
            cat(" CSV datetime parsed (24h):", as.character(parsed_datetime), "\n")
            return(parsed_datetime)
          }
        }
        
        # EXCEL FORMAT: Original method for Excel compatibility
        if (str_detect(experiment_date, "^\\d{4}-\\d{2}-\\d{2}$") || 
            !str_detect(experiment_date, "/")) {
          
          parsed_datetime <- ymd_hms(paste(experiment_date, experiment_time), quiet = TRUE)
          
          if (!is.na(parsed_datetime)) {
            cat(" Excel datetime parsed:", as.character(parsed_datetime), "\n")
            return(parsed_datetime)
          }
        }
        
        # FALLBACK: Try all common formats
        datetime_combinations <- c(
          paste(experiment_date, experiment_time),
          paste(mdy(experiment_date), experiment_time),
          paste(dmy(experiment_date), experiment_time)
        )
        
        for (combo in datetime_combinations) {
          for (parse_func in list(ymd_hms, mdy_hms, dmy_hms)) {
            result <- parse_func(combo, quiet = TRUE)
            if (!is.na(result)) {
              cat(" Fallback datetime parsed:", as.character(result), "\n")
              return(result)
            }
          }
        }
        
        # If all else fails
        stop("Could not parse datetime")
        
      }, error = function(e) {
        cat("️  Could not parse experiment datetime, using default\n")
        cat("   Date: '", experiment_date, "' Time: '", experiment_time, "'\n")
        cat("   Error:", e$message, "\n")
        ymd_hms("2025-01-01 00:00:00")
      })
    })
  } else {
    cat("ℹ️  Missing date/time info, using default experiment start time\n")
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
      # Excel numeric date
      suppressWarnings({
        tryCatch(
          as.character(as.Date(as.numeric(value), origin = "1899-12-30")),
          error = function(e) value
        )
      })
    },
    parameter == "Date" & str_detect(value, "^\\d{1,2}/\\d{1,2}/\\d{4}$") ~ {
      # CSV date format MM/DD/YYYY or DD/MM/YYYY
      value  # Keep as-is, could add parsing if needed
    },
    parameter == "Time" & str_detect(value, "^0\\.") ~ {
      # Excel decimal time
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
    parameter == "Time" & str_detect(value, "^\\d{1,2}:\\d{2}(:\\d{2})?$") ~ {
      # CSV time format HH:MM:SS or HH:MM
      value  # Keep as-is
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
        time_hrs_clean = as.numeric(time_hrs) * 24,           # Expected NAs here
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
        
        # Look for the header row (should contain "Time", "T", A1, A2, etc.)
        header_row <- NA
        data_start_row <- NA
        
        for (j in (i + 1):(i + 5)) {  # Check next few rows
          if (j <= nrow(all_data)) {
            # Check if this looks like a header row
            row_content <- all_data[j, ] %>% unlist() %>% na.omit() %>% as.character()
            
            # Look for Time, temperature, and well identifiers
            has_time <- any(str_detect(row_content, "Time"))
            has_temp <- any(str_detect(row_content, "T|temp"))
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

#' Process wavelength kinetic data with adaptive column handling
process_wavelength_kinetic_data <- function(kinetic_data, metadata, wavelength) {
  
  n_cols <- ncol(kinetic_data)
  cat("  - Processing", n_cols, "columns for", wavelength, "nm\n")
  
  # Check if first row is header
  first_row <- kinetic_data[1, ] %>% unlist() %>% na.omit() %>% as.character()
  has_header <- any(str_detect(first_row, "Time|T"))
  
  if (has_header) {
    kinetic_data <- kinetic_data[-1, ]
    cat("  - Removed header row\n")
    cat("  - Header was:", paste(head(first_row, 6), collapse = ", "), "\n")
  }
  
  # ADAPTIVE COLUMN DETECTION
  if (n_cols == 98) {
    cat("  - 98 columns detected - checking if Col1=Time, Col2=Temp\n")
    
    col1_sample <- suppressWarnings(as.numeric(kinetic_data[[1]][1:3]))
    col2_sample <- suppressWarnings(as.numeric(kinetic_data[[2]][1:3]))
    
    # Col1 should be time (0-24 hours), Col2 should be temp (30-50C)
    col1_is_time <- all(!is.na(col1_sample)) && all(col1_sample >= 0 & col1_sample <= 24)
    col2_is_temp <- all(!is.na(col2_sample)) && all(col2_sample >= 30 & col2_sample <= 50)
    
    if (col1_is_time && col2_is_temp) {
      cat("  - Standard format: Col1=Time, Col2=Temp - using as-is\n")
      use_data <- kinetic_data
      final_n_cols <- n_cols
    } else {
      cat("  - WARNING: 98 columns but Col1/Col2 don't look like Time/Temp\n")
      cat("  - Col1 sample:", paste(col1_sample, collapse = ", "), "\n")
      cat("  - Col2 sample:", paste(col2_sample, collapse = ", "), "\n")
      # Use as-is anyway
      use_data <- kinetic_data
      final_n_cols <- n_cols
    }
    
  } else if (n_cols == 99) {
    cat("  - 99 columns detected - checking if Col2=Time, Col3=Temp (Col1=extra)\n")
    
    col1_sample <- kinetic_data[[1]][1:3]
    col2_sample <- suppressWarnings(as.numeric(kinetic_data[[2]][1:3]))
    col3_sample <- suppressWarnings(as.numeric(kinetic_data[[3]][1:3]))
    
    cat("  - Col1 (potential extra):", paste(col1_sample, collapse = ", "), "\n")
    cat("  - Col2 (potential time):", paste(col2_sample, collapse = ", "), "\n")
    cat("  - Col3 (potential temp):", paste(col3_sample, collapse = ", "), "\n")
    
    # Col2 should be time (0-24 hours), Col3 should be temp (30-50C)
    col2_is_time <- all(!is.na(col2_sample)) && all(col2_sample >= 0 & col2_sample <= 24)
    col3_is_temp <- all(!is.na(col3_sample)) && all(col3_sample >= 30 & col3_sample <= 50)
    
    if (col2_is_time && col3_is_temp) {
      cat("  - Extra column format detected: dropping Col1, using Col2=Time, Col3=Temp\n")
      use_data <- kinetic_data[, -1]  # Drop first column
      final_n_cols <- 98
    } else {
      cat("  - WARNING: 99 columns but Col2/Col3 don't look like Time/Temp\n")
      cat("  - Assuming standard format and hoping for the best\n")
      use_data <- kinetic_data
      final_n_cols <- n_cols
    }
    
  } else {
    cat("  - Unexpected column count:", n_cols, "- using standard processing\n")
    use_data <- kinetic_data
    final_n_cols <- n_cols
  }
  
  # NOW PROCEED WITH STANDARD COLUMN ASSIGNMENT
  if (final_n_cols >= 98) {
    col_names <- c("time_hrs", "temperature", 
                   paste0(rep(LETTERS[1:8], each = 12), 1:12))
    col_names <- col_names[1:final_n_cols]
    well_cols <- col_names[3:98]  # A1-H12
    
  } else if (final_n_cols >= 14) {
    col_names <- c("time_hrs", "temperature", paste0("Col_", 3:final_n_cols))
    well_cols <- col_names[3:length(col_names)]
    
  } else {
    stop("Unexpected number of columns after processing: ", final_n_cols)
  }
  
  names(use_data) <- col_names
  
  cat("  - Final format: Time=", paste(use_data$time_hrs[1:2], collapse = ", "), "\n")
  cat("  - Final format: Temp=", paste(use_data$temperature[1:2], collapse = ", "), "\n")
  cat("  - Well columns identified:", length(well_cols), "\n")
  
  # Create experiment start datetime
  experiment_start <- create_experiment_datetime(metadata)
  
  # Process to long format (rest is unchanged)
  suppressWarnings({
    kinetic_clean <- use_data %>%
      mutate(
        time_hrs_clean = as.numeric(time_hrs) * 24,
        temperature_clean = as.numeric(temperature)
      ) %>%
      filter(!is.na(time_hrs_clean))
    
    # Verify final ranges make sense
    time_range <- range(kinetic_clean$time_hrs_clean, na.rm = TRUE)
    temp_range <- range(kinetic_clean$temperature_clean, na.rm = TRUE)
    cat("  - Final time range:", round(time_range[1], 3), "to", round(time_range[2], 3), "hours\n")
    cat("  - Final temp range:", round(temp_range[1], 1), "to", round(temp_range[2], 1), "C\n")
    
    kinetic_long <- kinetic_clean %>%
      mutate(datetime = experiment_start + seconds(time_hrs_clean * 3600)) %>%
      select(time_hrs_clean, temperature_clean, datetime, all_of(well_cols)) %>%
      pivot_longer(
        cols = all_of(well_cols),
        names_to = "well_id_raw",
        values_to = paste0("od", wavelength, "_raw")
      ) %>%
      mutate(
        !!paste0("od", wavelength, "_raw") := as.numeric(.data[[paste0("od", wavelength, "_raw")]]),
        !!paste0("od", wavelength) := .data[[paste0("od", wavelength, "_raw")]],
        well_id = case_when(
          str_detect(well_id_raw, "^[A-H]\\d{1}$") ~ str_replace(well_id_raw, "^([A-H])(\\d)$", "\\10\\2"),
          str_detect(well_id_raw, "^[A-H]\\d{2}$") ~ well_id_raw,
          TRUE ~ well_id_raw
        )
      ) %>%
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
  
  return(kinetic_long)
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
    
    cat("   Processed", nrow(kinetic_long), "data points for", wavelength, "nm\n")
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


# ===== EXPERIMENTAL LAYOUT FUNCTIONS =====

#' Create experimental layout with explicit replicate structure
#' Create experimental layout from Shiny app export or use default
#' Create experimental layout with optional custom CSV
create_experiment_layout <- 
  function(layout_type = "default", layout_csv = NULL, 
           orientation = "vertical") {
    
    if (!is.null(layout_csv)) {
      # FIXED: Check if layout_csv is a data frame (from launch_plate_editor) or a file path
      if (is.data.frame(layout_csv)) {
        cat(" Using experimental layout from plate editor\n")
        
        # Use the flexible layout processing function directly on the data frame
        layout <- process_flexible_layout_df(layout_csv)
        
      } else if (is.character(layout_csv) && file.exists(layout_csv)) {
        cat(" Reading experimental layout from:", basename(layout_csv), "\n")
        
        # Use the flexible layout processing function for file
        layout <- process_flexible_layout(layout_csv)
        
      } else {
        stop("layout_csv must be either a data frame or a valid file path")
      }
      
      # Create summary with safety checks
      layout_summary <- tryCatch({
        layout %>%
          filter(!is.na(concentration)) %>%
          # Ensure columns are character vectors
          mutate(
            concentration = as.character(concentration),
            sample_type = as.character(sample_type)
          ) %>%
          group_by(concentration, sample_type) %>%
          summarise(
            wells = paste(well_id, collapse = ", "),
            n_replicates = n(),
            .groups = "drop"
          )
      }, error = function(e) {
        cat("Warning: Could not create layout summary:", e$message, "\n")
        tibble(concentration = character(0), sample_type = character(0), 
               wells = character(0), n_replicates = integer(0))
      })
      
      cat(" Layout loaded:", nrow(layout), "wells configured\n")
      
    } else if (layout_type != "default") {
      stop("Only 'default' layout currently supported")
      
    } else {
      # Use default hardcoded layout with orientation support
      cat(" Using default experimental layout (", orientation, "orientation)\n")
      
      # ... rest of the default layout code stays the same ...
      if (orientation == "vertical") {
        # VERTICAL LAYOUT (your updated version with Conc_X labels)
        layout <- expand_grid(
          row = LETTERS[1:8],
          col = 1:12
        ) %>%
          mutate(
            well_id = paste0(row, sprintf("%02d", col)),
            row_letter = row,
            col_number = col,
            condition = case_when(
              col %in% 1:5 ~ "SAMPLE",
              col == 6 ~ "NP_only",
              col %in% 7:11 ~ "blank",
              col == 12 & row %in% c("A", "B", "C") ~ "broth_control",
              col == 12 & row %in% c("D", "E", "F", "G", "H") ~ "UNT",
              TRUE ~ "unknown"
            ),
            concentration = case_when(
              condition %in% c("SAMPLE", "NP_only") ~ {
                row_position <- match(row_letter, LETTERS[1:8])
                paste0("Conc_", row_position)
              },
              condition == "UNT" ~ {
                row_position <- match(row_letter, LETTERS[1:8])
                paste0("Conc_", row_position)
              },
              TRUE ~ NA_character_
            ),
            sample_type = case_when(
              condition == "SAMPLE" ~ "sample",
              condition == "NP_only" ~ "np_control",
              condition == "UNT" ~ "untreated_control",
              condition == "broth_control" ~ "broth",
              condition == "blank" ~ "blank",
              TRUE ~ "unknown"
            ),
            replicate_id = case_when(
              condition == "SAMPLE" ~ paste0("Rep_", col),
              condition == "NP_only" ~ "NP_Control",
              condition == "UNT" ~ paste0("UNT_", row_letter),
              condition == "broth_control" ~ paste0("Broth_", row_letter),
              condition == "blank" ~ paste0("Blank_", col),
              TRUE ~ NA_character_
            )
          ) %>%
          # ENSURE all columns are character vectors
          mutate(
            sample_type = as.character(sample_type),
            concentration = as.character(concentration),
            condition = as.character(condition),
            replicate_id = as.character(replicate_id)
          ) %>%
          select(well_id, condition, sample_type, concentration, replicate_id, 
                 row_letter, col_number)
        
      } else {
        # HORIZONTAL LAYOUT (your updated version with Conc_X labels)
        layout <- expand_grid(
          row = LETTERS[1:8],
          col = 1:12
        ) %>%
          mutate(
            well_id = paste0(row, sprintf("%02d", col)),
            row_letter = row,
            col_number = col,
            condition = case_when(
              col %in% c(1, 12) ~ "broth_control",
              row %in% c("A", "B", "C") & col %in% 2:11 ~ "SAMPLE",
              row == "D" & col %in% 2:11 ~ "NP_only",
              row %in% c("E", "F", "G") & col %in% 2:11 ~ "blank",
              row %in% c("E", "F", "G") & col %in% c(1, 12) ~ "broth_control",
              row == "H" & col %in% 2:11 ~ "UNT",
              row == "H" & col %in% c(1, 12) ~ "broth_control",
              TRUE ~ "unknown"
            ),
            concentration = case_when(
              condition %in% c("SAMPLE", "NP_only") ~ paste0("Conc_", col - 1),
              condition == "UNT" ~ paste0("Conc_", col - 1),
              condition == "blank" ~ paste0("Blank_", col - 1),
              TRUE ~ NA_character_
            ),
            sample_type = case_when(
              condition == "SAMPLE" ~ "sample",
              condition == "NP_only" ~ "np_control",
              condition == "UNT" ~ "untreated_control",
              condition == "broth_control" ~ "broth",
              condition == "blank" ~ "blank",
              TRUE ~ "unknown"
            ),
            replicate_id = case_when(
              row == "A" & col %in% 2:11 ~ "Rep_A",
              row == "B" & col %in% 2:11 ~ "Rep_B", 
              row == "C" & col %in% 2:11 ~ "Rep_C",
              row == "H" & col %in% 2:11 ~ "Rep_UNT",
              row == "D" & col %in% 2:11 ~ "NP_Control",
              row == "E" & col %in% 2:11 ~ "Blank_E",
              row == "F" & col %in% 2:11 ~ "Blank_F",
              row == "G" & col %in% 2:11 ~ "Blank_G",
              TRUE ~ NA_character_
            )
          ) %>%
          # ENSURE all columns are character vectors
          mutate(
            sample_type = as.character(sample_type),
            concentration = as.character(concentration),
            condition = as.character(condition),
            replicate_id = as.character(replicate_id)
          ) %>%
          select(well_id, condition, sample_type, concentration, replicate_id, 
                 row_letter, col_number)
      }
      
      # CREATE SUMMARY FOR DEFAULT LAYOUTS TOO (this was missing!)
      layout_summary <- tryCatch({
        layout %>%
          filter(!is.na(concentration)) %>%
          # Ensure columns are character vectors
          mutate(
            concentration = as.character(concentration),
            sample_type = as.character(sample_type)
          ) %>%
          group_by(concentration, sample_type) %>%
          summarise(
            wells = paste(well_id, collapse = ", "),
            n_replicates = n(),
            .groups = "drop"
          )
      }, error = function(e) {
        cat("Warning: Could not create layout summary:", e$message, "\n")
        tibble(concentration = character(0), sample_type = character(0), 
               wells = character(0), n_replicates = integer(0))
      })
      
      print_layout_summary()
    }
    
    return(list(
      layout = layout,
      summary = layout_summary  # Now this will always exist
    ))
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

#' Launch Plate Editor with Auto-Capture (FIXED with Blocking)
#'
#' This function launches the plate editor and automatically captures the layout data
#' when the app is closed using the onStop() hook.
#'
#' @param auto_capture Logical. If TRUE, automatically captures layout data when app closes.
#' @param return_data Logical. If TRUE, returns the captured data. If FALSE, returns file path.
#'
#' @return If return_data = TRUE, returns data frame with layout. 
#'         If return_data = FALSE, returns file path to saved CSV.
#'
#' @export
launch_plate_editor <- function(auto_capture = TRUE, return_data = TRUE) {
  
  if (!require(shiny, quietly = TRUE)) {
    stop("Please install shiny: install.packages('shiny')")
  }
  
  # Load required libraries
  if (!require(readr, quietly = TRUE)) {
    if (!require(utils, quietly = TRUE)) {
      stop("Could not load CSV writing functions")
    }
  }
  
  cat(" Launching plate editor...\n")
  
  # Create temporary files for communication
  result_file <- tempfile(fileext = ".rds")
  signal_file <- tempfile(fileext = ".signal")
  
  if (auto_capture) {
    cat(" Auto-capture mode: Layout will be saved when you close the app\n")
    cat("1. Design your plate layout\n")
    cat("2. Close the app (click X or use Stop & Save button)\n")
    cat("3. Layout data will be automatically captured\n\n")
    
    # Set up capture file path in working directory
    capture_file <- file.path(getwd(), paste0("plate_layout_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
    
    # Store paths in environment variables
    Sys.setenv(PLATE_CAPTURE_FILE = capture_file)
    Sys.setenv(PLATE_CAPTURE_ENABLED = "TRUE")
    Sys.setenv(PLATE_RESULT_FILE = result_file)
    Sys.setenv(PLATE_SIGNAL_FILE = signal_file)
    
    cat(" Will save to:", capture_file, "\n\n")
    
  } else {
    cat(" Manual mode: Use download button to save layout\n")
    cat("1. Design your plate layout\n")
    cat("2. Click 'Download CSV' to save\n\n")
    
    Sys.setenv(PLATE_CAPTURE_ENABLED = "FALSE")
    Sys.setenv(PLATE_RESULT_FILE = result_file)
    Sys.setenv(PLATE_SIGNAL_FILE = signal_file)
  }
  
  # Set flag to prevent auto-launch when sourcing
  SOURCED_FROM_FUNCTION <<- TRUE
  
  # Source the app code
  source("Shiny_96_wp_interface_stable.R", local = TRUE)
  
  # Create the app object
  app <- shinyApp(ui, server)
  
  cat(" App is starting... (close app window to continue)\n")
  
  # Use runApp with explicit blocking
  tryCatch({
    runApp(app, launch.browser = TRUE, host = "127.0.0.1", port = NULL)
  }, error = function(e) {
    cat(" Error running app:", e$message, "\n")
  }, interrupt = function(i) {
    cat(" App interrupted by user\n")
  })
  
  # This line should only execute AFTER the app closes
  cat(" App closed, checking for captured data...\n")
  
  # Wait for signal file to indicate data has been saved
  max_wait <- 10
  wait_interval <- 0.1
  waited <- 0
  
  while (waited < max_wait) {
    if (file.exists(signal_file)) {
      cat(" Received completion signal\n")
      
      # Small additional wait to ensure file operations are complete
      Sys.sleep(0.2)
      
      if (file.exists(result_file)) {
        tryCatch({
          result <- readRDS(result_file)
          
          # Clean up temp files
          unlink(result_file)
          unlink(signal_file)
          
          if (!is.null(result) && (is.data.frame(result) || is.list(result))) {
            if (is.data.frame(result) && nrow(result) > 0) {
              cat(" Layout data captured successfully!\n")
              cat(" Layout dimensions:", nrow(result), "rows x", ncol(result), "columns\n")
              if (return_data) {
                return(result)
              } else {
                return(Sys.getenv("PLATE_CAPTURE_FILE"))
              }
            } else {
              cat("️ No layout was created (empty data)\n")
              return(NULL)
            }
          } else {
            cat("️ No layout was created\n")
            return(NULL)
          }
        }, error = function(e) {
          cat(" Error reading result file:", e$message, "\n")
          unlink(result_file)
          unlink(signal_file)
          return(NULL)
        })
      } else {
        cat(" Signal received but no result file found\n")
        unlink(signal_file)
        return(NULL)
      }
    }
    
    Sys.sleep(wait_interval)
    waited <- waited + wait_interval
    
    # Show waiting message every 2 seconds
    if (waited %% 2 < wait_interval) {
      cat(" Waiting for data to be saved...\n")
    }
  }
  
  # Cleanup and timeout
  unlink(result_file)
  unlink(signal_file)
  cat(" Timeout - no data was captured\n")
  return(NULL)
}

#' Process flexible layout from file (FIXED)
#' Process flexible layout from file (COMPLETELY REWRITTEN)
process_flexible_layout <- function(layout_csv) {
  cat(" Processing flexible experimental layout from:", basename(layout_csv), "\n")
  
  # Read and clean the layout data
  layout_data <- read_csv(layout_csv, show_col_types = FALSE) %>%
    clean_names()
  
  cat("• Original layout columns:", paste(names(layout_data), collapse = ", "), "\n")
  
  # Create ordinal concentration mapping
  conc_mapping <- create_ordinal_concentrations(layout_data)
  
  # Detect experiment parameters from the data
  experiment_params <- detect_experiment_parameters(layout_data)
  
  # Convert to the expected format step by step
  layout <- layout_data %>%
    mutate(
      # STANDARDIZE WELL IDs
      well_id = case_when(
        str_detect(well, "^[A-H]\\d{1}$") ~ str_replace(well, "^([A-H])(\\d)$", "\\10\\2"),
        str_detect(well, "^[A-H]\\d{2}$") ~ well,
        TRUE ~ well
      ),
      # Ensure concentration_value is numeric for joining
      concentration_value = as.numeric(concentration_value)
    )
  
  cat("• Layout after well_id standardization:", nrow(layout), "rows\n")
  
  # JOIN WITH ORDINAL CONCENTRATION MAPPING - do this BEFORE other mutations
  layout <- layout %>%
    left_join(conc_mapping, by = "concentration_value")
  
  cat("• Layout after concentration mapping join:", nrow(layout), "rows\n")
  cat("• Sample of conc_label values:", paste(head(unique(layout$conc_label), 5), collapse = ", "), "\n")
  
  # Now continue with other mutations
  layout <- layout %>%
    mutate(
      condition = case_when(
        category == "Experiment" ~ "SAMPLE",
        category == "Material Control (NP Only)" ~ "NP_only", 
        category == "Positive Control (Untreated Bacteria)" ~ "UNT",  # UPDATED
        category == "Positive Control (Bacteria Only)" ~ "UNT",  # FALLBACK for old format
        category == "Broth Only" ~ "broth_control",
        category == "Half Broth" ~ "half_broth",  # NEW
        category == "Blank" ~ "blank",
        TRUE ~ paste0("unknown_", category)
      ),
      sample_type = case_when(
        condition == "SAMPLE" ~ "sample",
        condition == "NP_only" ~ "np_control", 
        condition == "UNT" ~ "untreated_control",
        condition == "broth_control" ~ "broth",
        condition == "half_broth" ~ "half_broth",  # NEW
        condition == "blank" ~ "blank",
        TRUE ~ paste0("unknown_", condition)
      ),
      row_letter = str_extract(well_id, "^[A-H]"),
      col_number = as.numeric(str_extract(well_id, "\\d+$")),
      # FIXED CONCENTRATION ASSIGNMENT - use the joined conc_label directly
      concentration = case_when(
        condition %in% c("SAMPLE", "NP_only", "UNT") & !is.na(conc_label) ~ conc_label,
        condition == "broth_control" ~ paste0("Broth_", col_number),
        condition == "half_broth" ~ paste0("HalfBroth_", col_number),  # NEW
        condition == "blank" ~ paste0("Blank_", col_number),
        TRUE ~ NA_character_
      )
    ) %>%
    # FINAL TYPE ENFORCEMENT - ensure everything is the right type
    mutate(
      sample_type = as.character(sample_type),
      concentration = as.character(concentration),
      condition = as.character(condition),
      well_id = as.character(well_id),
      row_letter = as.character(row_letter)
    ) %>%
    arrange(well_id)
  
  cat("• Layout after all mutations:", nrow(layout), "rows\n")
  cat("• Sample types found:", paste(unique(layout$sample_type), collapse = ", "), "\n")
  cat("• Sample of concentration values:", paste(head(unique(layout$concentration[!is.na(layout$concentration)]), 5), collapse = ", "), "\n")
  
  # Continue with replicate assignment...
  layout <- assign_replicate_ids_flexibly(layout, experiment_params)
  
  cat("• Layout after replicate assignment:", nrow(layout), "rows\n")
  
  # FINAL SAFETY CHECK - ensure no list columns exist
  list_columns <- sapply(layout, function(x) any(sapply(x, is.list)))
  if (any(list_columns)) {
    cat("• WARNING: List columns detected:", paste(names(list_columns)[list_columns], collapse = ", "), "\n")
    # Convert any list columns to character
    for (col_name in names(list_columns)[list_columns]) {
      layout[[col_name]] <- as.character(layout[[col_name]])
    }
  }
  
  # Validation and reporting
  validate_flexible_layout(layout, experiment_params)
  
  return(layout)
}

#' Process flexible layout from data frame
process_flexible_layout_df <- function(layout_data) {
  cat(" Processing flexible experimental layout from data frame\n")
  
  # Clean the layout data
  layout_data <- layout_data %>%
    clean_names()
  
  # Create ordinal concentration mapping
  conc_mapping <- create_ordinal_concentrations(layout_data)
  
  # Detect experiment parameters from the data
  experiment_params <- detect_experiment_parameters(layout_data)
  
  # Convert to the expected format with ordinal concentration assignments
  layout <- layout_data %>%
    mutate(
      # STANDARDIZE WELL IDs
      well_id = case_when(
        str_detect(well, "^[A-H]\\d{1}$") ~ str_replace(well, "^([A-H])(\\d)$", "\\10\\2"),
        str_detect(well, "^[A-H]\\d{2}$") ~ well,
        TRUE ~ well
      ),
      condition = case_when(
        category == "Experiment" ~ "SAMPLE",
        category == "Material Control (NP Only)" ~ "NP_only", 
        category == "Positive Control (Untreated Bacteria)" ~ "UNT",  # UPDATED
        category == "Positive Control (Bacteria Only)" ~ "UNT",  # FALLBACK for old format
        category == "Broth Only" ~ "broth_control",
        category == "Half Broth" ~ "half_broth",  # NEW
        category == "Blank" ~ "blank",
        TRUE ~ paste0("unknown_", category)
      ),
      sample_type = case_when(
        condition == "SAMPLE" ~ "sample",
        condition == "NP_only" ~ "np_control", 
        condition == "UNT" ~ "untreated_control",
        condition == "broth_control" ~ "broth",
        condition == "half_broth" ~ "half_broth",  # NEW
        condition == "blank" ~ "blank",
        TRUE ~ paste0("unknown_", condition)
      ),
      row_letter = str_extract(well_id, "^[A-H]"),
      col_number = as.numeric(str_extract(well_id, "\\d+$"))
    ) %>%
    # JOIN WITH ORDINAL CONCENTRATION MAPPING
    left_join(conc_mapping, by = "concentration_value") %>%
    mutate(
      # ENSURE concentration is always a character, not a list
      concentration = case_when(
        condition %in% c("SAMPLE", "NP_only", "UNT") & !is.na(conc_label) ~ as.character(conc_label),
        condition == "broth_control" ~ as.character(paste0("Broth_", col_number)),
        condition == "half_broth" ~ as.character(paste0("HalfBroth_", col_number)),  # NEW
        condition == "blank" ~ as.character(paste0("Blank_", col_number)),
        TRUE ~ NA_character_
      )
    ) %>%
    # ENSURE existing columns are the right type (but don't reference replicate_id yet)
    mutate(
      sample_type = as.character(sample_type),
      concentration = as.character(concentration),
      condition = as.character(condition),
      well_id = as.character(well_id),
      row_letter = as.character(row_letter)
    ) %>%
    arrange(well_id)
  
  cat("• Layout after all mutations:", nrow(layout), "rows\n")
  cat("• Sample types found:", paste(unique(layout$sample_type), collapse = ", "), "\n")
  cat("• Sample of concentration values:", paste(head(unique(layout$concentration[!is.na(layout$concentration)]), 5), collapse = ", "), "\n")
  
  # Continue with replicate assignment...
  layout <- assign_replicate_ids_flexibly(layout, experiment_params)
  
  cat("• Layout after replicate assignment:", nrow(layout), "rows\n")
  
  # NOW ensure replicate_id is character (after it's been created)
  layout <- layout %>%
    mutate(replicate_id = as.character(replicate_id))
  
  # FINAL SAFETY CHECK - ensure no list columns exist
  list_columns <- sapply(layout, function(x) any(sapply(x, is.list)))
  if (any(list_columns)) {
    cat("• WARNING: List columns detected:", paste(names(list_columns)[list_columns], collapse = ", "), "\n")
    # Convert any list columns to character
    for (col_name in names(list_columns)[list_columns]) {
      layout[[col_name]] <- as.character(layout[[col_name]])
    }
  }
  
  # Validation and reporting
  validate_flexible_layout(layout, experiment_params)
  
  return(layout)
}

detect_experiment_parameters <- function(layout_data) {
  
  # Detect number of dilutions
  experiment_wells <- layout_data %>% 
    filter(category == "Experiment", !is.na(concentration_value))
  
  n_dilutions <- length(unique(experiment_wells$concentration_value))
  
  # NEW: Detect orientation by checking concentration distribution
  conc_by_row <- experiment_wells %>% 
    group_by(row) %>% 
    summarise(unique_concs = n_distinct(concentration_value), .groups = "drop")
  
  conc_by_col <- experiment_wells %>% 
    group_by(column) %>% 
    summarise(unique_concs = n_distinct(concentration_value), .groups = "drop")
  
  # If each row has multiple concentrations, it's horizontal
  # If each column has multiple concentrations, it's vertical
  max_concs_per_row <- max(conc_by_row$unique_concs, na.rm = TRUE)
  max_concs_per_col <- max(conc_by_col$unique_concs, na.rm = TRUE)
  
  if (max_concs_per_row > 1 && max_concs_per_row >= max_concs_per_col) {
    orientation <- "horizontal"
    start_col <- min(experiment_wells$column, na.rm = TRUE)
    start_row <- NA
  } else if (max_concs_per_col > 1) {
    orientation <- "vertical"
    start_row <- min(match(experiment_wells$row, LETTERS), na.rm = TRUE)
    start_col <- NA
  } else {
    # Fallback
    orientation <- "horizontal"
    start_col <- min(experiment_wells$column, na.rm = TRUE)
    start_row <- NA
  }
  
  # Detect number of replicates  
  n_replicates <- length(unique(experiment_wells$replicate_number))
  
  # Detect concentration range
  conc_values <- sort(unique(experiment_wells$concentration_value), decreasing = TRUE)
  highest_conc <- max(conc_values, na.rm = TRUE)
  
  params <- list(
    orientation = orientation,          # NEW
    n_dilutions = n_dilutions,
    start_column = start_col,
    start_row = start_row,             # NEW
    n_replicates = n_replicates,
    highest_concentration = highest_conc,
    concentration_values = conc_values
  )
  
  cat(" Detected experiment parameters:\n")
  cat("   - Orientation:", orientation, "\n")        # NEW
  cat("   - Dilutions:", n_dilutions, "\n")
  cat("   - Start column:", start_col, "\n")
  cat("   - Start row:", start_row, "\n")            # NEW
  cat("   - Replicates:", n_replicates, "\n")
  cat("   - Highest concentration:", highest_conc, "\n")
  
  return(params)
}

assign_concentrations_flexibly <- function(layout, experiment_params) {
  
  layout <- layout %>%
    group_by(condition) %>%
    mutate(
      concentration = case_when(
        # For samples and NP controls: use ordinal ranking
        condition %in% c("SAMPLE", "NP_only") ~ {
          if (!is.null(experiment_params$orientation) && experiment_params$orientation == "vertical") {
            # Vertical: concentrations by row position
            relative_row <- match(row_letter, LETTERS) - experiment_params$start_row + 1
            paste0("Conc_", relative_row)
          } else {
            # Horizontal: concentrations by column position  
            relative_col <- col_number - experiment_params$start_column + 1
            paste0("Conc_", relative_col)
          }
        },
        
        # For untreated controls: assign by position order
        condition == "UNT" ~ {
          # Create ordinal sequence based on well position
          well_order <- order(interaction(row_letter, col_number))
          conc_num <- match(row_number(), well_order)
          paste0("Conc_", conc_num)
        },
        
        # Other conditions unchanged
        condition == "broth_control" ~ paste0("Broth_", col_number),
        condition == "blank" ~ paste0("Blank_", col_number),
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup()
  
  return(layout)
}

assign_replicate_ids_flexibly <- function(layout, experiment_params) {
  
  layout <- layout %>%
    mutate(
      replicate_id = case_when(
        # For samples: use replicate labels from CSV if available
        condition == "SAMPLE" & !is.na(replicate_label) ~ 
          str_replace_all(replicate_label, " ", "_"),
        
        # For untreated controls: use replicate labels or generate
        condition == "UNT" & !is.na(replicate_label) ~ 
          str_replace_all(replicate_label, " ", "_"),
        condition == "UNT" & is.na(replicate_label) ~ 
          paste0("UNT_", row_letter),
        
        # For NP controls: row-based naming
        condition == "NP_only" ~ paste0("NP_", row_letter),
        
        # For broth controls: row-based naming
        condition == "broth_control" ~ paste0("Broth_", row_letter),
        
        # For blanks: row-based naming
        condition == "blank" ~ paste0("Blank_", row_letter),
        
        TRUE ~ NA_character_
      )
    )
  
  return(layout)
}

#' Create ordinal concentration labels based on actual values
create_ordinal_concentrations <- function(layout_data) {
  
  # Get unique concentration values for samples/NP controls
  # NOTE: The CSV uses 'category' not 'condition'
  sample_concentrations <- layout_data %>%
    filter(category %in% c("Experiment", "Material Control (NP Only)"), 
           !is.na(concentration_value)) %>%
    distinct(concentration_value) %>%
    arrange(desc(concentration_value)) %>%  # Highest to lowest
    mutate(conc_rank = row_number()) %>%
    select(concentration_value, conc_rank)
  
  # Create mapping - ENSURE both columns are proper types
  conc_mapping <- sample_concentrations %>%
    mutate(
      concentration_value = as.numeric(concentration_value),  # Ensure numeric
      conc_label = as.character(paste0("Conc_", conc_rank))   # Ensure character
    ) %>%
    select(concentration_value, conc_label)
  
  cat("• Concentration mapping created:\n")
  print(conc_mapping)
  
  return(conc_mapping)
}

#' Validate flexible layout (FIXED)
validate_flexible_layout <- function(layout, experiment_params) {
  
  cat(" Layout validation:\n")
  
  # Debug: Check the structure of the layout
  cat("• Layout structure check:\n")
  cat("  - sample_type class:", class(layout$sample_type), "\n")
  cat("  - concentration class:", class(layout$concentration), "\n")
  cat("  - unique sample_types:", paste(unique(layout$sample_type), collapse = ", "), "\n")
  
  # Check if concentration column has any list elements
  if (any(sapply(layout$concentration, is.list))) {
    cat("  ️  Warning: concentration column contains list elements, converting...\n")
    layout <- layout %>%
      mutate(concentration = as.character(concentration))
  }
  
  # Safely check concentration assignments
  tryCatch({
    conc_check <- layout %>%
      filter(!is.na(concentration)) %>%
      # Ensure both columns are character vectors
      mutate(
        sample_type = as.character(sample_type),
        concentration = as.character(concentration)
      ) %>%
      arrange(sample_type, concentration) %>%
      count(sample_type, concentration, name = "n_wells") %>%
      arrange(sample_type, concentration)
    
    cat("• Concentration assignments:\n")
    print(conc_check)
    
  }, error = function(e) {
    cat("• Error in concentration assignments:", e$message, "\n")
    cat("• Sample of concentration values:\n")
    print(head(layout$concentration, 10))
    cat("• Sample of sample_type values:\n")
    print(head(layout$sample_type, 10))
  })
  
  # Check if Half Broth controls are present
  half_broth_present <- "half_broth" %in% unique(layout$sample_type)
  if (half_broth_present) {
    cat("\n• NOTE: Half Broth controls detected\n")
    cat("  This will trigger the new Half Broth correction method\n")
  }
  
  # Check if NP controls are present
  np_present <- "np_control" %in% unique(layout$sample_type)
  if (!np_present) {
    cat("\n• NOTE: No NP controls detected in layout\n")
    cat("  This is acceptable for experiments without nanoparticle treatments\n")
  }
  
  # Validate untreated controls specifically
  cat("\n• Untreated control assignments:\n")
  tryCatch({
    unt_check <- layout %>%
      filter(sample_type == "untreated_control") %>%
      select(well_id, concentration, replicate_id) %>%
      arrange(concentration)
    print(unt_check)
  }, error = function(e) {
    cat("  Error displaying untreated controls:", e$message, "\n")
  })
  
  # Check for expected number of dilutions
  sample_concs <- layout %>%
    filter(sample_type == "sample") %>%
    distinct(concentration) %>%
    nrow()
  
  if (sample_concs != experiment_params$n_dilutions) {
    cat("️  Warning: Expected", experiment_params$n_dilutions, 
        "dilutions but found", sample_concs, "\n")
  }
  
  return(invisible(TRUE))
}

#' Apply custom layout to analysis results
#' Apply custom layout to analysis results
process_custom_layout <- function(layout_csv) {
  
  # FIXED: Check if layout_csv is a data frame or file path
  if (is.data.frame(layout_csv)) {
    cat(" Using custom experimental layout from data frame\n")
    
    # Use the flexible layout processing function directly on the data frame
    layout <- process_flexible_layout_df(layout_csv)
    
  } else if (is.character(layout_csv) && file.exists(layout_csv)) {
    cat(" Reading custom experimental layout from:", basename(layout_csv), "\n")
    
    # Use the flexible layout processing function for file
    layout <- process_flexible_layout(layout_csv)
    
  } else {
    stop("layout_csv must be either a data frame or a valid file path")
  }
  
  cat(" Custom layout applied:", nrow(layout), "wells configured\n")
  
  return(layout)
}

# ===== DATA PROCESSING FUNCTIONS =====

#' Main data processing function
process_synergy_file <- 
  function(file_path, sheet_name = NULL, layout_type = "default", 
           layout_csv = NULL, orientation = "vertical") {
  
  # Detect plate reader type
  reader_type <- detect_plate_reader_type(file_path)
  
  if (reader_type == "cerillo") {
    cat(" Processing Cerillo data\n")
    return(process_cerillo_file(file_path, layout_type, layout_csv))
  } else {
    cat(" Processing Synergy HTX data\n")
    
    # Original Synergy HTX processing
    file_ext <- tools::file_ext(tolower(file_path))
    
    if (file_ext == "csv") {
      synergy_data <- read_synergy_htx_csv_file(file_path)
    } else if (file_ext %in% c("xlsx", "xls")) {
      synergy_data <- read_synergy_htx_file(file_path, sheet_name)
    } else {
      stop("Unsupported file format. Please use .xlsx, .xls, or .csv files.")
    }
    
    # Create layout - use custom if provided, otherwise default
    if (!is.null(layout_csv)) {
      layout <- process_custom_layout(layout_csv)
      
      # Create summary
      layout_summary <- layout %>%
        filter(!is.na(concentration)) %>%
        group_by(concentration, sample_type) %>%
        summarise(
          wells = paste(well_id, collapse = ", "),
          n_replicates = n(),
          .groups = "drop"
        )
      
      layout_info <- list(
        layout = layout,
        summary = layout_summary
      )
      
      cat(" Custom layout loaded:", nrow(layout), "wells configured\n")
      
    } else {
      # Use default layout with orientation parameter
      layout_info <- create_experiment_layout(layout_type, NULL, orientation)  # ADDED orientation
    }
    
    # Merge data with layout
    complete_data <- synergy_data$kinetic_data %>%
      left_join(layout_info$layout, by = "well_id") %>%
      mutate(
        row_letter = ifelse(is.na(row_letter), str_extract(well_id, "^[A-H]"), row_letter),
        col_number = ifelse(is.na(col_number), as.numeric(str_extract(well_id, "\\d+$")), col_number)
      )
    
    # Verify the merge worked
    missing_layout <- complete_data %>% 
      filter(is.na(sample_type)) %>% 
      distinct(well_id) %>% 
      nrow()
    
    if (missing_layout > 0) {
      cat("• ️ Warning:", missing_layout, "wells missing layout information\n")
    }
    
    # Create summary
    data_summary <- create_data_summary(synergy_data, complete_data, layout_info)
    
    return(list(
      metadata = synergy_data$metadata,
      data = complete_data,
      layout = layout_info$layout,
      summary = data_summary
    ))
  }
}

#' Process multi-wavelength data with layout
process_synergy_file_multi <- 
  function(file_path, layout_type = "default", 
           layout_csv = NULL, orientation = "vertical") {
  
  # Detect plate reader type
  reader_type <- detect_plate_reader_type(file_path)
  
  if (reader_type == "cerillo") {
    cat(" Processing Cerillo data (single wavelength)\n")
    
    # Process Cerillo data and wrap it in multi-wavelength format
    cerillo_data <- process_cerillo_file(file_path, layout_type, layout_csv)
    
    # Wrap in multi-wavelength structure (Cerillo is single wavelength at 600nm)
    wavelength_data <- list(
      "wavelength_600" = list(
        metadata = cerillo_data$metadata,
        data = cerillo_data$data,
        layout = cerillo_data$layout,
        summary = cerillo_data$summary,
        wavelength = "600"
      )
    )
    
    return(list(
      wavelength_data = wavelength_data,
      metadata = cerillo_data$metadata,
      layout = cerillo_data$layout,
      wavelength_sections = list("wavelength_600" = list(wavelength = "600"))
    ))
    
  } else {
    cat(" Processing Synergy HTX data (multi-wavelength)\n")
    
    # Original Synergy HTX processing
    file_ext <- tools::file_ext(tolower(file_path))
    
    if (file_ext == "csv") {
      synergy_data <- read_synergy_htx_csv_file_multi(file_path)
    } else if (file_ext %in% c("xlsx", "xls")) {
      synergy_data <- read_synergy_htx_file_multi(file_path)
    } else {
      stop("Unsupported file format. Please use .xlsx, .xls, or .csv files.")
    }
    
    # Process each wavelength
    processed_wavelengths <- list()
    
    for (wavelength_name in names(synergy_data$wavelength_data)) {
      
      wavelength_kinetic <- synergy_data$wavelength_data[[wavelength_name]]
      wavelength <- unique(wavelength_kinetic$wavelength)[1]
      
      cat("• Processing layout for", wavelength, "nm...\n")
      
      # Create layout - use custom if provided, otherwise default
      if (!is.null(layout_csv)) {
        layout <- process_custom_layout(layout_csv)
        
        # Create summary
        layout_summary <- layout %>%
          filter(!is.na(concentration)) %>%
          group_by(concentration, sample_type) %>%
          summarise(
            wells = paste(well_id, collapse = ", "),
            n_replicates = n(),
            .groups = "drop"
          )
        
        layout_info <- list(
          layout = layout,
          summary = layout_summary
        )
        
        cat(" Custom layout loaded:", nrow(layout), "wells configured\n")
        
      } else {
        # Use default layout with orientation
        layout_info <- create_experiment_layout(layout_type, NULL, orientation)  # ADDED orientation
      }
      
      # Merge data with layout
      complete_data <- wavelength_kinetic %>%
        left_join(layout_info$layout, by = "well_id") %>%
        mutate(
          row_letter = ifelse(is.na(row_letter), str_extract(well_id, "^[A-H]"), row_letter),
          col_number = ifelse(is.na(col_number), as.numeric(str_extract(well_id, "\\d+$")), col_number)
        )
      
      # Verify the merge worked
      missing_layout <- complete_data %>% 
        filter(is.na(sample_type)) %>% 
        distinct(well_id) %>% 
        nrow()
      
      if (missing_layout > 0) {
        cat("• ️ Warning:", missing_layout, "wells missing layout information\n")
      }
      
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
      layout = processed_wavelengths[[1]]$layout,  # Use first wavelength's layout
      wavelength_sections = synergy_data$wavelength_sections
    ))
  }
}

#' Create data summary statistics
create_data_summary <- function(synergy_data, complete_data, layout_info) {
  
  # Ensure complete_data columns are proper types
  complete_data <- complete_data %>%
    mutate(
      sample_type = as.character(sample_type),
      well_id = as.character(well_id)
    )
  
  # Safe sample distribution calculation
  sample_distribution <- tryCatch({
    complete_data %>% 
      distinct(well_id, sample_type) %>% 
      mutate(sample_type = as.character(sample_type)) %>%  # Ensure it's character
      count(sample_type, name = "n_wells")
  }, error = function(e) {
    cat("Warning: Could not create sample distribution, using fallback\n")
    tibble(sample_type = character(0), n_wells = integer(0))
  })
  
  list(
    experiment_start = synergy_data$experiment_start,
    experiment_date = synergy_data$metadata["Date"],
    experiment_time = synergy_data$metadata["Time"],
    n_timepoints = length(unique(complete_data$time_point)),
    time_range_hrs = range(complete_data$time_hrs, na.rm = TRUE),
    datetime_range = range(complete_data$datetime, na.rm = TRUE),
    n_wells = length(unique(complete_data$well_id)),
    temp_range = range(complete_data$temperature, na.rm = TRUE),
    sample_distribution = sample_distribution,
    layout_summary = layout_info$summary
  )
}

# ===== DATA CORRECTION FUNCTIONS =====

#' Detect the main OD column name
detect_od_column <- function(data) {
  possible_columns <- c("od600_raw", "od600", "od_raw", "od")
  
  for (col in possible_columns) {
    if (col %in% names(data)) {
      cat("• Using OD column:", col, "\n")
      return(col)
    }
  }
  
  # Look for any column with "od" in the name
  od_columns <- names(data)[str_detect(names(data), "od")]
  if (length(od_columns) > 0) {
    cat("• Using OD column:", od_columns[1], "\n")
    return(od_columns[1])
  }
  
  stop("Could not find OD column in data")
}

#' Apply broth and nanoparticle corrections with method options (single wavelength)
#' Updated process_with_corrections to handle standard method
#' Apply broth and nanoparticle corrections with method options (single wavelength)
process_with_corrections <- function(processed_data, method = "standard", threshold = TRUE) {
  
  cat(" Applying corrections using", toupper(method), "method\n")
  if (!threshold) cat("️  Thresholding disabled - negative values allowed\n")
  
  data <- processed_data$data
  
  # CLEAN INVALID WELLS FIRST
  original_rows <- nrow(data)
  data <- clean_well_data(data)
  removed_rows <- original_rows - nrow(data)
  
  if (removed_rows > 0) {
    cat("• Removed", removed_rows, "rows with invalid well identifiers\n")
  }
  
  # Detect the OD column name (should work for both Cerillo and Synergy)
  od_column <- detect_od_column(data)
  
  # Check if Half Broth wells are present
  has_halfbroth <- "half_broth" %in% unique(data$sample_type)
  
  if (has_halfbroth && method == "standard") {
    cat("   Half Broth wells detected - switching to Half Broth correction method\n")
    method <- "halfbroth"
  }
  
  # Check if NP controls are present
  has_np_controls <- "np_control" %in% unique(data$sample_type)
  
  if (!has_np_controls) {
    cat("   No NP controls detected - NP correction will be skipped\n")
  }
  
  # Rest of the function continues as before...
  if (method == "standard") {
    # Use the standard correction (safe from contamination)
    data_fully_corrected <- apply_standard_correction_unified(data, od_column)
    
    # Create dummy correction objects for compatibility
    broth_correction <- data_fully_corrected$broth_correction_applied[1]
    np_corrections <- data_fully_corrected %>%
      filter(sample_type %in% c("sample", "untreated_control"), np_correction_applied > 0) %>%
      distinct(time_hrs, time_point, concentration, np_correction_applied) %>%
      rename(np_correction = np_correction_applied)
    
  } else if (method == "halfbroth") {
    # Use the new Half Broth correction method
    data_fully_corrected <- apply_halfbroth_correction_unified(data, od_column)
    
    # Create dummy correction objects for compatibility
    broth_correction <- data_fully_corrected$broth_correction_applied[1]
    np_corrections <- data_fully_corrected %>%
      filter(sample_type == "sample", np_correction_applied > 0) %>%
      distinct(time_hrs, time_point, concentration, np_correction_applied) %>%
      rename(np_correction = np_correction_applied)
    
  } else {
    # Use existing traditional/robust methods with unified column handling
    broth_correction <- calculate_broth_correction_unified(data, method = method, od_column = od_column)
    data_broth_corrected <- apply_broth_correction_unified(data, broth_correction, threshold = threshold, od_column = od_column)
    
    np_corrections <- calculate_np_corrections_unified(data_broth_corrected, method = method, od_column = od_column)
    data_fully_corrected <- apply_np_corrections_unified(data_broth_corrected, np_corrections, threshold = threshold, od_column = od_column)
  }
  
  # Calculate replicate statistics
  replicate_stats <- calculate_replicate_statistics_unified(data_fully_corrected, od_column)
  
  cat(" Corrections applied successfully\n")
  if (method %in% c("traditional", "robust")) {
    cat("    Broth correction:", round(broth_correction, 4), "OD units\n")
    if (has_np_controls) {
      cat("    NP corrections calculated for", 
          length(unique(np_corrections$concentration)), "concentrations\n")
    } else {
      cat("    No NP corrections applied (no NP controls)\n")
    }
  }
  
  return(list(
    raw_data = data,
    corrected_data = data_fully_corrected,
    replicate_stats = replicate_stats,
    corrections = list(
      broth_correction = broth_correction,
      np_corrections = np_corrections,
      method = method,
      threshold = threshold
    )
  ))
}

#' Apply corrections for a specific wavelength with method options
#' Updated process_with_corrections_multi
process_with_corrections_multi <- 
  function(wavelength_processed, wavelength, 
           method = "traditional", threshold = TRUE) {
    
    data <- wavelength_processed$data
    
    # CLEAN THE DATA FIRST
    data <- clean_corrected_data(data)
    
    # Check if Half Broth wells are present
    has_halfbroth <- "half_broth" %in% unique(data$sample_type)
    
    if (has_halfbroth && method == "standard") {
      cat("   Half Broth wells detected - switching to Half Broth correction method\n")
      method <- "halfbroth"
    }
    
    cat(" Applying", toupper(method), "corrections for", wavelength, "nm data\n")
    if (!threshold) cat("️  Thresholding disabled - negative values allowed\n")
    
    cat("  - Available columns:", paste(names(data)[str_detect(names(data), paste0("od", wavelength))], collapse = ", "), "\n")
    
    # Determine which OD columns exist
    od_base_column <- paste0("od", wavelength)
    od_raw_column <- paste0("od", wavelength, "_raw")
    od_broth_corrected_column <- paste0("od", wavelength, "_broth_corrected")
    od_final_column <- paste0("od", wavelength, "_final")
    
    # Find the main OD data column and ensure it's numeric
    if (od_raw_column %in% names(data)) {
      main_od_column <- od_raw_column
      data <- data %>%
        mutate(!!main_od_column := as.numeric(.data[[main_od_column]]))
    } else if (od_base_column %in% names(data)) {
      main_od_column <- od_base_column
      data <- data %>%
        mutate(!!od_base_column := as.numeric(.data[[od_base_column]])) %>%
        rename(!!od_raw_column := !!od_base_column)
      main_od_column <- od_raw_column
    } else {
      stop("Could not find OD data column for wavelength ", wavelength)
    }
    
    cat("  - Using", main_od_column, "as source data\n")
    
    # Filter out any non-numeric or NA values
    data <- data %>%
      filter(!is.na(.data[[main_od_column]]), 
             is.finite(.data[[main_od_column]]))
    
    cat("  - Data points after cleaning:", nrow(data), "\n")
    
    if (method == "standard") {
      # Use the new standard correction for multi-wavelength
      data_fully_corrected <- apply_standard_correction_multi(data, wavelength)
      
      # Create dummy correction objects for compatibility
      broth_correction <- data_fully_corrected$broth_correction_applied[1]
      np_corrections <- data_fully_corrected %>%
        filter(sample_type %in% c("sample", "untreated_control"), np_correction_applied > 0) %>%
        distinct(time_hrs, time_point, concentration, np_correction_applied) %>%
        rename(np_correction = np_correction_applied)
      
    } else if (method == "halfbroth") {
      # Use the new Half Broth correction method for multi-wavelength
      data_fully_corrected <- apply_halfbroth_correction_multi(data, wavelength)
      
      # Create dummy correction objects for compatibility
      broth_correction <- data_fully_corrected$broth_correction_applied[1]
      np_corrections <- data_fully_corrected %>%
        filter(sample_type == "sample", np_correction_applied > 0) %>%
        distinct(time_hrs, time_point, concentration, np_correction_applied) %>%
        rename(np_correction = np_correction_applied)
      
    } else {
      # Use existing traditional/robust methods
      broth_correction <- calculate_broth_correction_multi(data, method = method, wavelength = wavelength)
      
      if (is.na(broth_correction) || !is.finite(broth_correction)) {
        cat("  - Warning: Could not calculate broth correction, using 0\n")
        broth_correction <- 0
      }
      
      data_broth_corrected <- data %>%
        mutate(
          !!od_broth_corrected_column := if(threshold) {
            pmax(.data[[main_od_column]] - broth_correction, 0)
          } else {
            .data[[main_od_column]] - broth_correction
          }
        )
      
      np_corrections <- calculate_np_corrections_multi(data_broth_corrected, method = method, wavelength = wavelength)
      
      data_fully_corrected <- data_broth_corrected %>%
        left_join(np_corrections, by = c("time_hrs", "time_point", "concentration")) %>%
        mutate(
          np_correction = ifelse(is.na(np_correction), 0, np_correction),
          !!od_final_column := case_when(
            sample_type %in% c("sample", "untreated_control") ~ {
              if(threshold) {
                pmax(.data[[od_broth_corrected_column]] - np_correction, 0)
              } else {
                .data[[od_broth_corrected_column]] - np_correction
              }
            },
            TRUE ~ .data[[od_broth_corrected_column]]
          )
        )
    }
    
    # CLEAN THE FINAL DATA
    data_fully_corrected <- clean_corrected_data(data_fully_corrected)
    
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
    
    cat("   Corrections applied for", wavelength, "nm\n")
    if (method %in% c("traditional", "robust")) {
      cat("     Broth correction:", round(broth_correction, 4), "OD units\n")
      cat("     NP corrections calculated for", 
          length(unique(np_corrections$concentration)), "concentrations\n")
    }
    
    return(list(
      raw_data = data,
      corrected_data = data_fully_corrected,
      replicate_stats = replicate_stats,
      corrections = list(
        broth_correction = broth_correction,
        np_corrections = np_corrections,
        method = method,
        threshold = threshold
      ),
      wavelength = wavelength
    ))
  }

#' Calculate broth correction with method options
# Update method validation
#' Calculate broth correction with method options
calculate_broth_correction <- function(data, method = "traditional") {
  
  if (method == "traditional") {
    # Use any broth control, not just column 1
    broth_data <- data %>%
      filter(sample_type == "broth")
    if (nrow(broth_data) > 0) {
      # Use the first broth well found, initial timepoint
      broth_correction <- broth_data %>%
        filter(time_point == min(time_point)) %>%
        slice(1) %>%  # Take first broth well
        pull(od600)
      cat("  Traditional broth correction: Single well (initial) =",
          round(broth_correction, 4), "\n")
    } else {
      cat("  WARNING: No broth control found, using 0\n")
      broth_correction <- 0
    }
  } else if (method == "robust") {
    # Enhanced robust method: Detect and exclude outlier wells
    broth_data <- data %>% filter(sample_type == "broth")
    if (nrow(broth_data) == 0) {
      cat("  WARNING: No broth controls found, using 0\n")
      broth_correction <- 0
    } else {
      # Calculate per-well statistics
      well_stats <- broth_data %>%
        group_by(well_id) %>%
        summarise(
          mean_od = mean(od600, na.rm = TRUE),
          drift = max(od600, na.rm = TRUE) - min(od600, na.rm = TRUE),
          .groups = "drop"
        )
      # Detect outliers using both mean OD and drift
      median_od <- median(well_stats$mean_od, na.rm = TRUE)
      mad_od <- mad(well_stats$mean_od, na.rm = TRUE)
      median_drift <- median(well_stats$drift, na.rm = TRUE)
      mad_drift <- mad(well_stats$drift, na.rm = TRUE)
      # Wells are outliers if they have high mean OD OR high drift
      outlier_wells <- well_stats %>%
        filter(
          abs(mean_od - median_od) > 3 * mad_od |
            drift > median_drift + 3 * mad_drift
        ) %>%
        pull(well_id)
      if (length(outlier_wells) > 0) {
        cat("  WARNING: Outlier broth wells detected:", paste(outlier_wells, collapse = ", "), "\n")
      }
      # Calculate correction using only good wells
      good_wells_data <- broth_data %>%
        filter(!well_id %in% outlier_wells)
      if (nrow(good_wells_data) > 0) {
        broth_correction <- good_wells_data %>%
          summarise(broth_od = median(od600, na.rm = TRUE)) %>%
          pull(broth_od)
        n_good_wells <- length(unique(good_wells_data$well_id))
        cat("  Robust broth correction: Median of", n_good_wells, "good wells =",
            round(broth_correction, 4), "\n")
      } else {
        cat("  WARNING: All broth wells flagged as outliers, using 0\n")
        broth_correction <- 0
      }
    }
  } else if (method %in% c("standard")) {
    # For standard methods, correction is applied differently in apply_standard_correction()
    cat("  Standard broth correction: Applied in main correction function\n")
    broth_correction <- 0
  } else {
    stop("Method must be 'traditional', 'robust', or 'standard'")
  }
  return(broth_correction)
}

#' Calculate broth correction for multi-wavelength data (updated for standard method)
calculate_broth_correction_multi <- function(data, method = "standard", wavelength) {
  
  # Determine the main OD column
  main_od_column <- paste0("od", wavelength, "_raw")
  if (!main_od_column %in% names(data)) {
    main_od_column <- paste0("od", wavelength)
  }
  if (method == "traditional") {
    # Use any broth control, not just column 1
    broth_data <- data %>%
      filter(sample_type == "broth")
    if (nrow(broth_data) > 0) {
      broth_correction <- broth_data %>%
        filter(time_point == min(time_point)) %>%
        slice(1) %>%  # Take first broth well
        pull(!!main_od_column)
      cat("  Traditional broth correction (", wavelength, "nm): Single well (initial) =",
          round(broth_correction, 4), "\n")
    } else {
      cat("  WARNING: No broth control found, using 0\n")
      broth_correction <- 0
    }
  } else if (method == "robust") {
    # Enhanced robust method: Detect and exclude outlier wells
    broth_data <- data %>% filter(sample_type == "broth")
    if (nrow(broth_data) == 0) {
      cat("  WARNING: No broth controls found (", wavelength, "nm), using 0\n")
      broth_correction <- 0
    } else {
      # Calculate per-well statistics
      well_stats <- broth_data %>%
        group_by(well_id) %>%
        summarise(
          mean_od = mean(.data[[main_od_column]], na.rm = TRUE),
          drift = max(.data[[main_od_column]], na.rm = TRUE) - min(.data[[main_od_column]], na.rm = TRUE),
          .groups = "drop"
        )
      # Detect outliers using both mean OD and drift
      median_od <- median(well_stats$mean_od, na.rm = TRUE)
      mad_od <- mad(well_stats$mean_od, na.rm = TRUE)
      median_drift <- median(well_stats$drift, na.rm = TRUE)
      mad_drift <- mad(well_stats$drift, na.rm = TRUE)
      # Wells are outliers if they have high mean OD OR high drift
      outlier_wells <- well_stats %>%
        filter(
          abs(mean_od - median_od) > 3 * mad_od |
            drift > median_drift + 3 * mad_drift
        ) %>%
        pull(well_id)
      if (length(outlier_wells) > 0) {
        cat("  WARNING: Outlier broth wells detected (", wavelength, "nm):", paste(outlier_wells, collapse = ", "), "\n")
      }
      # Calculate correction using only good wells
      good_wells_data <- broth_data %>%
        filter(!well_id %in% outlier_wells)
      if (nrow(good_wells_data) > 0) {
        broth_correction <- good_wells_data %>%
          summarise(broth_od = median(.data[[main_od_column]], na.rm = TRUE)) %>%
          pull(broth_od)
        n_good_wells <- length(unique(good_wells_data$well_id))
        cat("  Robust broth correction (", wavelength, "nm): Median of", n_good_wells, "good wells =",
            round(broth_correction, 4), "\n")
      } else {
        cat("  WARNING: All broth wells flagged as outliers (", wavelength, "nm), using 0\n")
        broth_correction <- 0
      }
    }
  } else if (method %in% c("standard")) {
    # For standard methods, correction is applied differently
    cat("  :bar_chart:", str_to_title(method), "broth correction (", wavelength, "nm): Applied in main correction function\n")
    broth_correction <- 0
  } else {
    stop("Method must be 'traditional', 'robust', or 'standard'")
  }
  
  return(broth_correction)
}

#' Apply corrections with thresholding option
apply_broth_correction <- function(data, broth_correction, threshold = TRUE) {
  data %>%
    mutate(
      od600_raw = od600,
      od600_broth_corrected = if(threshold) {
        pmax(od600 - broth_correction, 0)
      } else {
        od600 - broth_correction
      }
    )
}

#' Calculate nanoparticle corrections  with method options  
calculate_np_corrections <- function(data_broth_corrected, method = "standard") {
  
  if (method == "traditional") {
    # Use existing concentration assignments from layout, not column-based calculations
    cat("   Traditional NP correction: Same-timepoint pairing with RAW NP values\n")
    np_corrections <- data_broth_corrected %>%
      filter(sample_type == "np_control") %>%
      select(time_hrs, time_point, od600_raw, well_id, concentration) %>%  # Use existing concentration
      select(time_hrs, time_point, concentration, np_correction = od600_raw)
  } else if (method == "robust") {
    # Use existing concentration assignments from layout
    cat("  :microscope: Robust NP correction: Averaged by concentration and time with RAW NP values\n")
    np_corrections <- data_broth_corrected %>%
      filter(sample_type == "np_control") %>%
      select(time_hrs, time_point, concentration, od600_raw) %>%  # Use existing concentration
      group_by(time_hrs, time_point, concentration) %>%
      summarise(np_correction = mean(od600_raw, na.rm = TRUE), .groups = "drop")
  } else if (method %in% c("standard")) {
    # For standard methods, NP correction is applied differently in apply_standard_correction()
    cat("  :microscope: Standard NP correction: Applied in main correction function\n")
    np_corrections <- tibble(
      time_hrs = numeric(0),
      time_point = numeric(0),
      concentration = character(0),
      np_correction = numeric(0)
    )
  } else {
    stop("Method must be 'traditional', 'robust', or 'standard'")
  }
  return(np_corrections)
}

#' Calculate NP corrections for multi-wavelength data (updated for standard method)
calculate_np_corrections_multi <- 
  function(data_broth_corrected, method = "traditional", wavelength) {
  
    # Use RAW OD column instead of broth-corrected
    od_raw_column <- paste0("od", wavelength, "_raw")
    if (method == "traditional") {
      cat("   Traditional NP correction (", wavelength, "nm): Same-timepoint pairing with RAW NP values\n")
      np_corrections <- data_broth_corrected %>%
        filter(sample_type == "np_control") %>%
        select(time_hrs, time_point, !!od_raw_column, well_id, concentration) %>%  # Use existing concentration
        select(time_hrs, time_point, concentration, np_correction = !!od_raw_column)
    } else if (method == "robust") {
      cat("  :microscope: Robust NP correction (", wavelength, "nm): Averaged by concentration and time with RAW NP values\n")
      np_corrections <- data_broth_corrected %>%
        filter(sample_type == "np_control") %>%
        select(time_hrs, time_point, concentration, !!od_raw_column) %>%  # Use existing concentration
        group_by(time_hrs, time_point, concentration) %>%
        summarise(np_correction = mean(.data[[od_raw_column]], na.rm = TRUE), .groups = "drop")
    } else if (method %in% c("standard")) {
      # For standard methods, NP correction is applied differently
      cat("  :microscope:", str_to_title(method), "NP correction (", wavelength, "nm): Applied in main correction function\n")
      np_corrections <- tibble(
        time_hrs = numeric(0),
        time_point = numeric(0),
        concentration = character(0),
        np_correction = numeric(0)
      )
    } else {
      stop("Method must be 'traditional', 'robust', or 'standard'")
    }
    
    return(np_corrections)
}

#' Apply NP corrections with thresholding option
apply_np_corrections <- function(data_broth_corrected, np_corrections, threshold = TRUE) {
  data_broth_corrected %>%
    left_join(np_corrections, by = c("time_hrs", "time_point", "concentration")) %>%
    mutate(
      np_correction = ifelse(is.na(np_correction), 0, np_correction),
      od600_final = case_when(
        sample_type %in% c("sample", "untreated_control") ~ {
          if(threshold) {
            pmax(od600_broth_corrected - np_correction, 0)
          } else {
            od600_broth_corrected - np_correction
          }
        },
        TRUE ~ od600_broth_corrected
      )
    )
}

#' Apply corrections with thresholding option
apply_corrections <- function(data, broth_correction, np_corrections, 
                              threshold = TRUE, method = "traditional") {
  
  # Apply broth correction
  data_broth_corrected <- data %>%
    mutate(
      od600_raw = od600,
      od600_broth_corrected = if(threshold) {
        pmax(od600 - broth_correction, 0)
      } else {
        od600 - broth_correction
      }
    )
  
  # Apply NP correction
  data_final <- data_broth_corrected %>%
    left_join(np_corrections, by = c("time_hrs", "time_point", "concentration")) %>%
    mutate(
      np_correction = ifelse(is.na(np_correction), 0, np_correction),
      od600_final = case_when(
        sample_type %in% c("sample", "untreated_control") ~ {
          if(threshold) {
            pmax(od600_broth_corrected - np_correction, 0)
          } else {
            od600_broth_corrected - np_correction
          }
        },
        TRUE ~ od600_broth_corrected
      )
    )
  
  return(data_final)
}

#' Apply standard correction method (statistician's recommendation, contamination-safe)
apply_standard_correction <- function(data, wavelength = NULL) {
  
  if (is.null(wavelength)) {
    # Single wavelength case
    od_raw_column <- "od600_raw"
    od_broth_corrected_column <- "od600_broth_corrected"
    od_final_column <- "od600_final"
  } else {
    # Multi-wavelength case
    od_raw_column <- paste0("od", wavelength, "_raw")
    od_broth_corrected_column <- paste0("od", wavelength, "_broth_corrected")
    od_final_column <- paste0("od", wavelength, "_final")
  }
  
  cat("   Applying STANDARD correction (contamination-safe):\n")
  cat("     Step 1: Detect contaminated broth controls\n")
  cat("     Step 2: Apply robust broth correction\n") 
  cat("     Step 3: Apply time-matched NP correction\n")
  
  # Step 1: Detect contaminated broth controls using robust statistics
  broth_wells_analysis <- data %>%
    filter(sample_type == "broth") %>%
    group_by(well_id) %>%
    summarise(
      mean_od = mean(.data[[od_raw_column]], na.rm = TRUE),
      max_od = max(.data[[od_raw_column]], na.rm = TRUE),
      min_od = min(.data[[od_raw_column]], na.rm = TRUE),
      od_range = max_od - min_od,
      growth_trend = ifelse(n() > 1, 
                            cor(seq_along(.data[[od_raw_column]]), .data[[od_raw_column]], use = "complete.obs"),
                            0),
      .groups = "drop"
    )
  
  # Identify potential contaminants using multiple criteria
  median_mean_od <- median(broth_wells_analysis$mean_od, na.rm = TRUE)
  mad_mean_od <- mad(broth_wells_analysis$mean_od, na.rm = TRUE)
  median_range <- median(broth_wells_analysis$od_range, na.rm = TRUE)
  mad_range <- mad(broth_wells_analysis$od_range, na.rm = TRUE)
  
  # Wells are suspicious if they have:
  # 1. Mean OD significantly higher than other broth wells, OR
  # 2. Large OD range (indicating growth), OR
  # 3. Positive growth trend
  contaminated_wells <- broth_wells_analysis %>%
    filter(
      (mean_od > median_mean_od + 3 * mad_mean_od) |  # High mean OD
        (od_range > median_range + 3 * mad_range) |      # High variability 
        (growth_trend > 0.3)                            # Positive correlation with time
    ) %>%
    pull(well_id)
  
  if (length(contaminated_wells) > 0) {
    cat("     ️  Contaminated broth wells detected:", paste(contaminated_wells, collapse = ", "), "\n")
    
    # Show contamination stats
    contamination_stats <- broth_wells_analysis %>% 
      filter(well_id %in% contaminated_wells) %>%
      select(well_id, mean_od, od_range, growth_trend)
    
    for (i in 1:nrow(contamination_stats)) {
      cat("        ", contamination_stats$well_id[i], 
          ": mean=", round(contamination_stats$mean_od[i], 4),
          ", range=", round(contamination_stats$od_range[i], 4),
          ", trend=", round(contamination_stats$growth_trend[i], 3), "\n")
    }
  } else {
    cat("      No contaminated broth wells detected\n")
  }
  
  # Step 2: Calculate robust broth correction using only clean wells
  clean_broth_data <- data %>%
    filter(sample_type == "broth", !well_id %in% contaminated_wells)
  
  if (nrow(clean_broth_data) == 0) {
    cat("     ️  All broth wells appear contaminated! Using minimal correction.\n")
    broth_correction_value <- 0
  } else {
    # Use median of clean wells at initial timepoint for stability
    broth_correction_value <- clean_broth_data %>%
      filter(time_point == min(time_point)) %>%
      summarise(correction = median(.data[[od_raw_column]], na.rm = TRUE)) %>%
      pull(correction)
    
    cat("      Robust broth correction using", length(unique(clean_broth_data$well_id)), 
        "clean wells:", round(broth_correction_value, 4), "\n")
  }
  
  # Step 3: Calculate time-matched NP corrections by CONCENTRATION (not column)
  np_corrections_by_time <- data %>%
    filter(sample_type == "np_control") %>%
    group_by(concentration, time_hrs, time_point) %>%  # FIXED: Use concentration instead of col_number
    summarise(
      np_correction = mean(.data[[od_raw_column]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Apply the STANDARD correction
  corrected_data <- data %>%
    # Apply uniform broth correction
    mutate(
      !!od_broth_corrected_column := pmax(.data[[od_raw_column]] - broth_correction_value, 0)
    ) %>%
    # Join NP corrections by CONCENTRATION and time (not col_number)
    left_join(np_corrections_by_time, by = c("concentration", "time_hrs", "time_point")) %>%
    # Fill missing NP corrections with 0
    mutate(
      np_correction = ifelse(is.na(np_correction), 0, np_correction)
    ) %>%
    # Apply final correction
    mutate(
      !!od_final_column := case_when(
        sample_type %in% c("sample", "untreated_control") ~ {
          # For samples: subtract broth + subtract time-matched NP
          pmax(.data[[od_broth_corrected_column]] - np_correction + broth_correction_value, 0)
        },
        sample_type == "np_control" ~ {
          # For NP controls: just subtract broth
          .data[[od_broth_corrected_column]]
        },
        sample_type %in% c("broth", "blank") ~ {
          # For broth and blank wells: subtract the broth correction to zero them out
          pmax(.data[[od_raw_column]] - broth_correction_value, 0)
        },
        TRUE ~ .data[[od_raw_column]]  # For any other unknown types
      ),
      
      # Store correction components for diagnostics
      broth_correction_applied = broth_correction_value,
      np_correction_applied = ifelse(sample_type %in% c("sample", "untreated_control"), 
                                     np_correction, 0)
    )
  
  # Report correction statistics
  correction_stats <- corrected_data %>%
    filter(sample_type %in% c("sample", "untreated_control")) %>%
    summarise(
      mean_np_corr = mean(np_correction_applied, na.rm = TRUE),
      max_np_corr = max(np_correction_applied, na.rm = TRUE),
      negative_values = sum(.data[[od_final_column]] <= 0, na.rm = TRUE),
      total_samples = n(),
      .groups = "drop"
    )
  
  cat("      Final correction statistics:\n")
  cat("        Broth correction (uniform):", round(broth_correction_value, 4), "\n")
  cat("        NP correction range: 0 to", round(correction_stats$max_np_corr, 4), "\n")
  cat("        Average NP correction:", round(correction_stats$mean_np_corr, 4), "\n")
  cat("        Negative values after correction:", correction_stats$negative_values, 
      "out of", correction_stats$total_samples, "\n")
  
  return(corrected_data)
}

#' Apply standard correction for multi-wavelength data
apply_standard_correction_multi <- function(data, wavelength) {
  
  od_raw_column <- paste0("od", wavelength, "_raw")
  od_broth_corrected_column <- paste0("od", wavelength, "_broth_corrected")
  od_final_column <- paste0("od", wavelength, "_final")
  
  cat("   Applying STANDARD correction for", wavelength, "nm (contamination-safe):\n")
  cat("     Step 1: Detect contaminated broth controls\n")
  cat("     Step 2: Apply robust broth correction\n") 
  cat("     Step 3: Apply time-matched NP correction (if NP controls present)\n")
  
  # Step 1: Detect contaminated broth controls
  broth_wells_analysis <- data %>%
    filter(sample_type == "broth") %>%
    group_by(well_id) %>%
    summarise(
      mean_od = mean(.data[[od_raw_column]], na.rm = TRUE),
      max_od = max(.data[[od_raw_column]], na.rm = TRUE),
      min_od = min(.data[[od_raw_column]], na.rm = TRUE),
      od_range = max_od - min_od,
      growth_trend = ifelse(n() > 1, 
                            cor(seq_along(.data[[od_raw_column]]), .data[[od_raw_column]], use = "complete.obs"),
                            0),
      .groups = "drop"
    )
  
  # Identify potential contaminants
  median_mean_od <- median(broth_wells_analysis$mean_od, na.rm = TRUE)
  mad_mean_od <- mad(broth_wells_analysis$mean_od, na.rm = TRUE)
  median_range <- median(broth_wells_analysis$od_range, na.rm = TRUE)
  mad_range <- mad(broth_wells_analysis$od_range, na.rm = TRUE)
  
  contaminated_wells <- broth_wells_analysis %>%
    filter(
      (mean_od > median_mean_od + 3 * mad_mean_od) |
        (od_range > median_range + 3 * mad_range) |
        (growth_trend > 0.3)
    ) %>%
    pull(well_id)
  
  if (length(contaminated_wells) > 0) {
    cat("     ️  Contaminated broth wells detected (", wavelength, "nm):", 
        paste(contaminated_wells, collapse = ", "), "\n")
  } else {
    cat("      No contaminated broth wells detected (", wavelength, "nm)\n")
  }
  
  # Step 2: Calculate robust broth correction
  clean_broth_data <- data %>%
    filter(sample_type == "broth", !well_id %in% contaminated_wells)
  
  if (nrow(clean_broth_data) == 0) {
    cat("     ️  All broth wells appear contaminated! Using minimal correction.\n")
    broth_correction_value <- 0
  } else {
    broth_correction_value <- clean_broth_data %>%
      filter(time_point == min(time_point)) %>%
      summarise(correction = median(.data[[od_raw_column]], na.rm = TRUE)) %>%
      pull(correction)
    
    cat("      Robust broth correction (", wavelength, "nm) using", 
        length(unique(clean_broth_data$well_id)), "clean wells:", 
        round(broth_correction_value, 4), "\n")
  }
  
  # FIRST, apply broth correction to create the broth_corrected column
  data <- data %>%
    mutate(
      !!od_broth_corrected_column := pmax(.data[[od_raw_column]] - broth_correction_value, 0)
    )
  
  # Step 3: Check if NP controls exist
  np_control_count <- sum(data$sample_type == "np_control", na.rm = TRUE)
  
  if (np_control_count == 0) {
    cat("      No NP controls present - applying broth correction only\n")
    
    # Apply only broth correction (no NP correction)
    corrected_data <- data %>%
      mutate(
        !!od_final_column := .data[[od_broth_corrected_column]],
        broth_correction_applied = broth_correction_value,
        np_correction_applied = 0,
        np_correction = 0  # Add this for consistency
      )
    
  } else {
    cat("      NP controls detected - calculating time-matched corrections\n")
    
    # Calculate time-matched NP corrections by CONCENTRATION
    np_corrections_by_time <- data %>%
      filter(sample_type == "np_control") %>%
      group_by(concentration, time_hrs, time_point) %>%
      summarise(
        np_correction = mean(.data[[od_raw_column]], na.rm = TRUE),
        .groups = "drop"
      )
    
    # Apply corrections
    corrected_data <- data %>%
      left_join(np_corrections_by_time, by = c("concentration", "time_hrs", "time_point")) %>%
      mutate(
        np_correction = ifelse(is.na(np_correction), 0, np_correction),
        !!od_final_column := case_when(
          sample_type %in% c("sample", "untreated_control") ~ {
            pmax(.data[[od_broth_corrected_column]] - np_correction + broth_correction_value, 0)
          },
          sample_type == "np_control" ~ {
            .data[[od_broth_corrected_column]]
          },
          sample_type %in% c("broth", "blank") ~ {
            .data[[od_broth_corrected_column]]
          },
          TRUE ~ .data[[od_raw_column]]
        ),
        broth_correction_applied = broth_correction_value,
        np_correction_applied = ifelse(sample_type %in% c("sample", "untreated_control"), 
                                       np_correction, 0)
      )
  }
  
  # Report statistics
  correction_stats <- corrected_data %>%
    filter(sample_type %in% c("sample", "untreated_control")) %>%
    summarise(
      mean_np_corr = mean(np_correction_applied, na.rm = TRUE),
      max_np_corr = max(np_correction_applied, na.rm = TRUE),
      negative_values = sum(.data[[od_final_column]] <= 0, na.rm = TRUE),
      total_samples = n(),
      .groups = "drop"
    )
  
  cat("      Final correction statistics (", wavelength, "nm):\n")
  cat("        Broth correction (uniform):", round(broth_correction_value, 4), "\n")
  if (np_control_count > 0) {
    cat("        NP correction range: 0 to", round(correction_stats$max_np_corr, 4), "\n")
    cat("        Average NP correction:", round(correction_stats$mean_np_corr, 4), "\n")
  } else {
    cat("        No NP corrections applied\n")
  }
  cat("        Negative values:", correction_stats$negative_values, 
      "out of", correction_stats$total_samples, "\n")
  
  return(corrected_data)
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

#' Apply custom layout to analysis results
apply_custom_layout_to_result <- function(result, layout_csv) {
  
  if (!file.exists(layout_csv)) {
    stop("Layout CSV file not found: ", layout_csv)
  }
  
  cat("   Reading custom layout from:", basename(layout_csv), "\n")
  
  # Read the layout file
  layout_data <- read_csv(layout_csv, show_col_types = FALSE) %>%
    clean_names()
  
  # Convert to the expected format
  custom_layout <- layout_data %>%
    mutate(
      well_id = well,
      condition = case_when(
        category == "Experiment" ~ "SAMPLE",
        category == "Material Control (NP Only)" ~ "NP_only", 
        category == "Positive Control (Bacteria Only)" ~ "UNT",
        category == "Broth Only" ~ "broth_control",
        category == "Blank" ~ "blank",
        TRUE ~ "unknown"
      ),
      sample_type = case_when(
        condition == "SAMPLE" ~ "sample",
        condition == "NP_only" ~ "np_control", 
        condition == "UNT" ~ "untreated_control",
        condition == "broth_control" ~ "broth",
        condition == "blank" ~ "blank",
        TRUE ~ "unknown"
      ),
      concentration = ifelse(!is.na(concentration_value), 
                             paste0("Conc_", concentration_value), 
                             NA_character_),
      replicate_id = ifelse(!is.na(replicate_label), 
                            str_replace_all(replicate_label, " ", "_"),
                            NA_character_),
      row_letter = str_extract(well_id, "^[A-H]"),
      col_number = as.numeric(str_extract(well_id, "\\d+$"))
    ) %>%
    select(well_id, condition, sample_type, concentration, replicate_id, 
           row_letter, col_number) %>%
    # Add any missing wells as unknown
    complete(well_id = paste0(rep(LETTERS[1:8], each = 12), 
                              sprintf("%02d", rep(1:12, 8)))) %>%
    mutate(
      condition = ifelse(is.na(condition), "unknown", condition),
      sample_type = ifelse(is.na(sample_type), "unknown", sample_type),
      row_letter = ifelse(is.na(row_letter), str_extract(well_id, "^[A-H]"), row_letter),
      col_number = ifelse(is.na(col_number), as.numeric(str_extract(well_id, "\\d+$")), col_number)
    )
  
  cat("   Custom layout applied:", nrow(custom_layout), "wells configured\n")
  
  # Update the layout in the result
  result$processed_data$layout <- custom_layout
  
  # Re-merge the data with the new layout
  result$processed_data$data <- result$processed_data$data %>%
    select(-any_of(c("condition", "sample_type", "concentration", "replicate_id"))) %>%
    left_join(custom_layout, by = "well_id")
  
  return(result)
}

#' Apply time cutoff to data
apply_time_cutoff <- function(data, max_time_hrs = NULL) {
  if (is.null(max_time_hrs)) {
    return(data)
  }
  
  original_points <- nrow(data)
  data_filtered <- data %>%
    filter(time_hrs <= max_time_hrs)
  
  removed_points <- original_points - nrow(data_filtered)
  if (removed_points > 0) {
    cat("• Applied time cutoff at", max_time_hrs, "hours\n")
    cat("• Removed", removed_points, "data points after cutoff\n")
  }
  
  return(data_filtered)
}


# ===== UNIFIED CORRECTION FUNCTIONS =====

#' Apply standard correction method with unified column handling
apply_standard_correction_unified <- function(data, od_column) {
  
  cat("   Applying STANDARD correction (contamination-safe):\n")
  cat("     Step 1: Detect contaminated broth controls\n")
  cat("     Step 2: Apply robust broth correction\n") 
  cat("     Step 3: Apply time-matched NP correction\n")
  cat("     Using OD column:", od_column, "\n")
  
  # Create column names based on the detected OD column
  if (od_column == "od600_raw") {
    od_broth_corrected_column <- "od600_broth_corrected"
    od_final_column <- "od600_final"
  } else if (od_column == "od_raw") {
    od_broth_corrected_column <- "od_broth_corrected"
    od_final_column <- "od_final"
  } else if (od_column == "od600") {
    od_broth_corrected_column <- "od600_broth_corrected"
    od_final_column <- "od600_final"
  } else {
    od_broth_corrected_column <- paste0(od_column, "_broth_corrected")
    od_final_column <- paste0(od_column, "_final")
  }
  
  # Step 1: Detect contaminated broth controls using robust statistics
  broth_wells_analysis <- data %>%
    filter(sample_type == "broth") %>%
    group_by(well_id) %>%
    summarise(
      mean_od = mean(.data[[od_column]], na.rm = TRUE),
      max_od = max(.data[[od_column]], na.rm = TRUE),
      min_od = min(.data[[od_column]], na.rm = TRUE),
      od_range = max_od - min_od,
      growth_trend = ifelse(n() > 1, 
                            cor(seq_along(.data[[od_column]]), .data[[od_column]], use = "complete.obs"),
                            0),
      .groups = "drop"
    )
  
  # Identify potential contaminants using multiple criteria
  median_mean_od <- median(broth_wells_analysis$mean_od, na.rm = TRUE)
  mad_mean_od <- mad(broth_wells_analysis$mean_od, na.rm = TRUE)
  median_range <- median(broth_wells_analysis$od_range, na.rm = TRUE)
  mad_range <- mad(broth_wells_analysis$od_range, na.rm = TRUE)
  
  # Wells are suspicious if they have:
  # 1. Mean OD significantly higher than other broth wells, OR
  # 2. Large OD range (indicating growth), OR
  # 3. Positive growth trend
  contaminated_wells <- broth_wells_analysis %>%
    filter(
      (mean_od > median_mean_od + 3 * mad_mean_od) |  # High mean OD
        (od_range > median_range + 3 * mad_range) |      # High variability 
        (growth_trend > 0.3)                            # Positive correlation with time
    ) %>%
    pull(well_id)
  
  if (length(contaminated_wells) > 0) {
    cat("     ️  Contaminated broth wells detected:", paste(contaminated_wells, collapse = ", "), "\n")
  } else {
    cat("      No contaminated broth wells detected\n")
  }
  
  # Step 2: Calculate robust broth correction using only clean wells
  clean_broth_data <- data %>%
    filter(sample_type == "broth", !well_id %in% contaminated_wells)
  
  if (nrow(clean_broth_data) == 0) {
    cat("     ️  All broth wells appear contaminated! Using minimal correction.\n")
    broth_correction_value <- 0
  } else {
    # Use median of clean wells at initial timepoint for stability
    broth_correction_value <- clean_broth_data %>%
      filter(time_point == min(time_point)) %>%
      summarise(correction = median(.data[[od_column]], na.rm = TRUE)) %>%
      pull(correction)
    
    cat("      Robust broth correction using", length(unique(clean_broth_data$well_id)), 
        "clean wells:", round(broth_correction_value, 4), "\n")
  }
  
  # Step 3: Calculate time-matched NP corrections by CONCENTRATION
  np_corrections_by_time <- data %>%
    filter(sample_type == "np_control") %>%
    group_by(concentration, time_hrs, time_point) %>%
    summarise(
      np_correction = mean(.data[[od_column]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # Apply the STANDARD correction
  corrected_data <- data %>%
    # Apply uniform broth correction
    mutate(
      !!od_final_column := case_when(
        sample_type %in% c("sample", "untreated_control") ~ {
          # For samples: subtract broth + subtract time-matched NP
          pmax(.data[[od_broth_corrected_column]] - np_correction + broth_correction_value, 0)
        },
        sample_type == "np_control" ~ {
          # For NP controls: just subtract broth
          .data[[od_broth_corrected_column]]
        },
        sample_type %in% c("broth", "blank") ~ {
          # For broth and blank wells: subtract the broth correction to zero them out
          pmax(.data[[od_column]] - broth_correction_value, 0)
        },
        TRUE ~ .data[[od_column]]  # For any other unknown types
      ),
      
      # Store correction components for diagnostics
      broth_correction_applied = broth_correction_value,
      np_correction_applied = ifelse(sample_type %in% c("sample", "untreated_control"), 
                                     np_correction, 0)
    )
  
  # Report correction statistics
  correction_stats <- corrected_data %>%
    filter(sample_type %in% c("sample", "untreated_control")) %>%
    summarise(
      mean_np_corr = mean(np_correction_applied, na.rm = TRUE),
      max_np_corr = max(np_correction_applied, na.rm = TRUE),
      negative_values = sum(.data[[od_final_column]] <= 0, na.rm = TRUE),
      total_samples = n(),
      .groups = "drop"
    )
  
  cat("      Final correction statistics:\n")
  cat("        Broth correction (uniform):", round(broth_correction_value, 4), "\n")
  cat("        NP correction range: 0 to", round(correction_stats$max_np_corr, 4), "\n")
  cat("        Average NP correction:", round(correction_stats$mean_np_corr, 4), "\n")
  cat("        Negative values after correction:", correction_stats$negative_values, 
      "out of", correction_stats$total_samples, "\n")
  
  return(corrected_data)
}

#' Calculate broth correction with unified column handling
calculate_broth_correction_unified <- function(data, method = "standard", od_column) {
  
  if (method == "traditional") {
    # Use any broth control, not just column 1
    broth_data <- data %>%
      filter(sample_type == "broth")
    if (nrow(broth_data) > 0) {
      # Use the first broth well found, initial timepoint
      broth_correction <- broth_data %>%
        filter(time_point == min(time_point)) %>%
        slice(1) %>%
        pull(!!od_column)
      cat("   Traditional broth correction: Single well (initial) =",
          round(broth_correction, 4), "\n")
    } else {
      cat("  ️  No broth control found, using 0\n")
      broth_correction <- 0
    }
  } else if (method == "robust") {
    # Enhanced robust method: Detect and exclude outlier wells
    broth_data <- data %>% filter(sample_type == "broth")
    if (nrow(broth_data) == 0) {
      cat("  ️  No broth controls found, using 0\n")
      broth_correction <- 0
    } else {
      # Calculate per-well statistics
      well_stats <- broth_data %>%
        group_by(well_id) %>%
        summarise(
          mean_od = mean(.data[[od_column]], na.rm = TRUE),
          drift = max(.data[[od_column]], na.rm = TRUE) - min(.data[[od_column]], na.rm = TRUE),
          .groups = "drop"
        )
      
      # Detect outliers using both mean OD and drift
      median_od <- median(well_stats$mean_od, na.rm = TRUE)
      mad_od <- mad(well_stats$mean_od, na.rm = TRUE)
      median_drift <- median(well_stats$drift, na.rm = TRUE)
      mad_drift <- mad(well_stats$drift, na.rm = TRUE)
      
      # Wells are outliers if they have high mean OD OR high drift
      outlier_wells <- well_stats %>%
        filter(
          abs(mean_od - median_od) > 3 * mad_od |
            drift > median_drift + 3 * mad_drift
        ) %>%
        pull(well_id)
      
      if (length(outlier_wells) > 0) {
        cat("  ️  Outlier broth wells detected:", paste(outlier_wells, collapse = ", "), "\n")
      }
      
      # Calculate correction using only good wells
      good_wells_data <- broth_data %>%
        filter(!well_id %in% outlier_wells)
      
      if (nrow(good_wells_data) > 0) {
        broth_correction <- good_wells_data %>%
          summarise(broth_od = median(.data[[od_column]], na.rm = TRUE)) %>%
          pull(broth_od)
        n_good_wells <- length(unique(good_wells_data$well_id))
        cat("   Robust broth correction: Median of", n_good_wells, "good wells =",
            round(broth_correction, 4), "\n")
      } else {
        cat("  ️  All broth wells flagged as outliers, using 0\n")
        broth_correction <- 0
      }
    }
  } else {
    stop("Method must be 'traditional' or 'robust'")
  }
  
  return(broth_correction)
}

#' Apply broth correction with unified column handling
apply_broth_correction_unified <- function(data, broth_correction, threshold = TRUE, od_column) {
  
  # Create the broth corrected column name
  if (od_column == "od600_raw") {
    od_broth_corrected_column <- "od600_broth_corrected"
  } else if (od_column == "od_raw") {
    od_broth_corrected_column <- "od_broth_corrected"
  } else if (od_column == "od600") {
    od_broth_corrected_column <- "od600_broth_corrected"
  } else {
    od_broth_corrected_column <- paste0(od_column, "_broth_corrected")
  }
  
  data %>%
    mutate(
      !!od_broth_corrected_column := if(threshold) {
        pmax(.data[[od_column]] - broth_correction, 0)
      } else {
        .data[[od_column]] - broth_correction
      }
    )
}

#' Apply half-broth correction method (NEW)
#' Apply half-broth correction method with two-stage NP processing
apply_halfbroth_correction_unified <- function(data, od_column) {
  cat("   Applying HALF-BROTH correction method (TWO-STAGE NP PROCESSING):\n")
  cat("     Step 1: Calculate Half Broth baseline from time 0 values\n")
  cat("     Step 2: Apply Material Control (NP) correction to Experiment wells using RAW NP values\n")
  cat("     Step 3: Apply contamination-aware Half Broth correction to NP wells\n")
  cat("     Step 4: Apply Half Broth baseline correction to Positive Control wells\n")
  cat("     Using OD column:", od_column, "\n")
  
  # Create column names based on the detected OD column
  if (od_column == "od600_raw") {
    od_broth_corrected_column <- "od600_broth_corrected"
    od_final_column <- "od600_final"
  } else if (od_column == "od_raw") {
    od_broth_corrected_column <- "od_broth_corrected"
    od_final_column <- "od_final"
  } else if (od_column == "od600") {
    od_broth_corrected_column <- "od600_broth_corrected"
    od_final_column <- "od600_final"
  } else {
    od_broth_corrected_column <- paste0(od_column, "_broth_corrected")
    od_final_column <- paste0(od_column, "_final")
  }
  
  # Step 1: Calculate Half Broth baseline from time 0 values
  halfbroth_data <- data %>%
    filter(sample_type == "half_broth")
  
  if (nrow(halfbroth_data) == 0) {
    cat("     No Half Broth wells found! Cannot apply Half Broth correction.\n")
    halfbroth_correction_value <- 0
  } else {
    # Get the earliest timepoint and apply contamination-aware detection
    earliest_timepoint <- min(halfbroth_data$time_point, na.rm = TRUE)
    
    # Apply contamination detection to half broth wells
    halfbroth_wells_analysis <- halfbroth_data %>%
      group_by(well_id) %>%
      summarise(
        mean_od = mean(.data[[od_column]], na.rm = TRUE),
        max_od = max(.data[[od_column]], na.rm = TRUE),
        min_od = min(.data[[od_column]], na.rm = TRUE),
        od_range = max_od - min_od,
        growth_trend = ifelse(n() > 1, 
                              cor(seq_along(.data[[od_column]]), .data[[od_column]], use = "complete.obs"),
                              0),
        .groups = "drop"
      )
    
    # Identify contaminated half broth wells
    median_mean_od <- median(halfbroth_wells_analysis$mean_od, na.rm = TRUE)
    mad_mean_od <- mad(halfbroth_wells_analysis$mean_od, na.rm = TRUE)
    median_range <- median(halfbroth_wells_analysis$od_range, na.rm = TRUE)
    mad_range <- mad(halfbroth_wells_analysis$od_range, na.rm = TRUE)
    
    contaminated_halfbroth_wells <- halfbroth_wells_analysis %>%
      filter(
        (mean_od > median_mean_od + 3 * mad_mean_od) |
          (od_range > median_range + 3 * mad_range) |
          (growth_trend > 0.3)
      ) %>%
      pull(well_id)
    
    if (length(contaminated_halfbroth_wells) > 0) {
      cat("     ️  Contaminated Half Broth wells detected:", paste(contaminated_halfbroth_wells, collapse = ", "), "\n")
    }
    
    # Calculate baseline using clean half broth wells
    clean_halfbroth_data <- halfbroth_data %>%
      filter(!well_id %in% contaminated_halfbroth_wells,
             time_point == earliest_timepoint)
    
    if (nrow(clean_halfbroth_data) > 0) {
      halfbroth_correction_value <- clean_halfbroth_data %>%
        summarise(baseline = median(.data[[od_column]], na.rm = TRUE)) %>%
        pull(baseline)
      cat("     Half Broth baseline from clean wells:", round(halfbroth_correction_value, 4), "\n")
    } else {
      cat("     All Half Broth wells contaminated, using median of all:", "\n")
      halfbroth_correction_value <- halfbroth_data %>%
        filter(time_point == earliest_timepoint) %>%
        summarise(baseline = median(.data[[od_column]], na.rm = TRUE)) %>%
        pull(baseline)
    }
  }
  
  # Step 2: Calculate NP corrections using RAW NP values (before any correction to NP wells)
  np_control_count <- sum(data$sample_type == "np_control", na.rm = TRUE)
  
  if (np_control_count > 0) {
    np_corrections_by_time <- data %>%
      filter(sample_type == "np_control") %>%
      group_by(concentration, time_hrs, time_point) %>%
      summarise(
        np_correction = mean(.data[[od_column]], na.rm = TRUE),  # Using RAW values
        .groups = "drop"
      )
    cat("     NP corrections calculated using RAW NP values for",
        length(unique(np_corrections_by_time$concentration)), "concentrations\n")
  } else {
    cat("     No NP controls present\n")
    np_corrections_by_time <- tibble(
      concentration = character(0),
      time_hrs = numeric(0),
      time_point = integer(0),
      np_correction = numeric(0)
    )
  }
  
  # Step 3 & 4: Apply all corrections in the proper order
  corrected_data <- data %>%
    # First, create intermediate correction column
    mutate(
      !!od_broth_corrected_column := .data[[od_column]]  # Placeholder
    ) %>%
    # Join NP corrections (calculated from RAW NP values)
    left_join(np_corrections_by_time, by = c("concentration", "time_hrs", "time_point")) %>%
    mutate(
      np_correction = ifelse(is.na(np_correction), 0, np_correction),
      # Apply final corrections based on sample type
      !!od_final_column := case_when(
        # Experiment wells: subtract RAW NP control values
        sample_type == "sample" ~ pmax(.data[[od_column]] - np_correction, 0),
        
        # Positive Control (Untreated Bacteria): subtract Half Broth baseline
        sample_type == "untreated_control" ~ pmax(.data[[od_column]] - halfbroth_correction_value, 0),
        
        # NP controls: subtract Half Broth baseline (STAGE 2 CORRECTION)
        sample_type == "np_control" ~ pmax(.data[[od_column]] - halfbroth_correction_value, 0),
        
        # Half Broth wells: subtract their own baseline (should go to ~0)
        sample_type == "half_broth" ~ pmax(.data[[od_column]] - halfbroth_correction_value, 0),
        
        # Regular broth wells: no correction
        sample_type == "broth" ~ .data[[od_column]],
        
        # Blanks: no correction
        sample_type == "blank" ~ .data[[od_column]],
        
        TRUE ~ .data[[od_column]]
      ),
      
      # Store correction components for diagnostics
      broth_correction_applied = case_when(
        sample_type == "untreated_control" ~ halfbroth_correction_value,
        sample_type == "np_control" ~ halfbroth_correction_value,
        sample_type == "half_broth" ~ halfbroth_correction_value,
        TRUE ~ 0
      ),
      np_correction_applied = ifelse(sample_type == "sample", np_correction, 0)
    )
  
  # Report correction statistics
  correction_stats <- corrected_data %>%
    filter(sample_type %in% c("sample", "untreated_control", "np_control")) %>%
    summarise(
      mean_np_corr = mean(np_correction_applied[sample_type == "sample"], na.rm = TRUE),
      max_np_corr = max(np_correction_applied[sample_type == "sample"], na.rm = TRUE),
      mean_halfbroth_corr = mean(broth_correction_applied[sample_type %in% c("untreated_control", "np_control")], na.rm = TRUE),
      negative_values = sum(.data[[od_final_column]] <= 0, na.rm = TRUE),
      total_samples = n(),
      .groups = "drop"
    )
  
  cat("        Final correction statistics:\n")
  cat("        Half Broth baseline correction:", round(halfbroth_correction_value, 4), "\n")
  if (np_control_count > 0) {
    cat("        NP correction range: 0 to", round(correction_stats$max_np_corr, 4), "\n")
    cat("        Average NP correction:", round(correction_stats$mean_np_corr, 4), "\n")
  }
  cat("        Negative values after correction:", correction_stats$negative_values,
      "out of", correction_stats$total_samples, "\n")
  
  return(corrected_data)
}

apply_halfbroth_correction_multi <- function(data, wavelength) {
  od_raw_column <- paste0("od", wavelength, "_raw")
  od_broth_corrected_column <- paste0("od", wavelength, "_broth_corrected")
  od_final_column <- paste0("od", wavelength, "_final")
  
  cat("  :abacus: Applying HALF-BROTH correction for", wavelength, "nm (TWO-STAGE NP PROCESSING):\n")
  cat("     Step 1: Calculate contamination-aware Half Broth baseline\n")
  cat("     Step 2: Apply NP correction to Experiment wells using RAW NP values\n")
  cat("     Step 3: Apply Half Broth correction to NP wells\n")
  
  # Step 1: Calculate contamination-aware Half Broth baseline
  halfbroth_data <- data %>%
    filter(sample_type == "half_broth")
  
  if (nrow(halfbroth_data) == 0) {
    halfbroth_correction_value <- 0
  } else {
    earliest_timepoint <- min(halfbroth_data$time_point, na.rm = TRUE)
    
    # Contamination detection for half broth wells
    halfbroth_wells_analysis <- halfbroth_data %>%
      group_by(well_id) %>%
      summarise(
        mean_od = mean(.data[[od_raw_column]], na.rm = TRUE),
        max_od = max(.data[[od_raw_column]], na.rm = TRUE),
        min_od = min(.data[[od_raw_column]], na.rm = TRUE),
        od_range = max_od - min_od,
        growth_trend = ifelse(n() > 1, 
                              cor(seq_along(.data[[od_raw_column]]), .data[[od_raw_column]], use = "complete.obs"),
                              0),
        .groups = "drop"
      )
    
    median_mean_od <- median(halfbroth_wells_analysis$mean_od, na.rm = TRUE)
    mad_mean_od <- mad(halfbroth_wells_analysis$mean_od, na.rm = TRUE)
    median_range <- median(halfbroth_wells_analysis$od_range, na.rm = TRUE)
    mad_range <- mad(halfbroth_wells_analysis$od_range, na.rm = TRUE)
    
    contaminated_halfbroth_wells <- halfbroth_wells_analysis %>%
      filter(
        (mean_od > median_mean_od + 3 * mad_mean_od) |
          (od_range > median_range + 3 * mad_range) |
          (growth_trend > 0.3)
      ) %>%
      pull(well_id)
    
    if (length(contaminated_halfbroth_wells) > 0) {
      cat("     ️  Contaminated Half Broth wells (", wavelength, "nm):", 
          paste(contaminated_halfbroth_wells, collapse = ", "), "\n")
    }
    
    clean_halfbroth_data <- halfbroth_data %>%
      filter(!well_id %in% contaminated_halfbroth_wells,
             time_point == earliest_timepoint)
    
    if (nrow(clean_halfbroth_data) > 0) {
      halfbroth_correction_value <- clean_halfbroth_data %>%
        summarise(baseline = median(.data[[od_raw_column]], na.rm = TRUE)) %>%
        pull(baseline)
    } else {
      halfbroth_correction_value <- halfbroth_data %>%
        filter(time_point == earliest_timepoint) %>%
        summarise(baseline = median(.data[[od_raw_column]], na.rm = TRUE)) %>%
        pull(baseline)
    }
    
    cat("     Half Broth baseline (", wavelength, "nm):", round(halfbroth_correction_value, 4), "\n")
  }
  
  # Apply broth corrected column (placeholder)
  data <- data %>%
    mutate(
      !!od_broth_corrected_column := .data[[od_raw_column]]
    )
  
  # Step 2: Check for NP controls and calculate corrections using RAW values
  np_control_count <- sum(data$sample_type == "np_control", na.rm = TRUE)
  
  if (np_control_count == 0) {
    cat("     :memo: No NP controls present\n")
    corrected_data <- data %>%
      mutate(
        !!od_final_column := case_when(
          sample_type == "untreated_control" ~ pmax(.data[[od_raw_column]] - halfbroth_correction_value, 0),
          sample_type == "np_control" ~ pmax(.data[[od_raw_column]] - halfbroth_correction_value, 0),
          sample_type == "half_broth" ~ pmax(.data[[od_raw_column]] - halfbroth_correction_value, 0),
          TRUE ~ .data[[od_raw_column]]
        ),
        broth_correction_applied = case_when(
          sample_type %in% c("untreated_control", "np_control", "half_broth") ~ halfbroth_correction_value,
          TRUE ~ 0
        ),
        np_correction_applied = 0,
        np_correction = 0
      )
  } else {
    cat("     NP controls detected - using RAW values for experimental correction\n")
    
    # Calculate NP corrections using RAW values
    np_corrections_by_time <- data %>%
      filter(sample_type == "np_control") %>%
      group_by(concentration, time_hrs, time_point) %>%
      summarise(
        np_correction = mean(.data[[od_raw_column]], na.rm = TRUE),  # RAW values
        .groups = "drop"
      )
    
    # Apply corrections with two-stage NP processing
    corrected_data <- data %>%
      left_join(np_corrections_by_time, by = c("concentration", "time_hrs", "time_point")) %>%
      mutate(
        np_correction = ifelse(is.na(np_correction), 0, np_correction),
        !!od_final_column := case_when(
          # Experiment wells: subtract RAW NP values
          sample_type == "sample" ~ pmax(.data[[od_raw_column]] - np_correction, 0),
          
          # Positive Control: subtract Half Broth baseline
          sample_type == "untreated_control" ~ pmax(.data[[od_raw_column]] - halfbroth_correction_value, 0),
          
          # NP controls: subtract Half Broth baseline (STAGE 2)
          sample_type == "np_control" ~ pmax(.data[[od_raw_column]] - halfbroth_correction_value, 0),
          
          # Half Broth: subtract baseline
          sample_type == "half_broth" ~ pmax(.data[[od_raw_column]] - halfbroth_correction_value, 0),
          
          # Everything else: no correction
          TRUE ~ .data[[od_raw_column]]
        ),
        broth_correction_applied = case_when(
          sample_type %in% c("untreated_control", "np_control", "half_broth") ~ halfbroth_correction_value,
          TRUE ~ 0
        ),
        np_correction_applied = ifelse(sample_type == "sample", np_correction, 0)
      )
  }
  
  # Report statistics
  correction_stats <- corrected_data %>%
    filter(sample_type %in% c("sample", "untreated_control", "np_control")) %>%
    summarise(
      mean_np_corr = mean(np_correction_applied[sample_type == "sample"], na.rm = TRUE),
      max_np_corr = max(np_correction_applied[sample_type == "sample"], na.rm = TRUE),
      mean_halfbroth_corr = mean(broth_correction_applied[sample_type %in% c("untreated_control", "np_control")], na.rm = TRUE),
      negative_values = sum(.data[[od_final_column]] <= 0, na.rm = TRUE),
      total_samples = n(),
      .groups = "drop"
    )
  
  cat("     Final correction statistics (", wavelength, "nm):\n")
  cat("        Half Broth baseline correction:", round(halfbroth_correction_value, 4), "\n")
  if (np_control_count > 0) {
    cat("        NP correction range: 0 to", round(correction_stats$max_np_corr, 4), "\n")
    cat("        Average NP correction:", round(correction_stats$mean_np_corr, 4), "\n")
  }
  cat("        Negative values:", correction_stats$negative_values,
      "out of", correction_stats$total_samples, "\n")
  
  return(corrected_data)
}

#' Calculate NP corrections with unified column handling
#' Calculate NP corrections with unified column handling (UPDATED for optional NP)
calculate_np_corrections_unified <- function(data_broth_corrected, method = "standard", od_column) {
  
  # Check if there are any NP controls in the data
  np_control_count <- sum(data_broth_corrected$sample_type == "np_control", na.rm = TRUE)
  
  if (np_control_count == 0) {
    cat("   No NP controls detected - skipping NP correction\n")
    # Return empty corrections table with proper structure
    return(tibble(
      time_hrs = numeric(0),
      time_point = numeric(0),
      concentration = character(0),
      np_correction = numeric(0)
    ))
  }
  
  # If we have NP controls, proceed as normal
  if (method == "traditional") {
    cat("   Traditional NP correction: Same-timepoint pairing with RAW NP values\n")
    np_corrections <- data_broth_corrected %>%
      filter(sample_type == "np_control") %>%
      select(time_hrs, time_point, !!od_column, well_id, concentration) %>%
      select(time_hrs, time_point, concentration, np_correction = !!od_column)
  } else if (method == "robust") {
    cat("   Robust NP correction: Averaged by concentration and time with RAW NP values\n")
    np_corrections <- data_broth_corrected %>%
      filter(sample_type == "np_control") %>%
      select(time_hrs, time_point, concentration, !!od_column) %>%
      group_by(time_hrs, time_point, concentration) %>%
      summarise(np_correction = mean(.data[[od_column]], na.rm = TRUE), .groups = "drop")
  } else {
    stop("Method must be 'traditional' or 'robust'")
  }
  
  return(np_corrections)
}

#' Apply NP corrections with unified column handling (UPDATED for optional NP)
apply_np_corrections_unified <- function(data_broth_corrected, np_corrections, threshold = TRUE, od_column) {
  
  # Determine column names
  if (od_column == "od600_raw") {
    od_broth_corrected_column <- "od600_broth_corrected"
    od_final_column <- "od600_final"
  } else if (od_column == "od_raw") {
    od_broth_corrected_column <- "od_broth_corrected"
    od_final_column <- "od_final"
  } else if (od_column == "od600") {
    od_broth_corrected_column <- "od600_broth_corrected"
    od_final_column <- "od600_final"
  } else {
    od_broth_corrected_column <- paste0(od_column, "_broth_corrected")
    od_final_column <- paste0(od_column, "_final")
  }
  
  # Check if we have any NP corrections to apply
  if (nrow(np_corrections) == 0) {
    cat("   No NP corrections to apply - using broth-corrected values as final\n")
    # Simply copy broth-corrected to final
    return(data_broth_corrected %>%
             mutate(
               np_correction = 0,
               !!od_final_column := .data[[od_broth_corrected_column]]
             ))
  }
  
  # If we have NP corrections, apply them as before
  data_broth_corrected %>%
    left_join(np_corrections, by = c("time_hrs", "time_point", "concentration")) %>%
    mutate(
      np_correction = ifelse(is.na(np_correction), 0, np_correction),
      !!od_final_column := case_when(
        sample_type %in% c("sample", "untreated_control") ~ {
          if(threshold) {
            pmax(.data[[od_broth_corrected_column]] - np_correction, 0)
          } else {
            .data[[od_broth_corrected_column]] - np_correction
          }
        },
        TRUE ~ .data[[od_broth_corrected_column]]
      )
    )
}

#' Apply standard correction method with unified column handling (UPDATED for optional NP)
apply_standard_correction_unified <- function(data, od_column) {
  
  cat("   Applying STANDARD correction (contamination-safe):\n")
  cat("     Step 1: Detect contaminated broth controls\n")
  cat("     Step 2: Apply robust broth correction\n") 
  cat("     Step 3: Apply time-matched NP correction (if NP controls present)\n")
  cat("     Using OD column:", od_column, "\n")
  
  # Create column names based on the detected OD column
  if (od_column == "od600_raw") {
    od_broth_corrected_column <- "od600_broth_corrected"
    od_final_column <- "od600_final"
  } else if (od_column == "od_raw") {
    od_broth_corrected_column <- "od_broth_corrected"
    od_final_column <- "od_final"
  } else if (od_column == "od600") {
    od_broth_corrected_column <- "od600_broth_corrected"
    od_final_column <- "od600_final"
  } else {
    od_broth_corrected_column <- paste0(od_column, "_broth_corrected")
    od_final_column <- paste0(od_column, "_final")
  }
  
  # Step 1: Detect contaminated broth controls using robust statistics
  broth_wells_analysis <- data %>%
    filter(sample_type == "broth") %>%
    group_by(well_id) %>%
    summarise(
      mean_od = mean(.data[[od_column]], na.rm = TRUE),
      max_od = max(.data[[od_column]], na.rm = TRUE),
      min_od = min(.data[[od_column]], na.rm = TRUE),
      od_range = max_od - min_od,
      growth_trend = ifelse(n() > 1, 
                            cor(seq_along(.data[[od_column]]), .data[[od_column]], use = "complete.obs"),
                            0),
      .groups = "drop"
    )
  
  # Identify potential contaminants using multiple criteria
  median_mean_od <- median(broth_wells_analysis$mean_od, na.rm = TRUE)
  mad_mean_od <- mad(broth_wells_analysis$mean_od, na.rm = TRUE)
  median_range <- median(broth_wells_analysis$od_range, na.rm = TRUE)
  mad_range <- mad(broth_wells_analysis$od_range, na.rm = TRUE)
  
  contaminated_wells <- broth_wells_analysis %>%
    filter(
      (mean_od > median_mean_od + 3 * mad_mean_od) |
        (od_range > median_range + 3 * mad_range) |
        (growth_trend > 0.3)
    ) %>%
    pull(well_id)
  
  if (length(contaminated_wells) > 0) {
    cat("     ️  Contaminated broth wells detected:", paste(contaminated_wells, collapse = ", "), "\n")
  } else {
    cat("      No contaminated broth wells detected\n")
  }
  
  # Step 2: Calculate robust broth correction using only clean wells
  clean_broth_data <- data %>%
    filter(sample_type == "broth", !well_id %in% contaminated_wells)
  
  if (nrow(clean_broth_data) == 0) {
    cat("     ️  All broth wells appear contaminated! Using minimal correction.\n")
    broth_correction_value <- 0
  } else {
    broth_correction_value <- clean_broth_data %>%
      filter(time_point == min(time_point)) %>%
      summarise(correction = median(.data[[od_column]], na.rm = TRUE)) %>%
      pull(correction)
    
    cat("      Robust broth correction using", length(unique(clean_broth_data$well_id)), 
        "clean wells:", round(broth_correction_value, 4), "\n")
  }
  
  # FIRST, apply broth correction to create the broth_corrected column
  data <- data %>%
    mutate(
      !!od_broth_corrected_column := pmax(.data[[od_column]] - broth_correction_value, 0)
    )
  
  # Step 3: Check if NP controls exist
  np_control_count <- sum(data$sample_type == "np_control", na.rm = TRUE)
  
  if (np_control_count == 0) {
    cat("      No NP controls present - applying broth correction only\n")
    
    # Apply only broth correction (no NP correction)
    corrected_data <- data %>%
      mutate(
        !!od_final_column := .data[[od_broth_corrected_column]],
        broth_correction_applied = broth_correction_value,
        np_correction_applied = 0,
        np_correction = 0  # Add this for consistency
      )
    
  } else {
    cat("      NP controls detected - calculating time-matched corrections\n")
    
    # Calculate time-matched NP corrections by CONCENTRATION
    np_corrections_by_time <- data %>%
      filter(sample_type == "np_control") %>%
      group_by(concentration, time_hrs, time_point) %>%
      summarise(
        np_correction = mean(.data[[od_column]], na.rm = TRUE),
        .groups = "drop"
      )
    
    # Apply the STANDARD correction with NP
    corrected_data <- data %>%
      left_join(np_corrections_by_time, by = c("concentration", "time_hrs", "time_point")) %>%
      mutate(
        np_correction = ifelse(is.na(np_correction), 0, np_correction),
        !!od_final_column := case_when(
          sample_type %in% c("sample", "untreated_control") ~ {
            # For samples: subtract both broth and NP, but add back broth to get net NP effect
            pmax(.data[[od_broth_corrected_column]] - np_correction + broth_correction_value, 0)
          },
          sample_type == "np_control" ~ {
            # For NP controls: just use broth-corrected value
            .data[[od_broth_corrected_column]]
          },
          sample_type %in% c("broth", "blank") ~ {
            # For broth and blank: use broth-corrected value
            .data[[od_broth_corrected_column]]
          },
          TRUE ~ .data[[od_column]]  # For any unknown types
        ),
        broth_correction_applied = broth_correction_value,
        np_correction_applied = ifelse(sample_type %in% c("sample", "untreated_control"), 
                                       np_correction, 0)
      )
  }
  
  # Report correction statistics
  correction_stats <- corrected_data %>%
    filter(sample_type %in% c("sample", "untreated_control")) %>%
    summarise(
      mean_np_corr = mean(np_correction_applied, na.rm = TRUE),
      max_np_corr = max(np_correction_applied, na.rm = TRUE),
      negative_values = sum(.data[[od_final_column]] <= 0, na.rm = TRUE),
      total_samples = n(),
      .groups = "drop"
    )
  
  cat("      Final correction statistics:\n")
  cat("        Broth correction (uniform):", round(broth_correction_value, 4), "\n")
  if (np_control_count > 0) {
    cat("        NP correction range: 0 to", round(correction_stats$max_np_corr, 4), "\n")
    cat("        Average NP correction:", round(correction_stats$mean_np_corr, 4), "\n")
  } else {
    cat("        No NP corrections applied\n")
  }
  cat("        Negative values after correction:", correction_stats$negative_values, 
      "out of", correction_stats$total_samples, "\n")
  
  return(corrected_data)
}

#' Updated QC plots to handle missing NP controls
#' Create NP kinetics plot (FIXED to handle missing NP controls properly)
create_np_kinetics_plot <- function(corrected_results) {
  
  # Check if there are any NP controls
  np_data <- corrected_results$corrected_data %>%
    filter(sample_type == "np_control")
  
  if (nrow(np_data) == 0) {
    cat("• No NP controls found - creating placeholder plot\n")
    # Return a placeholder plot
    placeholder_plot <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = "No NP Controls\nin this experiment", 
               size = 8, hjust = 0.5, vjust = 0.5,
               color = "gray50") +
      theme_void() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        panel.border = element_rect(color = "gray80", fill = NA, linewidth = 1)
      ) +
      labs(title = "QC: Nanoparticle Kinetics",
           subtitle = "NP controls not included in experimental design")
    
    return(placeholder_plot)
  }
  
  # If we have NP controls, proceed with the normal plot
  # Get concentrations in proper numerical order
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
    facet_wrap(~concentration_f, scales = "fixed") +
    labs(
      title = "QC: Nanoparticle Kinetics",
      subtitle = "Concentrations ordered numerically with fixed scale for comparison",
      x = "Time (hours)",
      y = "OD600 (broth corrected)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold")
    )
}

#' Update the layout validation to make NP controls optional
#' Validate flexible layout (COMPLETELY REWRITTEN)
validate_flexible_layout <- function(layout, experiment_params) {
  
  cat(" Layout validation:\n")
  
  # Debug: Check the structure of the layout
  cat("• Layout structure check:\n")
  cat("  - Dimensions:", nrow(layout), "x", ncol(layout), "\n")
  cat("  - sample_type class:", class(layout$sample_type), "\n")
  cat("  - concentration class:", class(layout$concentration), "\n")
  
  # Check for any list columns
  list_columns <- sapply(layout, function(x) any(sapply(x, is.list)))
  if (any(list_columns)) {
    cat("  ️  ERROR: List columns detected:", paste(names(list_columns)[list_columns], collapse = ", "), "\n")
    cat("  Attempting to fix...\n")
    
    # Try to fix list columns
    for (col_name in names(list_columns)[list_columns]) {
      cat("    Fixing column:", col_name, "\n")
      layout[[col_name]] <- as.character(unlist(layout[[col_name]]))
    }
  }
  
  # Show unique values for debugging
  cat("  - unique sample_types:", paste(unique(layout$sample_type), collapse = ", "), "\n")
  cat("  - unique concentrations (first 5):", paste(head(unique(layout$concentration[!is.na(layout$concentration)]), 5), collapse = ", "), "\n")
  
  # Safely check concentration assignments with multiple fallbacks
  cat("• Concentration assignments:\n")
  
  tryCatch({
    # First attempt - standard approach
    conc_check <- layout %>%
      filter(!is.na(concentration)) %>%
      # TRIPLE ensure both columns are character vectors
      mutate(
        sample_type = as.character(unlist(sample_type)),
        concentration = as.character(unlist(concentration))
      ) %>%
      arrange(sample_type, concentration) %>%
      count(sample_type, concentration, name = "n_wells") %>%
      arrange(sample_type, concentration)
    
    print(conc_check)
    
  }, error = function(e) {
    cat("  Error in standard approach:", e$message, "\n")
    
    # Fallback approach - manual counting
    tryCatch({
      cat("  Using fallback manual counting...\n")
      
      valid_rows <- layout[!is.na(layout$concentration), ]
      
      if (nrow(valid_rows) > 0) {
        # Manual counting
        manual_count <- valid_rows %>%
          select(sample_type, concentration) %>%
          # Ensure character conversion
          mutate(
            sample_type = as.character(unlist(sample_type)),
            concentration = as.character(unlist(concentration))
          ) %>%
          group_by(sample_type, concentration) %>%
          summarise(n_wells = n(), .groups = "drop") %>%
          arrange(sample_type, concentration)
        
        print(manual_count)
      } else {
        cat("  No valid concentration data found\n")
      }
      
    }, error = function(e2) {
      cat("  Fallback also failed:", e2$message, "\n")
      cat("  Showing raw data sample:\n")
      print(head(layout[c("sample_type", "concentration")], 10))
    })
  })
  
  # Check if Half Broth controls are present
  half_broth_present <- "half_broth" %in% unique(as.character(unlist(layout$sample_type)))
  if (half_broth_present) {
    cat("\n• NOTE: Half Broth controls detected\n")
    cat("  This will trigger the new Half Broth correction method\n")
  }
  
  # Check if NP controls are present
  np_present <- "np_control" %in% unique(as.character(unlist(layout$sample_type)))
  if (!np_present) {
    cat("\n• NOTE: No NP controls detected in layout\n")
    cat("  This is acceptable for experiments without nanoparticle treatments\n")
  }
  
  # Validate untreated controls specifically
  cat("\n• Untreated control assignments:\n")
  tryCatch({
    unt_check <- layout %>%
      filter(as.character(unlist(sample_type)) == "untreated_control") %>%
      select(well_id, concentration, replicate_id) %>%
      mutate(
        concentration = as.character(unlist(concentration)),
        replicate_id = as.character(unlist(replicate_id))
      ) %>%
      arrange(concentration)
    print(unt_check)
  }, error = function(e) {
    cat("  Error displaying untreated controls:", e$message, "\n")
  })
  
  return(invisible(TRUE))
}

#' Apply NP corrections with unified column handling
apply_np_corrections_unified <- function(data_broth_corrected, np_corrections, threshold = TRUE, od_column) {
  
  # Determine column names
  if (od_column == "od600_raw") {
    od_broth_corrected_column <- "od600_broth_corrected"
    od_final_column <- "od600_final"
  } else if (od_column == "od_raw") {
    od_broth_corrected_column <- "od_broth_corrected"
    od_final_column <- "od_final"
  } else if (od_column == "od600") {
    od_broth_corrected_column <- "od600_broth_corrected"
    od_final_column <- "od600_final"
  } else {
    od_broth_corrected_column <- paste0(od_column, "_broth_corrected")
    od_final_column <- paste0(od_column, "_final")
  }
  
  data_broth_corrected %>%
    left_join(np_corrections, by = c("time_hrs", "time_point", "concentration")) %>%
    mutate(
      np_correction = ifelse(is.na(np_correction), 0, np_correction),
      !!od_final_column := case_when(
        sample_type %in% c("sample", "untreated_control") ~ {
          if(threshold) {
            pmax(.data[[od_broth_corrected_column]] - np_correction, 0)
          } else {
            .data[[od_broth_corrected_column]] - np_correction
          }
        },
        TRUE ~ .data[[od_broth_corrected_column]]
      )
    )
}

#' Calculate replicate statistics with unified column handling
calculate_replicate_statistics_unified <- 
  function(data_fully_corrected, od_column) {
    
    # Determine the final column name
    if (od_column == "od600_raw") {
      od_final_column <- "od600_final"
    } else if (od_column == "od_raw") {
      od_final_column <- "od_final"
    } else if (od_column == "od600") {
      od_final_column <- "od600_final"
    } else {
      od_final_column <- paste0(od_column, "_final")
    }
    
    # Clean and standardize time points first
    data_clean <- data_fully_corrected %>%
      filter(sample_type == "sample") %>%
      # Round time_hrs to avoid floating point precision issues
      mutate(time_hrs_rounded = round(time_hrs, 3)) %>%
      # Group by rounded time to ensure consistency
      group_by(sample_type, concentration, time_point, time_hrs_rounded) %>%
      # If there are multiple time_hrs for same time_point, use the most common one
      mutate(
        time_hrs_consensus = median(time_hrs, na.rm = TRUE),
        n_wells_at_timepoint = n()
      ) %>%
      ungroup() %>%
      # Use consensus time
      mutate(time_hrs = time_hrs_consensus) %>%
      select(-time_hrs_rounded, -time_hrs_consensus)
    
    # Now calculate statistics properly
    replicate_stats <- data_clean %>%
      group_by(sample_type, concentration, time_point, time_hrs) %>%
      summarise(
        n_replicates = n(),
        mean_od = mean(.data[[od_final_column]], na.rm = TRUE),
        sd_od = sd(.data[[od_final_column]], na.rm = TRUE),
        median_od = median(.data[[od_final_column]], na.rm = TRUE),
        min_od = min(.data[[od_final_column]], na.rm = TRUE),
        max_od = max(.data[[od_final_column]], na.rm = TRUE),
        wells_included = paste(sort(well_id), collapse = ","),
        .groups = "drop"
      ) %>%
      mutate(
        # Standard error calculation with safety checks
        se_od_raw = sd_od / sqrt(n_replicates),
        se_od = case_when(
          n_replicates <= 1 ~ 0,
          is.na(se_od_raw) | is.infinite(se_od_raw) ~ 0,
          se_od_raw > mean_od * 0.5 ~ mean_od * 0.1,  # Cap large SE
          TRUE ~ se_od_raw
        )
      ) %>%
      select(-se_od_raw)
    
    # Debug: Check for remaining duplicates
    duplicate_timepoints <- replicate_stats %>%
      group_by(sample_type, concentration, time_point) %>%
      filter(n() > 1) %>%
      ungroup()
    
    if (nrow(duplicate_timepoints) > 0) {
      cat("️  Warning: Still have duplicate time points after cleaning:\n")
      print(duplicate_timepoints %>% 
              select(concentration, time_point, time_hrs, n_replicates, wells_included))
      
      # If duplicates remain, keep the one with more replicates
      replicate_stats <- replicate_stats %>%
        group_by(sample_type, concentration, time_point) %>%
        slice_max(n_replicates, n = 1, with_ties = FALSE) %>%
        ungroup()
    }
    
    # Report statistics
    conc_summary <- replicate_stats %>%
      group_by(concentration) %>%
      summarise(
        min_reps = min(n_replicates),
        max_reps = max(n_replicates),
        mean_reps = round(mean(n_replicates), 1),
        .groups = "drop"
      )
    
    cat("• Replicate summary by concentration:\n")
    print(conc_summary)
    
    return(replicate_stats)
  }

# ===== SUMMARY FUNCTIONS =====

#' Create comprehensive summary of replicate results
summarize_replicate_results <- function(corrected_results) {
  
  cat(" Calculating experiment metrics...\n")
  
  # CLEAN the corrected data first
  corrected_results$corrected_data <- clean_corrected_data(corrected_results$corrected_data)
  
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
  
  cat(" Summary calculations complete\n")
  
  return(list(
    experiment = experiment_summary,
    growth_metrics = growth_metrics,
    time_summaries = time_summaries,
    statistics = statistical_summaries,
    quality = quality_metrics
  ))
}

#' Create experiment summary with safe statistics
create_experiment_summary <- function(corrected_results) {
  
  data <- corrected_results$corrected_data
  
  # Basic experiment info with safe statistics
  basic_info <- data %>%
    summarise(
      total_wells = n_distinct(well_id),
      total_timepoints = n_distinct(time_point),
      experiment_duration_hrs = safe_max(time_hrs) - safe_min(time_hrs),
      sampling_interval_hrs = ifelse(
        length(unique(time_hrs)) > 1,
        round(mean(diff(sort(unique(time_hrs))), na.rm = TRUE), 2),
        NA_real_
      ),
      temperature_range = paste(
        round(safe_min(temperature, na.rm = TRUE), 1), "-", 
        round(safe_max(temperature, na.rm = TRUE), 1), "C"
      )
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

#' Calculate growth metrics with safe statistics
calculate_growth_metrics_summary <- function(corrected_results) {
  
  # Clean the data first
  corrected_data <- clean_corrected_data(corrected_results$corrected_data)
  
  # Per-well metrics with safe calculations
  well_metrics <- corrected_data %>%
    filter(sample_type %in% c("sample", "untreated_control")) %>%
    group_by(well_id, sample_type, concentration, replicate_id) %>%
    safe_summarise(
      initial_od = ifelse(n() > 0, .data[["od600_final"]][1], NA_real_),  # FIXED: Use [1] instead of first()
      final_od = ifelse(n() > 0, .data[["od600_final"]][n()], NA_real_),  # FIXED: Use [n()] instead of last()
      max_od = safe_max(od600_final, na.rm = TRUE),
      time_to_max_hrs = ifelse(
        length(od600_final) > 0 && !all(is.na(od600_final)),
        time_hrs[which.max(od600_final)],
        NA_real_
      ),
      total_growth = final_od - initial_od,
      max_growth = max_od - initial_od,
      auc = sum(od600_final * 
                  ifelse(length(unique(time_hrs)) > 1,
                         mean(diff(sort(unique(time_hrs))), na.rm = TRUE),
                         1), 
                na.rm = TRUE),
      .groups = "drop"
    )
  
  # Summary metrics by condition with safe calculations
  condition_metrics <- well_metrics %>%
    group_by(sample_type, concentration) %>%
    safe_summarise(
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
      cv_final_od = ifelse(mean_final_od != 0, sd_final_od / mean_final_od * 100, NA_real_),
      cv_total_growth = ifelse(mean_total_growth != 0, sd_total_growth / abs(mean_total_growth) * 100, NA_real_)
    )
  
  return(list(
    well_metrics = well_metrics,
    condition_metrics = condition_metrics
  ))
}

#' Create time-based summaries with safe statistics
create_time_summaries <- function(corrected_results) {
  
  # Growth at specific timepoints with safe quantiles
  time_values <- corrected_results$replicate_stats %>%
    filter(sample_type == "sample") %>%
    pull(time_hrs)
  
  if (length(time_values) > 0) {
    key_timepoints <- suppressWarnings(
      quantile(time_values, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
    ) %>% round(1)
  } else {
    key_timepoints <- numeric(0)
  }
  
  timepoint_summary <- if(length(key_timepoints) > 0) {
    corrected_results$replicate_stats %>%
      filter(time_hrs %in% key_timepoints) %>%
      select(sample_type, concentration, time_hrs, mean_od, se_od) %>%
      pivot_wider(
        names_from = time_hrs,
        values_from = c(mean_od, se_od),
        names_sep = "_hrs_"
      )
  } else {
    tibble()
  }
  
  # Growth phases (early, mid, late) with safe range
  time_range <- safe_range(corrected_results$replicate_stats$time_hrs)
  
  phase_summary <- if (!any(is.na(time_range))) {
    corrected_results$replicate_stats %>%
      mutate(
        growth_phase = case_when(
          time_hrs <= time_range[1] + (time_range[2] - time_range[1]) * 0.33 ~ "early",
          time_hrs <= time_range[1] + (time_range[2] - time_range[1]) * 0.67 ~ "mid",
          TRUE ~ "late"
        )
      ) %>%
      group_by(sample_type, concentration, growth_phase) %>%
      safe_summarise(
        mean_od_phase = mean(mean_od, na.rm = TRUE),
        max_od_phase = safe_max(mean_od, na.rm = TRUE),
        n_timepoints = n(),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = growth_phase,
        values_from = c(mean_od_phase, max_od_phase),
        names_sep = "_"
      )
  } else {
    tibble()
  }
  
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
  
  # Broth stability (Broth Only wells)
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
  
  # Half Broth stability (NEW)
  halfbroth_stability <- corrected_results$corrected_data %>%
    filter(sample_type == "half_broth") %>%
    group_by(time_hrs) %>%
    summarise(mean_halfbroth_od = mean(od600_raw, na.rm = TRUE), .groups = "drop") %>%
    summarise(
      halfbroth_mean = mean(mean_halfbroth_od, na.rm = TRUE),
      halfbroth_drift = max(mean_halfbroth_od) - min(mean_halfbroth_od),
      halfbroth_cv = sd(mean_halfbroth_od) / mean(mean_halfbroth_od) * 100,
      halfbroth_range = paste(round(min(mean_halfbroth_od), 4), "-", round(max(mean_halfbroth_od), 4)),
      halfbroth_stability_rating = case_when(
        halfbroth_cv < 5 ~ "Excellent",
        halfbroth_cv < 10 ~ "Good", 
        halfbroth_cv < 20 ~ "Acceptable",
        TRUE ~ "Poor"
      )
    )
  
  # Blank stability
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
    halfbroth_stability = halfbroth_stability,  # NEW
    blank_stability = blank_stability,
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
  
  # Ensure complete_data columns are proper types
  complete_data <- complete_data %>%
    mutate(
      sample_type = as.character(sample_type),
      well_id = as.character(well_id)
    )
  
  # Safe sample distribution calculation
  sample_distribution <- tryCatch({
    complete_data %>% 
      distinct(well_id, sample_type) %>% 
      mutate(sample_type = as.character(sample_type)) %>%  # Ensure it's character
      count(sample_type, name = "n_wells")
  }, error = function(e) {
    cat("Warning: Could not create sample distribution, using fallback\n")
    tibble(sample_type = character(0), n_wells = integer(0))
  })
  
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
    sample_distribution = sample_distribution,
    layout_summary = layout_info$summary
  )
}

#' Quick diagnostic function to identify data issues
diagnose_data_issues <- function(corrected_results) {
  
  cat("=== DATA DIAGNOSTIC REPORT ===\n\n")
  
  issues <- list()
  
  # Clean the data first
  corrected_data <- clean_corrected_data(corrected_results$corrected_data)
  
  # Check untreated control values
  untreated_summary <- corrected_data %>%
    filter(sample_type == "untreated_control") %>%
    group_by(concentration) %>%
    summarise(
      mean_final_od = mean(od600_final[time_point == max(time_point)], na.rm = TRUE),
      .groups = "drop"
    )
  
  cat(" UNTREATED CONTROL SUMMARY:\n")
  print(untreated_summary)
  cat("\n")
  
  low_controls <- untreated_summary %>%
    filter(mean_final_od < 0.1)
  
  if (nrow(low_controls) > 0) {
    cat("️  LOW UNTREATED CONTROL VALUES (< 0.1 OD):\n")
    print(low_controls)
    cat("This may affect inhibition calculations\n\n")
    issues$low_controls <- low_controls
  }
  
  # Check for negative values with SAFE counting
  tryCatch({
    negative_ods <- corrected_data %>%
      filter(od600_final < 0) %>%
      # ENSURE columns are character vectors before counting
      mutate(
        sample_type = as.character(unlist(sample_type)),
        concentration = as.character(unlist(concentration))
      ) %>%
      count(sample_type, concentration, name = "n_negative") %>%
      arrange(desc(n_negative))
    
    if (nrow(negative_ods) > 0) {
      cat("️  NEGATIVE OD VALUES DETECTED:\n")
      print(negative_ods)
      cat("\n")
      issues$negative_ods <- negative_ods
    }
    
  }, error = function(e) {
    cat("️  Could not analyze negative values:", e$message, "\n")
    
    # Fallback: manual count
    neg_count <- sum(corrected_data$od600_final < 0, na.rm = TRUE)
    if (neg_count > 0) {
      cat("️  NEGATIVE VALUES DETECTED:", neg_count, "total negative values\n\n")
      issues$negative_count <- neg_count
    }
  })
  
  # High variability - get replicate stats if available
  if (!is.null(corrected_results$replicate_stats)) {
    high_cv <- corrected_results$replicate_stats %>%
      filter(sample_type == "sample") %>%
      mutate(cv = se_od / mean_od * 100) %>%
      filter(cv > 50, !is.na(cv), !is.infinite(cv)) %>%
      distinct(concentration) %>%
      arrange(concentration)
    
    if (nrow(high_cv) > 0) {
      cat("️  HIGH VARIABILITY CONCENTRATIONS (CV > 50%):\n")
      print(high_cv)
      cat("\n")
      issues$high_variability <- high_cv
    }
  }
  
  if (length(issues) == 0) {
    cat(" No major data issues detected!\n")
  }
  
  cat("==============================\n\n")
  
  return(issues)
}

#' Detailed layout verification function
#' Verify experimental layout function (UPDATED to handle multi-wavelength results)
verify_experimental_layout <- function(results) {
  
  cat("=== EXPERIMENTAL LAYOUT VERIFICATION ===\n\n")
  
  # Handle different result structures
  if ("wavelength" %in% names(results)) {
    # This is a single wavelength result from multi-wavelength analysis
    layout_data <- results$corrected_results$corrected_data
  } else if ("corrected_results" %in% names(results)) {
    # This is a standard single-wavelength result
    layout_data <- results$corrected_results$corrected_data
  } else if ("processed_data" %in% names(results)) {
    # Old structure
    if (!is.null(results$processed_data$data)) {
      layout_data <- results$processed_data$data
    } else if (!is.null(results$processed_data$corrected_data)) {
      layout_data <- results$processed_data$corrected_data
    } else {
      stop("Could not find data in processed_data object")
    }
  } else {
    stop("Could not find appropriate data in results object")
  }
  
  # Extract unique layout information
  layout_summary <- layout_data %>%
    distinct(well_id, sample_type, concentration, condition, replicate_id) %>%
    arrange(well_id)
  
  # Generate the visual layout
  layout_viz <- layout_summary %>%
    mutate(
      row_letter = str_extract(well_id, "^[A-H]"),
      col_number = as.numeric(str_extract(well_id, "\\d+$")),
      row_num = as.numeric(factor(row_letter, levels = LETTERS[1:8])),
      display_label = case_when(
        sample_type == "sample" ~ paste0("S", str_extract(concentration, "\\d+"), "\n", replicate_id),
        sample_type == "np_control" ~ paste0("NP", str_extract(concentration, "\\d+")),
        sample_type == "untreated_control" ~ paste0("UNT\n", well_id),
        sample_type == "blank" ~ "BLK",
        sample_type == "broth" ~ "BROTH",
        sample_type == "half_broth" ~ "1/2BRTH",  # ADD THIS LINE
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
      subtitle = "S# = Sample, NP# = NP control, UNT = Untreated, BLK = Blank, BROTH = Broth control",
      x = "Column",
      y = "Row",
      caption = "Verify this matches your experimental setup"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_blank()
    ) +
    coord_fixed()
  
  # Display the plot
  print(layout_viz)
  
  # Print summary
  cat("\n=== LAYOUT VALIDATION ===\n")
  
  # Sample replicates
  cat("Sample replicates by concentration:\n")
  sample_summary <- layout_summary %>%
    filter(sample_type == "sample") %>%
    select(concentration, replicate_id, well_id) %>%
    arrange(concentration, replicate_id)
  print(sample_summary)
  
  # Check for NP controls
  np_controls <- layout_summary %>%
    filter(sample_type == "np_control")
  
  if (nrow(np_controls) > 0) {
    cat("\nNP control wells:\n")
    np_summary <- np_controls %>%
      select(well_id, concentration) %>%
      arrange(well_id)
    print(np_summary)
  } else {
    cat("\nNo NP controls in this experiment\n")
  }
  
  # Untreated controls
  cat("\nUntreated control wells:\n")
  untreated_summary <- layout_summary %>%
    filter(sample_type == "untreated_control") %>%
    select(well_id) %>%
    arrange(well_id)
  print(untreated_summary)
  
  # Blank replicates
  cat("\nBlank wells:\n")
  blank_summary <- layout_summary %>%
    filter(sample_type == "blank") %>%
    select(well_id, replicate_id) %>%
    arrange(well_id)
  print(blank_summary)
  
  # Broth controls
  cat("\nBroth control wells:\n")
  broth_summary <- layout_summary %>%
    filter(sample_type == "broth") %>%
    select(well_id) %>%
    arrange(well_id)
  print(broth_summary)
  
  cat("\n=== VERIFICATION COMPLETE ===\n")
  cat("Please confirm this matches your actual experimental setup!\n\n")
  
  return(list(
    layout_data = layout_summary,
    plot = layout_viz,
    sample_summary = sample_summary,
    untreated_summary = untreated_summary,
    blank_summary = blank_summary,
    broth_summary = broth_summary
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
display_summaries <- function(summaries, layout_data = NULL) {
  
  cat("\n=== EXPERIMENT SUMMARIES ===\n\n")
  
  # 1. EXPERIMENTAL LAYOUT - VERIFY YOUR SETUP
  cat(" EXPERIMENTAL LAYOUT VERIFICATION:\n")
  cat("Please verify this matches your actual plate setup!\n")
  cat(strrep("=", 50), "\n")  # Fixed this line
  
  # Show the layout using actual data
  display_plate_layout(summaries, layout_data)
  
  # Layout summary table
  cat("\n LAYOUT SUMMARY TABLE:\n")
  if (!is.null(summaries$experiment$concentration_distribution)) {
    print(summaries$experiment$concentration_distribution)
  }
  cat("\n")
  
  # 2. Basic experiment info
  cat(" EXPERIMENT OVERVIEW:\n")
  print(summaries$experiment$basic_info)
  cat("\n")
  
  # 3. Sample distribution
  cat(" SAMPLE DISTRIBUTION:\n")
  print(summaries$experiment$sample_distribution)
  cat("\n")
  
  # 4. Top growth performers
  cat(" TOP GROWTH PERFORMERS:\n")
  top_growth <- summaries$growth_metrics$condition_metrics %>%
    filter(sample_type == "sample") %>%
    arrange(desc(mean_final_od)) %>%
    select(concentration, mean_final_od, se_final_od, cv_final_od) %>%
    slice_head(n = 10)
  print(top_growth, digits = 3)
  cat("\n")
  
  # 5. Concentration effects (if available)
  cat(" CONCENTRATION EFFECTS:\n")
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
  cat(" QUALITY CONTROL:\n")
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
  cat(" DATA DIMENSIONS:\n")
  cat("Total rows:", nrow(data), "\n")
  cat("Total wells:", length(unique(data$well_id)), "\n")
  cat("Timepoints:", length(unique(data$time_point)), "\n")
  cat("Time range:", round(min(data$time_hrs), 2), "to", round(max(data$time_hrs), 2), "hours\n")
  cat("\n")
  
  # Sample type breakdown
  cat(" SAMPLE TYPES:\n")
  sample_summary <- data %>%
    distinct(well_id, sample_type, concentration) %>%
    count(sample_type, name = "n_wells")
  print(sample_summary)
  cat("\n")
  
  # OD value ranges by sample type
  cat(" OD RANGES BY SAMPLE TYPE:\n")
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
  cat("️  POTENTIAL ISSUES:\n")
  
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
    cat("- No obvious data issues detected \n")
  }
  
  cat("\n=== END EXPLORATION ===\n")
}

# ===== VISUALIZATION FUNCTIONS =====

#' Create main growth curves plot with error bars
plot_growth_curves <- function(corrected_results, max_time_hrs = NULL) {
  
  # Apply time filter if specified
  if (!is.null(max_time_hrs)) {
    corrected_results$corrected_data <- corrected_results$corrected_data %>%
      filter(time_hrs <= max_time_hrs)
    corrected_results$replicate_stats <- corrected_results$replicate_stats %>%
      filter(time_hrs <= max_time_hrs)
    cat("• Applied time cutoff at", max_time_hrs, "hours\n")
  }
  
  # Detect the final OD column
  data <- corrected_results$corrected_data
  od_final_column <- detect_final_od_column(data)
  
  # Get sample replicate stats
  sample_stats <- corrected_results$replicate_stats %>%
    filter(sample_type == "sample")
  
  # Get untreated control raw data
  untreated_data <- corrected_results$corrected_data %>%
    filter(sample_type == "untreated_control") %>%
    rename(od_value = !!od_final_column)
  
  # Combine data for faceting
  # For samples, we'll create a simplified version from stats
  sample_plot_data <- sample_stats %>%
    select(time_hrs, sample_type, concentration, mean_od, se_od) %>%
    rename(od_value = mean_od, group_id = concentration) %>%
    mutate(data_type = "replicate_mean")
  
  # For untreated, each well is its own line
  untreated_plot_data <- untreated_data %>%
    select(time_hrs, sample_type, well_id, od_value) %>%
    rename(group_id = well_id) %>%
    mutate(data_type = "individual",
           se_od = 0)
  
  # Create the plot
  p <- ggplot() +
    # Sample data with error ribbons
    geom_ribbon(data = sample_plot_data,
                aes(x = time_hrs, 
                    ymin = od_value - se_od, 
                    ymax = od_value + se_od, 
                    fill = group_id),
                alpha = 0.2, show.legend = TRUE) +
    geom_line(data = sample_plot_data,
              aes(x = time_hrs, y = od_value, color = group_id),
              linewidth = 1, show.legend = TRUE) +
    # Untreated control data (individual lines, no legend)
    geom_line(data = untreated_plot_data,
              aes(x = time_hrs, y = od_value, group = group_id),
              color = "steelblue4", linewidth = 0.8, alpha = 0.7,
              show.legend = FALSE) +
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
    scale_color_viridis_d(option = "magma", begin = 0.1, end = 0.8) +
    scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.8)
  
  return(p)
}

#' Detect the final corrected OD column
detect_final_od_column <- function(data) {
  possible_columns <- c("od600_final", "od_final", "od600", "od")
  
  for (col in possible_columns) {
    if (col %in% names(data)) {
      return(col)
    }
  }
  
  # Look for any column with "final" in the name
  final_columns <- names(data)[str_detect(names(data), "final")]
  if (length(final_columns) > 0) {
    return(final_columns[1])
  }
  
  stop("Could not find final OD column in data")
}

#' Create growth curves plot for specific wavelength
plot_growth_curves_multi <- function(corrected_results, wavelength,
                                     max_time_hrs = NULL) {
  
  # Apply time filter if specified
  if (!is.null(max_time_hrs)) {
    corrected_results$corrected_data <- corrected_results$corrected_data %>%
      filter(time_hrs <= max_time_hrs)
    corrected_results$replicate_stats <- corrected_results$replicate_stats %>%
      filter(time_hrs <= max_time_hrs)
    cat("• Applied time cutoff at", max_time_hrs, "hours for", wavelength, "nm\n")
  }
  
  # Get sample replicate stats
  sample_stats <- corrected_results$replicate_stats %>%
    filter(sample_type == "sample")
  
  # Get untreated control raw data
  od_final_column <- paste0("od", wavelength, "_final")
  untreated_data <- corrected_results$corrected_data %>%
    filter(sample_type == "untreated_control") %>%
    rename(od_value = !!od_final_column)
  
  # Create combined plot data (similar structure to above)
  sample_plot_data <- sample_stats %>%
    select(time_hrs, sample_type, concentration, mean_od, se_od) %>%
    rename(od_value = mean_od, group_id = concentration) %>%
    mutate(data_type = "replicate_mean")
  
  untreated_plot_data <- untreated_data %>%
    select(time_hrs, sample_type, well_id, od_value) %>%
    rename(group_id = well_id) %>%
    mutate(data_type = "individual", se_od = 0)
  
  # Create the plot
  p <- ggplot() +
    # Sample data
    geom_ribbon(data = sample_plot_data,
                aes(x = time_hrs, ymin = od_value - se_od, ymax = od_value + se_od, 
                    fill = group_id),
                alpha = 0.2, show.legend = TRUE) +
    geom_line(data = sample_plot_data,
              aes(x = time_hrs, y = od_value, color = group_id),
              linewidth = 1, show.legend = TRUE) +
    # Untreated controls (no legend)
    geom_line(data = untreated_plot_data,
              aes(x = time_hrs, y = od_value, group = group_id),
              color = "steelblue4", linewidth = 0.8, alpha = 0.7,
              show.legend = FALSE) +
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
    scale_color_viridis_d(option = "magma", begin = 0.1, end = 0.8) +
    scale_fill_viridis_d(option = "magma", begin = 0.1,  end = 0.8)
  
  return(p)
}

#' Create single plate heatmap
plot_plate_heatmap <- function(corrected_results, timepoint = "last", 
                               data_type = "corrected") {
  
  # Determine timepoint with robust handling
  if (timepoint == "first") {
    target_timepoint <- 1
    time_label <- "Initial"
  } else if (timepoint == "last") {
    # UPDATED: Find the last timepoint that has data for most wells
    timepoint_coverage <- corrected_results$corrected_data %>%
      group_by(time_point) %>%
      summarise(n_wells = n_distinct(well_id), .groups = "drop") %>%
      arrange(desc(time_point))
    
    max_wells <- max(timepoint_coverage$n_wells)
    threshold_wells <- max_wells * 0.95
    
    suitable_timepoints <- timepoint_coverage %>%
      filter(n_wells >= threshold_wells)
    
    if (nrow(suitable_timepoints) > 0) {
      target_timepoint <- max(suitable_timepoints$time_point)
      cat("  • Using timepoint", target_timepoint, "as 'last' (", 
          suitable_timepoints$n_wells[1], "wells have data)\n")
    } else {
      target_timepoint <- max(corrected_results$corrected_data$time_point)
      cat("  • WARNING: Using max timepoint", target_timepoint, "\n")
    }
    time_label <- "Final"
  } else {
    target_timepoint <- timepoint
    time_label <- paste("Timepoint", timepoint)
  }
  
  # Select data column
  od_column <- detect_od_column_for_heatmap(corrected_results$corrected_data, data_type)
  
  # Get data for target timepoint
  timepoint_data <- corrected_results$corrected_data %>%
    filter(time_point == target_timepoint) %>%
    select(well_id, !!od_column)
  
  # Create complete wells grid
  complete_wells <- expand_grid(
    row_letter = LETTERS[1:8],
    col_number = 1:12
  ) %>%
    mutate(
      well_id = paste0(row_letter, sprintf("%02d", col_number)),
      row_num = as.numeric(factor(row_letter, levels = LETTERS[1:8]))
    )
  
  # UPDATED: Handle missing wells with carry-forward
  missing_wells <- setdiff(complete_wells$well_id, timepoint_data$well_id)
  
  if (length(missing_wells) > 0) {
    cat("  • Carrying forward data for", length(missing_wells), 
        "wells missing from timepoint", target_timepoint, "\n")
    
    carry_forward_data <- corrected_results$corrected_data %>%
      filter(well_id %in% missing_wells) %>%
      group_by(well_id) %>%
      filter(time_point == max(time_point)) %>%
      ungroup() %>%
      select(well_id, !!od_column)
    
    timepoint_data <- bind_rows(timepoint_data, carry_forward_data)
  }
  
  # Continue with existing heatmap logic...
  plot_data <- complete_wells %>%
    left_join(timepoint_data, by = "well_id") %>%
    mutate(
      od_value_raw = .data[[od_column]],
      od_value = case_when(
        is.na(od_value_raw) ~ NA_real_,
        od_value_raw < 0 ~ 0.001,
        TRUE ~ pmax(od_value_raw, 0)
      ),
      text_label = case_when(
        is.na(od_value_raw) ~ "NA",
        od_value_raw < -0.01 ~ paste0("(", round(od_value_raw, 2), ")"),
        od_value_raw < 0 ~ paste0("(-", abs(round(od_value_raw, 2)), ")"),
        od_value_raw < 0.001 ~ paste0("~", round(od_value_raw, 2)),
        TRUE ~ as.character(round(od_value_raw, 2))
      ),
      text_color = case_when(
        is.na(od_value_raw) ~ "red",
        od_value_raw < 0 ~ "skyblue", 
        od_value_raw < 0.001 ~ "burlywood",
        TRUE ~ "white"
      )
    )
  
  # Get summary for subtitle
  n_missing <- sum(is.na(plot_data$od_value_raw))
  n_negative <- sum(plot_data$od_value_raw < 0, na.rm = TRUE)
  n_very_low <- sum(plot_data$od_value_raw >= 0 & plot_data$od_value_raw < 0.001, na.rm = TRUE)
  
  actual_time <- corrected_results$corrected_data %>%
    filter(time_point == target_timepoint) %>%
    summarise(time = mean(time_hrs, na.rm = TRUE)) %>%
    pull(time)
  
  subtitle_parts <- c(
    paste("Time:", round(actual_time, 2), "hours"),
    str_to_title(str_replace_all(data_type, "_", " "))
  )
  
  issue_parts <- c()
  if (n_missing > 0) issue_parts <- c(issue_parts, paste(n_missing, "missing"))
  if (n_negative > 0) issue_parts <- c(issue_parts, paste(n_negative, "negative"))
  if (n_very_low > 0) issue_parts <- c(issue_parts, paste(n_very_low, "very low"))
  
  if (length(issue_parts) > 0) {
    subtitle_parts <- c(subtitle_parts, paste("Issues:", paste(issue_parts, collapse = ", ")))
  }
  
  subtitle <- paste(subtitle_parts, collapse = " | ")
  
  # Create heatmap
  p <- plot_data %>%
    ggplot(aes(x = col_number, y = 9 - row_num)) +
    geom_tile(aes(fill = od_value), color = "white", linewidth = 0.5) +
    geom_text(aes(label = text_label, color = text_color), size = 2.2) +
    scale_x_continuous(breaks = 1:12, expand = c(0,0)) +
    scale_y_continuous(breaks = 1:8, labels = LETTERS[8:1], expand = c(0,0)) +
    scale_fill_viridis_c(name = "OD600", end = 0.9, option = "magma", 
                         na.value = "gray70") +
    scale_color_identity() +
    labs(
      title = paste("Plate Layout -", time_label),
      subtitle = subtitle,
      x = "Column",
      y = "Row"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      plot.caption = element_text(size = 9, color = "gray50"),
      plot.subtitle = element_text(size = 10)
    ) +
    coord_fixed()
  
  return(p)
}

#' Clean and filter valid well data
clean_well_data <- function(data) {
  data %>%
    filter(
      # Only keep valid well IDs (A01-H12 format)
      str_detect(well_id, "^[A-H][0-9]{2}$"),
      # Remove any rows where well_id contains non-well identifiers
      !str_detect(well_id, "CONTROL|STANDARD|DEVIATION|AVERAGE|MEAN"),
      # Remove NA sample types that shouldn't be there
      !is.na(sample_type)
    )
}

#' Diagnose problematic wells in the dataset
diagnose_problematic_wells <- function(corrected_results, threshold_low = 0.001, threshold_negative = -0.01) {
  
  cat("=== WELL DIAGNOSTICS ===\n")
  
  # Clean the data first
  clean_data <- clean_well_data(corrected_results$corrected_data)
  
  # Check if any invalid wells were removed
  original_wells <- unique(corrected_results$corrected_data$well_id)
  clean_wells <- unique(clean_data$well_id)
  invalid_wells <- setdiff(original_wells, clean_wells)
  
  if (length(invalid_wells) > 0) {
    cat("️  Removed invalid well identifiers:\n")
    for (invalid_well in invalid_wells) {
      cat("   -", invalid_well, "\n")
    }
    cat("\n")
  }
  
  # Analyze final timepoint
  final_data <- clean_data %>%
    filter(time_point == max(time_point))
  
  # Identify problematic wells
  problematic_wells <- final_data %>%
    mutate(
      issue_type = case_when(
        is.na(od600_final) ~ "Missing Data",
        od600_final < threshold_negative ~ "Large Negative",
        od600_final < 0 ~ "Small Negative",
        od600_final < threshold_low ~ "Very Low Positive",
        TRUE ~ "Normal"
      ),
      # Add row and column info for better identification
      row_letter = str_extract(well_id, "^[A-H]"),
      col_number = as.numeric(str_extract(well_id, "\\d+$"))
    ) %>%
    filter(issue_type != "Normal") %>%
    arrange(issue_type, row_letter, col_number)
  
  if (nrow(problematic_wells) > 0) {
    cat("Problematic wells found:\n")
    
    issue_summary <- problematic_wells %>%
      count(issue_type, sample_type) %>%
      arrange(issue_type, sample_type)
    
    print(issue_summary)
    
    cat("\nDetailed list (with plate positions):\n")
    problem_details <- problematic_wells %>%
      select(well_id, row_letter, col_number, sample_type, concentration, od600_final, issue_type) %>%
      arrange(issue_type, row_letter, col_number)
    
    print(problem_details)
    
    # Show specific recommendations
    cat("\nRECOMMENDATIONS:\n")
    
    if (any(problematic_wells$issue_type == "Missing Data")) {
      cat("• Missing Data: Check if these wells were intentionally empty or if there's a data processing issue\n")
    }
    
    if (any(problematic_wells$issue_type %in% c("Large Negative", "Small Negative"))) {
      cat("• Negative Values: These may indicate over-correction or baseline issues\n")
      cat("  - Consider using method='traditional' with threshold=FALSE\n")
      cat("  - Check if blank/broth corrections are appropriate\n")
    }
    
    if (any(problematic_wells$issue_type == "Very Low Positive")) {
      cat("• Very Low Values: These are near the detection limit\n")
      cat("  - May indicate successful inhibition or technical issues\n")
      cat("  - Consider if these are expected based on your experimental design\n")
    }
    
  } else {
    cat(" No problematic wells detected at final timepoint\n")
  }
  
  # Check for NP controls
  np_controls <- clean_data %>%
    filter(sample_type == "np_control") %>%
    distinct(well_id)
  
  if (nrow(np_controls) == 0) {
    cat("\n NOTE: No NP (Material) controls detected in this experiment\n")
    cat("   This is normal for experiments without nanoparticle treatments\n")
  } else {
    cat("\n NP controls found in wells:", paste(np_controls$well_id, collapse = ", "), "\n")
  }
  
  return(invisible(problematic_wells))
}

#' Helper function to detect OD column for heatmaps
detect_od_column_for_heatmap <- function(data, data_type) {
  
  # Try standard column names first
  if (data_type == "raw") {
    candidates <- c("od600_raw", "od_raw", "od600", "od")
  } else if (data_type == "broth_corrected") {
    candidates <- c("od600_broth_corrected", "od_broth_corrected", "od600", "od")
  } else { # corrected
    candidates <- c("od600_final", "od_final", "od600", "od")
  }
  
  for (col in candidates) {
    if (col %in% names(data)) {
      return(col)
    }
  }
  
  # Fallback to first OD column found
  od_columns <- names(data)[str_detect(names(data), "od")]
  if (length(od_columns) > 0) {
    return(od_columns[1])
  }
  
  stop("Could not find appropriate OD column for data_type:", data_type)
}

#' Clean corrected data to ensure proper column types
clean_corrected_data <- function(corrected_data) {
  
  # Check for list columns and convert them
  list_columns <- sapply(corrected_data, function(x) any(sapply(x, is.list)))
  
  if (any(list_columns)) {
    cat("• Cleaning list columns in corrected data:", paste(names(list_columns)[list_columns], collapse = ", "), "\n")
    
    for (col_name in names(list_columns)[list_columns]) {
      # Handle different types of list columns
      if (col_name %in% c("time_hrs", "time_point", "temperature") || str_detect(col_name, "od\\d+")) {
        # Numeric columns
        corrected_data[[col_name]] <- as.numeric(unlist(corrected_data[[col_name]]))
      } else if (col_name == "time_point") {
        # Integer columns
        corrected_data[[col_name]] <- as.integer(unlist(corrected_data[[col_name]]))
      } else {
        # Character columns
        corrected_data[[col_name]] <- as.character(unlist(corrected_data[[col_name]]))
      }
    }
  }
  
  # Ensure key columns are proper types
  corrected_data <- corrected_data %>%
    mutate(
      time_hrs = as.numeric(time_hrs),
      time_point = as.integer(time_point),
      temperature = as.numeric(temperature),
      sample_type = as.character(sample_type),
      concentration = as.character(concentration),
      well_id = as.character(well_id)
    )
  
  # Clean any remaining OD columns
  od_columns <- names(corrected_data)[str_detect(names(corrected_data), "od\\d+")]
  for (od_col in od_columns) {
    corrected_data[[od_col]] <- as.numeric(corrected_data[[od_col]])
  }
  
  return(corrected_data)
}

#' Create plate heatmap for specific wavelength
#' Updated multi-wavelength plate heatmap with same fixes
plot_plate_heatmap_multi <- function(corrected_results, timepoint = "last", 
                                     data_type = "corrected", wavelength) {
  
  # Determine timepoint with robust handling
  if (timepoint == "first") {
    target_timepoint <- 1
    time_label <- "Initial"
  } else if (timepoint == "last") {
    # UPDATED: Find the last timepoint that has data for most wells
    timepoint_coverage <- corrected_results$corrected_data %>%
      group_by(time_point) %>%
      summarise(n_wells = n_distinct(well_id), .groups = "drop") %>%
      arrange(desc(time_point))
    
    # Use the most recent timepoint that has at least 95% of wells
    max_wells <- max(timepoint_coverage$n_wells)
    threshold_wells <- max_wells * 0.95
    
    suitable_timepoints <- timepoint_coverage %>%
      filter(n_wells >= threshold_wells)
    
    if (nrow(suitable_timepoints) > 0) {
      target_timepoint <- max(suitable_timepoints$time_point)
      cat("  • Using timepoint", target_timepoint, "as 'last' (", 
          suitable_timepoints$n_wells[1], "wells have data)\n")
    } else {
      # Fallback to max timepoint
      target_timepoint <- max(corrected_results$corrected_data$time_point)
      cat("  • WARNING: Using max timepoint", target_timepoint, 
          "but some wells may be missing\n")
    }
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
  
  # Create a complete grid of all wells
  complete_wells <- expand_grid(
    row_letter = LETTERS[1:8],
    col_number = 1:12
  ) %>%
    mutate(
      well_id = paste0(row_letter, sprintf("%02d", col_number)),
      row_num = as.numeric(factor(row_letter, levels = LETTERS[1:8]))
    )
  
  # Get data for the target timepoint
  timepoint_data <- corrected_results$corrected_data %>%
    filter(time_point == target_timepoint) %>%
    select(well_id, !!od_column)
  
  # UPDATED: For missing wells, use carry-forward from their last available timepoint
  missing_wells <- setdiff(complete_wells$well_id, timepoint_data$well_id)
  
  if (length(missing_wells) > 0) {
    cat("  • Carrying forward data for", length(missing_wells), 
        "wells missing from timepoint", target_timepoint, "\n")
    
    # Get the last available data for missing wells
    carry_forward_data <- corrected_results$corrected_data %>%
      filter(well_id %in% missing_wells) %>%
      group_by(well_id) %>%
      filter(time_point == max(time_point)) %>%  # Each well's maximum timepoint
      ungroup() %>%
      select(well_id, !!od_column)
    
    # Combine with original timepoint data
    timepoint_data <- bind_rows(timepoint_data, carry_forward_data)
    
    cat("  • Carried forward from timepoints:", 
        paste(unique(corrected_results$corrected_data %>%
                       filter(well_id %in% missing_wells) %>%
                       group_by(well_id) %>%
                       summarise(max_tp = max(time_point), .groups = "drop") %>%
                       pull(max_tp)), collapse = ", "), "\n")
  }
  
  # Join with complete grid
  plot_data <- complete_wells %>%
    left_join(timepoint_data, by = "well_id") %>%
    mutate(
      od_value_raw = .data[[od_column]],
      # Handle remaining NAs (true missing wells)
      od_value = case_when(
        is.na(od_value_raw) ~ NA_real_,  # Keep as NA for gray70 color
        od_value_raw < 0 ~ 0.001,
        TRUE ~ pmax(od_value_raw, 0)
      ),
      # Create text labels
      text_label = case_when(
        is.na(od_value_raw) ~ "NA",
        od_value_raw < -0.01 ~ paste0("(", round(od_value_raw, 2), ")"),
        od_value_raw < 0 ~ paste0("(-", abs(round(od_value_raw, 2)), ")"),
        od_value_raw < 0.001 ~ paste0("~", round(od_value_raw, 2)),
        TRUE ~ as.character(round(od_value_raw, 2))
      ),
      text_color = case_when(
        is.na(od_value_raw) ~ "red",
        od_value_raw < 0 ~ "skyblue",
        od_value_raw < 0.001 ~ "burlywood",
        TRUE ~ "white"
      )
    )
  
  # Get actual time for the timepoint
  actual_time <- corrected_results$corrected_data %>%
    filter(time_point == target_timepoint) %>%
    summarise(time = mean(time_hrs, na.rm = TRUE)) %>%
    pull(time)
  
  # Count issues
  n_missing <- sum(is.na(plot_data$od_value_raw))
  n_negative <- sum(plot_data$od_value_raw < 0, na.rm = TRUE)
  n_very_low <- sum(plot_data$od_value_raw >= 0 & plot_data$od_value_raw < 0.001, na.rm = TRUE)
  
  # Create subtitle
  subtitle_parts <- c(
    paste("Time:", round(actual_time, 2), "hours"),
    paste("Timepoint:", target_timepoint),
    str_to_title(str_replace_all(data_type, "_", " "))
  )
  
  issue_parts <- c()
  if (n_missing > 0) {
    issue_parts <- c(issue_parts, paste(n_missing, "missing"))
  }
  if (n_negative > 0) {
    issue_parts <- c(issue_parts, paste(n_negative, "negative"))
  }
  if (n_very_low > 0) {
    issue_parts <- c(issue_parts, paste(n_very_low, "very low"))
  }
  
  if (length(issue_parts) > 0) {
    subtitle_parts <- c(subtitle_parts, paste("Issues:", paste(issue_parts, collapse = ", ")))
  }
  
  subtitle <- paste(subtitle_parts, collapse = " | ")
  
  # Create heatmap
  p <- plot_data %>%
    ggplot(aes(x = col_number, y = 9 - row_num)) +
    geom_tile(aes(fill = od_value), color = "white", linewidth = 0.5) +
    geom_text(aes(label = text_label, color = text_color), size = 2.2) +
    scale_x_continuous(breaks = 1:12, expand = c(0,0)) +
    scale_y_continuous(breaks = 1:8, labels = LETTERS[8:1], expand = c(0,0)) +
    scale_fill_viridis_c(name = paste0("OD", wavelength), end = 0.9, option = "magma", 
                         na.value = "gray70") +  # Gray for true missing wells
    scale_color_identity() +
    labs(
      title = paste("Plate Layout -", time_label, "-", wavelength, "nm"),
      subtitle = subtitle,
      x = "Column",
      y = "Row"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      plot.caption = element_text(size = 9, color = "gray50"),
      plot.subtitle = element_text(size = 10)
    ) +
    coord_fixed()
  
  return(p)
}

#' Create composite plate heatmap with raw vs corrected comparison
#' @param corrected_results Output from process_with_corrections()
#' @return List of individual plots and combined 2x2 layout
#' Updated composite plate heatmap with layout verification
plot_plate_heatmap_composite <- function(corrected_results, 
                                         max_time_hrs = NULL) {
  
  # Apply time filter if specified
  if (!is.null(max_time_hrs)) {
    corrected_results$corrected_data <- corrected_results$corrected_data %>%
      filter(time_hrs <= max_time_hrs)
    if (!is.null(corrected_results$raw_data)) {
      corrected_results$raw_data <- corrected_results$raw_data %>%
        filter(time_hrs <= max_time_hrs)
    }
    cat("• Applied time cutoff at", max_time_hrs, "hours for composite heatmap\n")
  }
  
  # Create all four heatmaps using the updated plot_plate_heatmap function
  initial_raw <- plot_plate_heatmap(corrected_results, "first", "raw") +
    labs(title = "Initial - Raw Data")
  
  initial_corrected <- plot_plate_heatmap(corrected_results, "first", "corrected") +
    labs(title = "Initial - Corrected Data")
  
  final_raw <- plot_plate_heatmap(corrected_results, "last", "raw") +
    labs(title = "Final - Raw Data")
  
  final_corrected <- plot_plate_heatmap(corrected_results, "last", "corrected") +
    labs(title = "Final - Corrected Data")
  
  # Create SIMPLIFIED layout verification plot (no text)
  layout_verification <- tryCatch({
    if (!is.null(corrected_results$corrected_data)) {
      create_simplified_layout_plot(corrected_results$corrected_data)
    } else {
      # Fallback
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Layout Verification\nNot Available", 
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        labs(title = "Layout Verification")
    }
  }, error = function(e) {
    cat("• Warning: Could not create layout verification plot:", e$message, "\n")
    # Create a placeholder
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = "Layout Verification\nNot Available", 
               size = 6, hjust = 0.5, vjust = 0.5) +
      theme_void() +
      labs(title = "Layout Verification")
  })
  
  # Create the combined layout properly
  design <- "
    AABB
    CCDD
    #EE#
  "
  
  combined_with_layout <- initial_raw + final_raw + initial_corrected + final_corrected + layout_verification +
    plot_layout(design = design, heights = c(1, 1, 0.5))
  
  # Add overall title
  combined_with_layout <- combined_with_layout + 
    plot_annotation(
      title = "Plate Layout Analysis: Raw vs Corrected Data with Experimental Layout Verification",
      subtitle = "Top row: Raw data | Bottom row: Corrected data | Left: Initial | Right: Final | Bottom: Layout verification",
      theme = theme(plot.title = element_text(size = 16, face = "bold"),
                    plot.subtitle = element_text(size = 12))
    )
  
  # Also keep the simple 2x2 for backward compatibility
  combined_2x2 <- (initial_raw | final_raw) / (initial_corrected | final_corrected)
  
  combined_2x2 <- combined_2x2 + 
    plot_annotation(
      title = "Plate Layout Analysis: Raw vs Corrected Data",
      subtitle = "Top row: Raw data | Bottom row: Corrected data | Left: Initial | Right: Final",
      theme = theme(plot.title = element_text(size = 16, face = "bold"),
                    plot.subtitle = element_text(size = 12))
    )
  
  return(list(
    initial_raw = initial_raw,
    initial_corrected = initial_corrected,
    final_raw = final_raw,
    final_corrected = final_corrected,
    layout_verification = layout_verification,
    combined_2x2 = combined_2x2,
    combined_with_layout = combined_with_layout
  ))
}

#' Create composite plate heatmap for specific wavelength
plot_plate_heatmap_composite_multi <- function(corrected_results, wavelength,
                                               max_time_hrs = NULL) {
  
  # Apply time filter if specified
  if (!is.null(max_time_hrs)) {
    corrected_results$corrected_data <- corrected_results$corrected_data %>%
      filter(time_hrs <= max_time_hrs)
    if (!is.null(corrected_results$raw_data)) {
      corrected_results$raw_data <- corrected_results$raw_data %>%
        filter(time_hrs <= max_time_hrs)
    }
    cat("• Applied time cutoff at", max_time_hrs, "hours for", wavelength, "nm composite heatmap\n")
  }
  
  # Create all four heatmaps
  initial_raw <- plot_plate_heatmap_multi(corrected_results, "first", "raw", wavelength) +
    labs(title = paste("Initial -", wavelength, "nm - Raw Data"))
  
  initial_corrected <- plot_plate_heatmap_multi(corrected_results, "first", "corrected", wavelength) +
    labs(title = paste("Initial -", wavelength, "nm - Corrected Data"))
  
  final_raw <- plot_plate_heatmap_multi(corrected_results, "last", "raw", wavelength) +
    labs(title = paste("Final -", wavelength, "nm - Raw Data"))
  
  final_corrected <- plot_plate_heatmap_multi(corrected_results, "last", "corrected", wavelength) +
    labs(title = paste("Final -", wavelength, "nm - Corrected Data"))
  
  # Create SIMPLIFIED layout verification plot (no text)
  layout_verification <- tryCatch({
    if (!is.null(corrected_results$corrected_data)) {
      create_simplified_layout_plot(corrected_results$corrected_data)
    } else {
      # Fallback
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = paste0("Layout Verification\nNot Available\n(", wavelength, " nm)"), 
                 size = 6, hjust = 0.5, vjust = 0.5) +
        theme_void() +
        labs(title = "Layout Verification")
    }
  }, error = function(e) {
    cat("• Warning: Could not create layout verification plot:", e$message, "\n")
    # Create a placeholder
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste0("Layout Verification\nNot Available\n(", wavelength, " nm)"), 
               size = 6, hjust = 0.5, vjust = 0.5) +
      theme_void() +
      labs(title = "Layout Verification")
  })
  
  # Create the combined layout properly
  design <- "
    AABB
    CCDD
    #EE#
  "
  
  combined_with_layout <- initial_raw + final_raw + initial_corrected + final_corrected + layout_verification +
    plot_layout(design = design, heights = c(1, 1, 0.5))
  
  # Add overall title
  combined_with_layout <- combined_with_layout + 
    plot_annotation(
      title = paste("Plate Layout Analysis:", wavelength, "nm - Raw vs Corrected Data with Layout Verification"),
      subtitle = "Top row: Raw data | Bottom row: Corrected data | Left: Initial | Right: Final | Bottom: Layout verification",
      theme = theme(plot.title = element_text(size = 16, face = "bold"),
                    plot.subtitle = element_text(size = 12))
    )
  
  # Also keep the simple 2x2 for backward compatibility
  combined_2x2 <- (initial_raw | final_raw) / (initial_corrected | final_corrected)
  
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
    layout_verification = layout_verification,
    combined_2x2 = combined_2x2,
    combined_with_layout = combined_with_layout
  ))
}

#' Plot Growth Curves for a Specific Concentration
#'
#' This function generates a detailed growth curve plot for a specified concentration,
#' with options to include untreated controls and choose from multiple visualization styles.
#'
#' @param results A list returned by `analyze_growth_curves()` or one of its wrappers.
#' @param concentration Character. The concentration to plot (e.g., `"10"` or `"50"`).
#' @param include_untreated Logical. If `TRUE`, includes untreated control data in the plot. Default is `TRUE`.
#' @param data_type Character. Specifies which OD data to use. Options are `"raw"`, `"broth_corrected"`, or `"corrected"` (default).
#' @param style Character. Plotting style. Options include:
#'   - `"default"`: Clean lines and points with color/shape by sample type and replicate.
#'   - `"jittered"`: Adds jitter to reduce overplotting.
#'   - `"separated"`: Facets by sample type.
#'   - `"with_ribbons"`: Adds mean ± standard error ribbons.
#'
#' @return A `ggplot` object representing the growth curve for the specified concentration.
#'
#' @details
#' - Internally uses `get_concentration_data()` to extract and format the data.
#' - Supports multiple replicates and sample types.
#' - Automatically adjusts aesthetics based on the selected style.
#'
#' @seealso [get_concentration_data()], [ggplot2::ggplot()]
#'
#' @examples
#' \dontrun{
#' results <- analyze_growth_curves("data/plate_reader_output.csv")
#' plot_concentration(results, concentration = "10")
#' plot_concentration(results, concentration = "25", style = "with_ribbons")
#' }
#'
#' @export
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

#' Generate Panel Plot of Growth Curves Across Concentrations
#'
#' This function creates a faceted panel plot of growth curves across multiple concentrations,
#' supporting both single and multi-wavelength results. It allows filtering by sample type,
#' selection of data type, and control over y-axis scaling.
#'
#' @param results A list returned by `analyze_growth_curves()` or one of its wrappers.
#' @param concentrations Optional. A character vector of concentrations to include. If `NULL`, all available concentrations are shown.
#' @param sample_types Character vector. Sample types to include (e.g., `"sample"`, `"untreated_control"`, or both). Default is `"sample"`.
#' @param data_type Character. Specifies which OD data to use. Options are `"raw"`, `"broth_corrected"`, or `"corrected"` (default).
#' @param free_scale Logical. If `TRUE`, allows each facet to have its own y-axis scale. Default is `FALSE`.
#' @param wavelength Optional. Character string specifying the wavelength to use (e.g., `"600"`). Required for multi-wavelength results.
#'
#' @return A `ggplot` object showing growth curves across concentrations in a faceted layout, or `NULL` if no wavelength is specified in a multi-wavelength result.
#'
#' @details
#' - Automatically detects whether the input contains single or multiple wavelengths.
#' - If `wavelength` is not specified for multi-wavelength results, the function prompts the user.
#' - Internally calls `plot_concentrations_panel_single()` for plotting.
#'
#' @seealso [show_concentrations_panel()], [plot_concentration()]
#'
#' @examples
#' \dontrun{
#' results <- analyze_growth_curves("data/plate_reader_output.csv")
#' plot_concentrations_panel(results, sample_types = c("sample", "untreated_control"))
#' plot_concentrations_panel(results, wavelength = "600", free_scale = TRUE)
#' }
#'
#' @export
plot_concentrations_panel <- function(results, concentrations = NULL, 
                                      sample_types = "sample", data_type = "corrected",
                                      free_scale = FALSE, wavelength = NULL,
                                      max_time_hrs = NULL) {
  
  # Detect if this is multi-wavelength results
  if ("wavelength_600" %in% names(results) || "wavelength_420" %in% names(results)) {
    
    wavelength_keys <- names(results)[str_detect(names(results), "^wavelength_")]
    available_wavelengths <- str_extract(wavelength_keys, "\\d+")
    
    if (is.null(wavelength)) {
      cat("• Multi-wavelength results detected. Specify wavelength:\n")
      for (wl in available_wavelengths) {
        cat("  plot_concentrations_panel(results, wavelength = '", wl, "')  # ", wl, " nm\n")
      }
      return(invisible())
      
    } else {
      # Specific wavelength requested
      wavelength_key <- paste0("wavelength_", wavelength)
      
      if (!wavelength_key %in% names(results)) {
        stop("Wavelength ", wavelength, " not found. Available: ", 
             paste(available_wavelengths, collapse = ", "))
      }
      
      # FIXED: Pass the entire single wavelength result, not just corrected_results
      single_result <- results[[wavelength_key]]
      return(plot_concentrations_panel_single(single_result, concentrations, 
                                              sample_types, data_type, free_scale, wavelength, max_time_hrs))
    }
    
  } else {
    # Single wavelength results - pass the entire results object
    return(plot_concentrations_panel_single(results, concentrations, 
                                            sample_types, data_type, free_scale, NULL, max_time_hrs))
  }
}

#' Core concentration panel plotting function
plot_concentrations_panel_single <- function(corrected_results, concentrations = NULL, 
                                             sample_types = "sample", data_type = "corrected",
                                             free_scale = FALSE, wavelength = NULL,
                                             max_time_hrs = NULL) {
  
  # FIXED: Handle different result structures
  if (is.null(corrected_results$corrected_data)) {
    # This might be a full result object, try to extract corrected_results
    if (!is.null(corrected_results$corrected_results)) {
      actual_corrected_results <- corrected_results$corrected_results
    } else {
      stop("Could not find corrected_data in the provided results structure")
    }
  } else {
    actual_corrected_results <- corrected_results
  }
  
  # Apply time filter if specified
  if (!is.null(max_time_hrs)) {
    actual_corrected_results$corrected_data <- actual_corrected_results$corrected_data %>%
      filter(time_hrs <= max_time_hrs)
    cat("• Applied time cutoff at", max_time_hrs, "hours\n")
  }
  
  # Detect wavelength if not provided
  if (is.null(wavelength)) {
    data <- actual_corrected_results$corrected_data
    od_columns <- names(data)[str_detect(names(data), "^od\\d+")]
    if (length(od_columns) > 0) {
      wavelength <- str_extract(od_columns[1], "\\d+")
    } else {
      wavelength <- "600"  # Default fallback
    }
  }
  
  # Select the appropriate OD column
  od_column <- case_when(
    data_type == "raw" ~ paste0("od", wavelength, "_raw"),
    data_type == "broth_corrected" ~ paste0("od", wavelength, "_broth_corrected"), 
    data_type == "corrected" ~ paste0("od", wavelength, "_final"),
    TRUE ~ paste0("od", wavelength, "_final")
  )
  
  # Check if the OD column exists, try alternatives
  available_od_columns <- names(actual_corrected_results$corrected_data)[str_detect(names(actual_corrected_results$corrected_data), "^od")]
  
  if (!od_column %in% names(actual_corrected_results$corrected_data)) {
    # Try standard single-wavelength column names
    if ("od600_final" %in% names(actual_corrected_results$corrected_data)) {
      od_column <- "od600_final"
      cat("• Using standard single-wavelength column: od600_final\n")
    } else if ("od_final" %in% names(actual_corrected_results$corrected_data)) {
      od_column <- "od_final"
      cat("• Using standard single-wavelength column: od_final\n")
    } else if (length(available_od_columns) > 0) {
      od_column <- available_od_columns[1]
      cat("• Using first available OD column:", od_column, "\n")
    } else {
      stop("Could not find appropriate OD column. Available columns: ", 
           paste(names(actual_corrected_results$corrected_data), collapse = ", "))
    }
  }
  
  # Get available concentrations in numerical order
  available_concs <- actual_corrected_results$corrected_data %>%
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
  plot_data <- actual_corrected_results$corrected_data %>%
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
      subtitle = paste("Sample types:", paste(sample_types, collapse = ", "), "|", scale_subtitle, 
                       if(!is.null(wavelength)) paste("|", wavelength, "nm") else ""),
      x = "Time (hours)",
      y = paste0("OD", if(!is.null(wavelength)) wavelength else "600"),
      color = "Sample Type",
      fill = "Sample Type"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    ) +
    scale_color_viridis_d(option = "magma", begin = 0.1, end = 0.8) +
    scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.8)
  
  return(p)
}

#' Display Panel Plot of Growth Curves Across Concentrations
#'
#' This function displays a faceted panel plot of growth curves across multiple concentrations.
#' It acts as a user-friendly wrapper around `plot_concentrations_panel()` and supports
#' filtering by sample type and wavelength.
#'
#' @param results A list returned by `analyze_growth_curves()` or one of its wrappers.
#' @param concentrations Optional. A character vector of concentrations to include. If `NULL`, all available concentrations are shown.
#' @param sample_types Character vector. Sample types to include (e.g., `"sample"`, `"untreated_control"`, or both). Default is `"sample"`.
#' @param free_scale Logical. If `TRUE`, allows each facet to have its own y-axis scale. Default is `FALSE`.
#' @param wavelength Optional. Character string specifying the wavelength to use (e.g., `"600"`). Only needed for multi-wavelength results.
#'
#' @return A `ggplot` object showing growth curves across concentrations in a faceted layout.
#'
#' @seealso [plot_concentrations_panel()], [plot_concentration()]
#'
#' @examples
#' \dontrun{
#' results <- analyze_growth_curves("data/plate_reader_output.csv")
#' show_concentrations_panel(results)
#' show_concentrations_panel(results, concentrations = c("10", "25", "50"), free_scale = TRUE)
#' }
#'
#' @export
show_concentrations_panel <- function(results, concentrations = NULL, 
                                      sample_types = "sample", free_scale = FALSE, 
                                      wavelength = NULL, 
                                      max_time_hrs = NULL) {
  p <- plot_concentrations_panel(results, concentrations, sample_types, 
                                 "corrected", free_scale, wavelength, max_time_hrs)  # PASS PARAMETER
  if (!is.null(p)) print(p)
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
    scale_color_viridis_d(option = "magma", begin = 0.1, end = 0.8) +
    scale_fill_viridis_d(option = "magma", begin = 0.1, end = 0.8)
  
  return(p)
}

#' Display plate layout in visual format
display_plate_layout <- function(summaries, layout_data = NULL) {
  
  if (is.null(layout_data)) {
    # Try to get layout from summaries if available
    if ("layout" %in% names(summaries)) {
      layout_data <- summaries$layout
    } else {
      # Fallback to old hardcoded display
      display_plate_layout_hardcoded()
      return()
    }
  }
  
  # Create a visual representation of the actual plate
  cat("    ", sprintf("%2d ", 1:12), "\n")
  cat("   ", strrep("-", 36), "\n")
  
  rows <- LETTERS[1:8]
  
  for (i in 1:8) {
    row_letter <- rows[i]
    cat(row_letter, " |")
    
    for (j in 1:12) {
      well_id <- paste0(row_letter, sprintf("%02d", j))
      
      # Get actual content from layout data
      well_info <- layout_data %>%
        filter(well_id == !!well_id) %>%
        slice(1)
      
      if (nrow(well_info) > 0) {
        content <- case_when(
          well_info$sample_type == "sample" ~ " S ",
          well_info$sample_type == "np_control" ~ " N ",
          well_info$sample_type == "untreated_control" ~ " U ",
          well_info$sample_type == "broth" ~ " B ",
          well_info$sample_type == "blank" ~ " K ",  # K for blanK to distinguish from Broth
          TRUE ~ " ? "
        )
      } else {
        content <- " ? "
      }
      
      cat(content)
    }
    cat("|\n")
  }
  
  cat("   ", strrep("-", 36), "\n")
  cat("\nLEGEND (based on your actual layout):\n")
  
  # Generate legend from actual data
  legend_info <- layout_data %>%
    distinct(sample_type) %>%
    arrange(sample_type) %>%
    mutate(
      symbol = case_when(
        sample_type == "sample" ~ "S",
        sample_type == "np_control" ~ "N", 
        sample_type == "untreated_control" ~ "U",
        sample_type == "broth" ~ "B",
        sample_type == "blank" ~ "K",
        TRUE ~ "?"
      ),
      description = case_when(
        sample_type == "sample" ~ "Sample (Bacteria + NP)",
        sample_type == "np_control" ~ "NP-only control",
        sample_type == "untreated_control" ~ "Untreated control",
        sample_type == "broth" ~ "Broth control",
        sample_type == "blank" ~ "Blank control",
        TRUE ~ "Unknown"
      )
    )
  
  for (i in 1:nrow(legend_info)) {
    cat(legend_info$symbol[i], "=", legend_info$description[i], "\n")
  }
  
  # Show concentration info
  conc_info <- layout_data %>%
    filter(sample_type %in% c("sample", "np_control"), !is.na(concentration)) %>%
    distinct(concentration) %>%
    arrange(concentration)
  
  if (nrow(conc_info) > 0) {
    cat("\nConcentrations:", paste(conc_info$concentration, collapse = ", "), "\n")
  }
}

#' Fallback hardcoded version
display_plate_layout_hardcoded <- function() {
  cat("    ", sprintf("%2d ", 1:12), "\n")
  cat("   ", strrep("-", 36), "\n")
  
  rows <- LETTERS[1:8]
  
  for (i in 1:8) {
    row_letter <- rows[i]
    cat(row_letter, " |")
    
    for (j in 1:12) {
      well_id <- paste0(row_letter, sprintf("%02d", j))
      
      # Determine what's in this well based on default layout
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
  
  cat("   ", strrep("-", 36), "\n")
  cat("\nLEGEND (default layout):\n")
  cat("B = Broth blank\n")
  cat("S = Sample (A=Rep1, B=Rep2, C=Rep3)\n") 
  cat("N = NP-only control\n")
  cat("U = Untreated control\n")
  cat("Columns 2-11 = Concentrations 1-10\n")
}


#' Plot Temperature Profile from Growth Curve Data
#'
#' Creates a comprehensive temperature plot with optional quality control checks
#' and statistical summaries. Works with both single and multi-wavelength results.
#'
#' @param results A list returned by analyze_growth_curves() or one of its wrappers
#' @param wavelength Character. For multi-wavelength data, specify which wavelength 
#'   to use (e.g., "600"). If NULL, prompts user or uses first available.
#' @param data_type Character. Which data to use: "raw" (default), "corrected", or "both"
#' @param style Character. Plot style: "detailed" (default), "line", "smooth", or "ribbon"
#' @param show_target Numeric. If provided, shows a target temperature line (e.g., 37)
#' @param time_range Numeric vector of length 2. Time range to plot (e.g., c(0, 16))
#' @param qc_checks Logical. If TRUE, adds quality control annotations
#'
#' @return A ggplot object showing temperature over time
#'
#' @examples
#' \dontrun{
#' # Basic temperature plot
#' plot_temperature(results)
#' 
#' # With target temperature and quality checks
#' plot_temperature(results, show_target = 37, qc_checks = TRUE)
#' 
#' # Smooth style with ribbon showing variation
#' plot_temperature(results, style = "ribbon")
#' 
#' # Compare raw vs corrected data temperature readings
#' plot_temperature(results, data_type = "both")
#' }
#'
#' @export
plot_temperature <- function(results, wavelength = NULL, data_type = "raw",
                             style = "detailed", show_target = NULL, 
                             time_range = NULL, qc_checks = FALSE,
                             max_time_hrs = NULL) {
  
  cat("️  Creating temperature profile plot...\n")
  
  # Apply max_time_hrs if specified and time_range is not already set
  if (!is.null(max_time_hrs) && is.null(time_range)) {
    time_range <- c(0, max_time_hrs)
    cat("• Applied max_time_hrs filter:", max_time_hrs, "hours\n")
  }
  
  # Handle multi-wavelength results
  if ("wavelength_600" %in% names(results) || "wavelength_420" %in% names(results)) {
    wavelength_keys <- names(results)[str_detect(names(results), "^wavelength_")]
    available_wavelengths <- str_extract(wavelength_keys, "\\d+")
    
    if (is.null(wavelength)) {
      if (length(wavelength_keys) == 1) {
        # Only one wavelength, use it
        wavelength <- available_wavelengths[1]
        cat("• Using", wavelength, "nm data\n")
        data <- results[[wavelength_keys[1]]]$corrected_results$corrected_data
      } else {
        cat("• Multi-wavelength data detected. Specify wavelength:\n")
        for (wl in available_wavelengths) {
          cat("  plot_temperature(results, wavelength = '", wl, "')\n")
        }
        return(invisible())
      }
    } else {
      wavelength_key <- paste0("wavelength_", wavelength)
      if (!wavelength_key %in% names(results)) {
        stop("Wavelength ", wavelength, " not found. Available: ", 
             paste(available_wavelengths, collapse = ", "))
      }
      data <- results[[wavelength_key]]$corrected_results$corrected_data
    }
  } else {
    # Single wavelength results
    data <- results$corrected_results$corrected_data
    wavelength <- "600"  # Assume 600nm for labeling
  }
  
  # Apply time range filter if specified
  if (!is.null(time_range)) {
    data <- data %>%
      filter(time_hrs >= time_range[1], time_hrs <= time_range[2])
    cat("• Applied time range:", time_range[1], "to", time_range[2], "hours\n")
  }
  
  # Prepare plot data based on data_type
  if (data_type == "both") {
    # Get both raw and corrected data
    raw_data <- if ("corrected_results" %in% names(results)) {
      results$corrected_results$raw_data
    } else {
      data  # Use corrected_data as fallback
    }
    
    if (!is.null(time_range)) {
      raw_data <- raw_data %>%
        filter(time_hrs >= time_range[1], time_hrs <= time_range[2])
    }
    
    plot_data <- bind_rows(
      raw_data %>% 
        select(time_hrs, temperature, well_id) %>%
        mutate(data_source = "Raw Data"),
      data %>% 
        select(time_hrs, temperature, well_id) %>%
        mutate(data_source = "Corrected Data")
    )
    
    group_var <- "data_source"
    color_var <- "data_source"
    
  } else {
    # Use single data source
    if (data_type == "raw" && "corrected_results" %in% names(results) && 
        !is.null(results$corrected_results$raw_data)) {
      plot_data <- results$corrected_results$raw_data %>%
        select(time_hrs, temperature, well_id) %>%
        mutate(data_source = "Raw Data")
      
      if (!is.null(time_range)) {
        plot_data <- plot_data %>%
          filter(time_hrs >= time_range[1], time_hrs <= time_range[2])
      }
    } else {
      plot_data <- data %>%
        select(time_hrs, temperature, well_id) %>%
        mutate(data_source = str_to_title(str_replace_all(data_type, "_", " ")))
    }
    
    group_var <- NULL
    color_var <- NULL
  }
  
  # Calculate temperature statistics
  temp_stats <- plot_data %>%
    summarise(
      mean_temp = mean(temperature, na.rm = TRUE),
      min_temp = min(temperature, na.rm = TRUE),
      max_temp = max(temperature, na.rm = TRUE),
      sd_temp = sd(temperature, na.rm = TRUE),
      range_temp = max_temp - min_temp,
      cv_temp = sd_temp / mean_temp * 100
    )
  
  cat("• Temperature statistics:\n")
  cat("  Range:", round(temp_stats$min_temp, 1), "to", 
      round(temp_stats$max_temp, 1), "C\n")
  cat("  Mean:", round(temp_stats$mean_temp, 1), "±", 
      round(temp_stats$sd_temp, 2), "C\n")
  cat("  Variation (CV):", round(temp_stats$cv_temp, 2), "%\n")
  
  # Create base plot based on style
  if (style == "line") {
    if (is.null(color_var)) {
      p <- ggplot(plot_data, aes(x = time_hrs, y = temperature)) +
        geom_line(alpha = 0.3, aes(group = well_id)) +
        stat_summary(fun = mean, geom = "line", color = "firebrick", linewidth = 1.2)
    } else {
      p <- ggplot(plot_data, aes(x = time_hrs, y = temperature, color = .data[[color_var]])) +
        geom_line(alpha = 0.3, aes(group = interaction(well_id, .data[[color_var]]))) +
        stat_summary(fun = mean, geom = "line", linewidth = 1.2)
    }
    
  } else if (style == "smooth") {
    if (is.null(color_var)) {
      p <- ggplot(plot_data, aes(x = time_hrs, y = temperature)) +
        geom_point(alpha = 0.1, size = 0.5) +
        geom_smooth(method = "loess", color = "firebrick", linewidth = 1.5, se = TRUE)
    } else {
      p <- ggplot(plot_data, aes(x = time_hrs, y = temperature, color = .data[[color_var]])) +
        geom_point(alpha = 0.1, size = 0.5) +
        geom_smooth(method = "loess", linewidth = 1.5, se = TRUE)
    }
    
  } else if (style == "ribbon") {
    # Create temperature summary with proper grouping
    if (is.null(group_var)) {
      # Single data source
      temp_summary <- plot_data %>%
        group_by(time_hrs) %>%
        summarise(
          mean_temp = mean(temperature, na.rm = TRUE),
          sd_temp = sd(temperature, na.rm = TRUE),
          min_temp = min(temperature, na.rm = TRUE),
          max_temp = max(temperature, na.rm = TRUE),
          .groups = "drop"
        )
      
      p <- ggplot(temp_summary, aes(x = time_hrs, y = mean_temp)) +
        geom_ribbon(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), 
                    alpha = 0.3, fill = "firebrick") +
        geom_line(color = "firebrick", linewidth = 1.2)
      
    } else {
      # Multiple data sources - group properly
      temp_summary <- plot_data %>%
        group_by(time_hrs, data_source) %>%  # Use actual column name
        summarise(
          mean_temp = mean(temperature, na.rm = TRUE),
          sd_temp = sd(temperature, na.rm = TRUE),
          min_temp = min(temperature, na.rm = TRUE),
          max_temp = max(temperature, na.rm = TRUE),
          .groups = "drop"
        )
      
      p <- ggplot(temp_summary, aes(x = time_hrs, y = mean_temp, 
                                    color = data_source, fill = data_source)) +
        geom_ribbon(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp), 
                    alpha = 0.2) +
        geom_line(linewidth = 1.2)
    }
  } else if (style == "detailed") {
    # Show individual well traces with summary statistics
    temp_summary <- plot_data %>%
      group_by(time_hrs) %>%
      summarise(
        mean_temp = mean(temperature, na.rm = TRUE),
        median_temp = median(temperature, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot() +
      geom_line(data = plot_data, aes(x = time_hrs, y = temperature, group = well_id), 
                alpha = 0.15, color = "gray40") +
      geom_line(data = temp_summary, aes(x = time_hrs, y = mean_temp), 
                color = "firebrick", linewidth = 1.2, linetype = "solid") +
      geom_line(data = temp_summary, aes(x = time_hrs, y = median_temp), 
                color = "steelblue", linewidth = 1.2, linetype = "dashed") +
      annotate("text", x = Inf, y = Inf, 
               label = paste("Mean (red solid)\nMedian (blue dashed)"), 
               hjust = 1.05, vjust = 1.05, size = 3, color = "black")
  } else {
    stop("Style must be one of: 'line', 'smooth', 'ribbon', 'detailed'")
  }
  
  # Add target temperature line if specified
  if (!is.null(show_target)) {
    p <- p + 
      geom_hline(yintercept = show_target, color = "gray43", linewidth = 1, 
                 linetype = "dashed", alpha = 0.7) +
      annotate("text", x = Inf, y = show_target, 
               label = paste("Target:", show_target, "C"), 
               hjust = 1.05, vjust = -0.5, size = 3, color = "gray43")
  }
  
  # Add quality control annotations if requested
  if (qc_checks) {
    # Flag potential issues
    qc_annotations <- c()
    
    if (temp_stats$range_temp > 2.0) {
      qc_annotations <- c(qc_annotations, 
                          paste("High variation (", round(temp_stats$range_temp, 1), "C range)"))
    }
    
    if (temp_stats$cv_temp > 2.0) {
      qc_annotations <- c(qc_annotations, 
                          paste("High CV (", round(temp_stats$cv_temp, 1), "%)"))
    }
    
    if (!is.null(show_target)) {
      temp_deviation <- abs(temp_stats$mean_temp - show_target)
      if (temp_deviation > 1.0) {
        qc_annotations <- c(qc_annotations, 
                            paste("Off target by", round(temp_deviation, 1), "C"))
      }
    }
    
    if (length(qc_annotations) > 0) {
      qc_text <- paste("QC Issues:", paste(qc_annotations, collapse = "; "))
      p <- p + 
        annotate("text", x = -Inf, y = Inf, label = qc_text, 
                 hjust = -0.05, vjust = 1.05, size = 3, color = "firebrick",
                 fontface = "bold")
    } else {
      p <- p + 
        annotate("text", x = -Inf, y = Inf, label = "QC: Temperature stable ", 
                 hjust = -0.05, vjust = 1.05, size = 3, color = "gray43",
                 fontface = "bold")
    }
  }
  
  # Finalize plot
  p <- p +
    labs(
      title = paste("Temperature Profile -", 
                    if (!is.null(wavelength)) paste(wavelength, "nm") else "Growth Curve Data"),
      subtitle = paste("Range:", round(temp_stats$min_temp, 1), "to", 
                       round(temp_stats$max_temp, 1), "C |",
                       "Mean:", round(temp_stats$mean_temp, 1), "C |",
                       "CV:", round(temp_stats$cv_temp, 1), "%"),
      x = "Time (hours)",
      y = "Temperature (C)",
      color = if (!is.null(color_var)) str_to_title(str_replace_all(color_var, "_", " ")) else NULL,
      fill = if (!is.null(color_var)) str_to_title(str_replace_all(color_var, "_", " ")) else NULL
    ) +
    theme_minimal() +
    theme(
      legend.position = if (!is.null(color_var)) "bottom" else "none",
      panel.grid.minor = element_blank()
    )
  
  # Add color scales if needed
  if (!is.null(color_var)) {
    p <- p +
      scale_color_viridis_d(option = "plasma", begin = 0.2, end = 0.8) +
      scale_fill_viridis_d(option = "plasma", begin = 0.2, end = 0.8)
  }
  
  cat(" Temperature plot created\n")
  return(p)
}

#' Quick temperature plot wrapper
#' @param results Analysis results
#' @param ... Additional arguments passed to plot_temperature()
#' @export
show_temperature <- function(results, max_time_hrs = NULL, ...) {  # NEW PARAMETER
  p <- plot_temperature(results, max_time_hrs = max_time_hrs, ...)
  print(p)
  return(p)
}

#' Quick temperature check - just prints statistics
#' @param results Analysis results
#' @param wavelength Character. Wavelength to check for multi-wavelength data
#' @export
check_temperature <- function(results, wavelength = NULL) {
  
  # Handle multi-wavelength
  if ("wavelength_600" %in% names(results) || "wavelength_420" %in% names(results)) {
    wavelength_keys <- names(results)[str_detect(names(results), "^wavelength_")]
    available_wavelengths <- str_extract(wavelength_keys, "\\d+")
    
    if (is.null(wavelength)) {
      if (length(wavelength_keys) == 1) {
        data <- results[[wavelength_keys[1]]]$corrected_results$corrected_data
      } else {
        cat("Multi-wavelength data - specify wavelength:", paste(available_wavelengths, collapse = ", "), "\n")
        return(invisible())
      }
    } else {
      wavelength_key <- paste0("wavelength_", wavelength)
      data <- results[[wavelength_key]]$corrected_results$corrected_data
    }
  } else {
    data <- results$corrected_results$corrected_data
  }
  
  temp_check <- data %>%
    summarise(
      min_temp = min(temperature, na.rm = TRUE),
      max_temp = max(temperature, na.rm = TRUE),
      mean_temp = mean(temperature, na.rm = TRUE),
      sd_temp = sd(temperature, na.rm = TRUE),
      range_temp = max_temp - min_temp,
      cv_temp = sd_temp / mean_temp * 100,
      n_readings = n()
    )
  
  cat("️  TEMPERATURE CHECK\n")
  cat("====================\n")
  cat("Range:", round(temp_check$min_temp, 1), "to", round(temp_check$max_temp, 1), "C\n")
  cat("Mean:", round(temp_check$mean_temp, 1), "±", round(temp_check$sd_temp, 2), "C\n")
  cat("Variation:", round(temp_check$cv_temp, 2), "% CV\n")
  cat("Readings:", temp_check$n_readings, "\n")
  
  # Quality assessment
  if (temp_check$cv_temp < 1.0) {
    cat(" Excellent temperature stability\n")
  } else if (temp_check$cv_temp < 2.0) {
    cat(" Good temperature stability\n")
  } else if (temp_check$cv_temp < 5.0) {
    cat("️  Moderate temperature variation\n")
  } else {
    cat(" High temperature variation - check instrument\n")
  }
  
  return(invisible(temp_check))
}

#' Create quality control plots
create_qc_plots <- function(corrected_results, processed_data = NULL) {
  
  qc_plots <- list(
    broth_contamination = create_broth_contamination_plot(corrected_results),
    np_kinetics = create_np_kinetics_plot(corrected_results),
    before_after = create_before_after_plot(corrected_results),
    temperature = plot_temperature(list(corrected_results = corrected_results), style = "detailed")  # NEW
  )
  
  # Add layout validation plot if processed_data is provided
  if (!is.null(processed_data)) {
    layout_validation <- validate_experiment_layout(processed_data)
    qc_plots$layout_validation = layout_validation$plot
  }
  
  return(qc_plots)
}

#' Create broth contamination detection plot
#' Create broth contamination detection plot (single wavelength)
create_broth_contamination_plot <- function(corrected_results) {
  
  # Clean the data first
  corrected_data <- clean_corrected_data(corrected_results$corrected_data)
  od_raw_column <- detect_od_column_for_export(corrected_data, "raw")
  
  # Include both broth and half_broth wells
  broth_data <- corrected_data %>%
    filter(sample_type %in% c("broth", "half_broth"))
  
  if (nrow(broth_data) == 0) {
    return(ggplot() + 
             labs(title = "QC: No Broth/Half Broth Controls Found") + 
             theme_minimal())
  }
  
  # Calculate contamination indicators with FIXED first() usage
  broth_summary <- broth_data %>%
    group_by(well_id, sample_type) %>%
    summarise(
      initial_od = ifelse(n() > 0, .data[[od_raw_column]][1], NA_real_),  # FIXED
      final_od = ifelse(n() > 0, .data[[od_raw_column]][n()], NA_real_),  # FIXED
      max_od = max(.data[[od_raw_column]], na.rm = TRUE),
      od_range = max_od - initial_od,
      growth_trend = cor(time_hrs, .data[[od_raw_column]], use = "complete.obs"),
      .groups = "drop"
    ) %>%
    mutate(
      contamination_risk = case_when(
        od_range > 0.1 & growth_trend > 0.3 ~ "HIGH RISK",
        od_range > 0.05 | growth_trend > 0.1 ~ "MODERATE RISK", 
        TRUE ~ "CLEAN"
      )
    )
  
  broth_data %>%
    left_join(broth_summary %>% select(well_id, contamination_risk), by = "well_id") %>%
    ggplot(aes(x = time_hrs, y = .data[[od_raw_column]], color = contamination_risk)) +
    geom_line(aes(group = well_id), alpha = 0.7, linewidth = 1) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.3) +
    facet_wrap(~sample_type, scales = "free_y") +  # Separate panels for Broth and Half Broth
    scale_color_manual(
      values = c("CLEAN" = "green3", "MODERATE RISK" = "orange", "HIGH RISK" = "red"),
      name = "Contamination Risk"
    ) +
    labs(
      title = "QC: Broth and Half Broth Control Contamination Detection",
      subtitle = "Monitoring broth-only and half-broth wells for signs of bacterial growth",
      x = "Time (hours)",
      y = "Raw OD600",
      caption = "Clean broth should remain flat. Growth curves indicate contamination."
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.subtitle = element_text(color = "darkblue")
    )
}

#' Create broth contamination plot for specific wavelength
create_broth_contamination_plot_multi <- function(corrected_results, wavelength) {
  od_raw_column <- paste0("od", wavelength, "_raw")
  
  # Clean the data first
  corrected_data <- clean_corrected_data(corrected_results$corrected_data)
  
  # Include both broth and half_broth wells
  broth_data <- corrected_data %>%
    filter(sample_type %in% c("broth", "half_broth"))
  
  if (nrow(broth_data) == 0) {
    return(ggplot() + 
             labs(title = paste("QC: No Broth/Half Broth Controls Found -", wavelength, "nm")) + 
             theme_minimal())
  }
  
  # Calculate contamination indicators with FIXED first() usage
  broth_summary <- broth_data %>%
    group_by(well_id, sample_type) %>%
    summarise(
      initial_od = ifelse(n() > 0, .data[[od_raw_column]][1], NA_real_),  # FIXED
      final_od = ifelse(n() > 0, .data[[od_raw_column]][n()], NA_real_),  # FIXED
      max_od = max(.data[[od_raw_column]], na.rm = TRUE),
      od_range = max_od - initial_od,
      growth_trend = cor(time_hrs, .data[[od_raw_column]], use = "complete.obs"),
      .groups = "drop"
    ) %>%
    mutate(
      contamination_risk = case_when(
        od_range > 0.1 & growth_trend > 0.3 ~ "HIGH RISK",
        od_range > 0.05 | growth_trend > 0.1 ~ "MODERATE RISK", 
        TRUE ~ "CLEAN"
      )
    )
  
  broth_data %>%
    left_join(broth_summary %>% select(well_id, contamination_risk), by = "well_id") %>%
    ggplot(aes(x = time_hrs, y = .data[[od_raw_column]], color = contamination_risk)) +
    geom_line(aes(group = well_id), alpha = 0.7, linewidth = 1) +
    geom_smooth(method = "loess", se = TRUE, alpha = 0.3) +
    facet_wrap(~sample_type, scales = "free_y") +  # Separate panels for Broth and Half Broth
    scale_color_manual(
      values = c("CLEAN" = "green3", "MODERATE RISK" = "orange", "HIGH RISK" = "red"),
      name = "Contamination Risk"
    ) +
    labs(
      title = paste("QC: Broth/Half Broth Contamination Detection -", wavelength, "nm"),
      subtitle = "Monitoring broth-only and half-broth wells for bacterial growth",
      x = "Time (hours)",
      y = paste0("Raw OD", wavelength),
      caption = "Clean broth should remain flat. Growth curves indicate contamination."
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.subtitle = element_text(color = "darkblue")
    )
}

#' Create QC plots for specific wavelength
create_qc_plots_multi <- function(corrected_results, 
                                  processed_data, wavelength) {
  
  qc_plots <- list(
    broth_contamination = create_broth_contamination_plot_multi(corrected_results, wavelength),
    np_kinetics = create_np_kinetics_plot_multi(corrected_results, wavelength),
    before_after = create_before_after_plot_multi(corrected_results, wavelength),
    temperature = plot_temperature(list(corrected_results = corrected_results), wavelength = wavelength, style = "detailed")  # NEW
  )
  
  # Add layout validation plot
  layout_validation <- validate_experiment_layout(processed_data)
  qc_plots$layout_validation = layout_validation$plot
  
  return(qc_plots)
}

create_broth_stability_plot <- function(corrected_results) {
  
  data <- corrected_results$corrected_data
  od_raw_column <- detect_od_column_for_export(data, "raw")
  
  data %>%
    filter(sample_type == "broth") %>%
    ggplot(aes(x = time_hrs, y = .data[[od_raw_column]])) +
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
    geom_smooth(method = "loess", color = "firebrick", linewidth = 1) +
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
      geom_line(aes(group = well_id), alpha = 0.5, color = "gray43") +
      geom_smooth(method = "loess", color = "gray43", linewidth = 1) +
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
      geom_line(aes(group = well_id), alpha = 0.5, color = "gray43") +
      geom_smooth(method = "loess", color = "gray43", linewidth = 1) +
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
  
  data <- corrected_results$corrected_data
  od_raw_column <- detect_od_column_for_export(data, "raw")
  
  data %>%
    filter(sample_type == "blank") %>%
    ggplot(aes(x = time_hrs, y = .data[[od_raw_column]])) +
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

detect_od_column_for_export <- function(data, data_type) {
  
  if (data_type == "raw") {
    candidates <- c("od600_raw", "od_raw", "od600", "od")
  } else if (data_type == "broth_corrected") {
    candidates <- c("od600_broth_corrected", "od_broth_corrected", "od600", "od")
  } else { # corrected
    candidates <- c("od600_final", "od_final", "od600", "od")
  }
  
  for (col in candidates) {
    if (col %in% names(data)) {
      return(col)
    }
  }
  
  # Fallback
  od_columns <- names(data)[str_detect(names(data), "od")]
  if (length(od_columns) > 0) {
    return(od_columns[1])
  }
  
  stop("Could not find OD column for data_type:", data_type)
}

#' Create blank stability plot for specific wavelength (FIXED ORDERING)
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
    # Sort concentrations numerically
    conc_numbers <- as.numeric(str_extract(blank_concentrations, "\\d+"))
    blank_concentrations_sorted <- blank_concentrations[order(conc_numbers)]
    
    cat("  • Blank concentrations found:", paste(blank_concentrations_sorted, collapse = ", "), "\n")
    
    # Create plot with ordered concentrations
    blank_data %>%
      filter(!is.na(concentration)) %>%
      mutate(concentration_f = factor(concentration, levels = blank_concentrations_sorted)) %>%
      ggplot(aes(x = time_hrs, y = .data[[od_raw_column]])) +
      geom_line(aes(group = well_id), alpha = 0.5, color = "gray43") +
      geom_smooth(method = "loess", color = "gray43", linewidth = 1) +
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
      geom_line(aes(group = well_id), alpha = 0.5, color = "gray43") +
      geom_smooth(method = "loess", color = "gray43", linewidth = 1) +
      labs(
        title = paste("QC: Blank Control Stability -", wavelength, "nm"),
        subtitle = "Blank controls (should be near zero)",
        x = "Time (hours)",
        y = paste0("Raw OD", wavelength)
      ) +
      theme_minimal()
  }
}

#' Create NP kinetics plot for specific wavelength
#' Create NP kinetics plot for specific wavelength (FIXED to handle missing NP controls properly)
create_np_kinetics_plot_multi <- function(corrected_results, wavelength) {
  od_broth_corrected_column <- paste0("od", wavelength, "_broth_corrected")
  
  # Check if there are any NP controls
  np_data <- corrected_results$corrected_data %>%
    filter(sample_type == "np_control")
  
  if (nrow(np_data) == 0) {
    cat("  • No NP controls found for", wavelength, "nm - creating placeholder plot\n")
    # Return a placeholder plot
    placeholder_plot <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = paste0("No NP Controls\nin this experiment\n(", wavelength, " nm)"), 
               size = 8, hjust = 0.5, vjust = 0.5,
               color = "gray50") +
      theme_void() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        panel.border = element_rect(color = "gray80", fill = NA, linewidth = 1)
      ) +
      labs(title = paste("QC: Nanoparticle Kinetics -", wavelength, "nm"),
           subtitle = "NP controls not included in experimental design")
    
    return(placeholder_plot)
  }
  
  # If we have NP controls, proceed with the normal plot
  # Get concentrations in proper numerical order
  np_concentrations <- unique(np_data$concentration)
  conc_numbers <- as.numeric(str_extract(np_concentrations, "\\d+"))
  np_concentrations_sorted <- np_concentrations[order(conc_numbers)]
  
  cat("  • NP concentrations found for", wavelength, "nm:", paste(np_concentrations_sorted, collapse = ", "), "\n")
  
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
    scale_color_viridis_d(option = "magma", begin = 0.1, end = 0.8)
}

# create_before_after_plot <- function(corrected_results) {
#   
#   # Get concentrations in proper numerical order
#   sample_data <- corrected_results$corrected_data %>%
#     filter(sample_type == "sample")
#   
#   sample_concentrations <- unique(sample_data$concentration)
#   conc_numbers <- as.numeric(str_extract(sample_concentrations, "\\d+"))
#   sample_concentrations_sorted <- sample_concentrations[order(conc_numbers)]
#   
#   cat("• Sample concentrations found:", paste(sample_concentrations_sorted, collapse = ", "), "\n")
#   
#   # Create the comparison data
#   comparison_data <- sample_data %>%
#     select(time_hrs, concentration, well_id, od600_raw, od600_final) %>%
#     pivot_longer(cols = c(od600_raw, od600_final), 
#                  names_to = "data_type", values_to = "od600") %>%
#     mutate(
#       data_type = case_when(
#         data_type == "od600_raw" ~ "Raw",
#         data_type == "od600_final" ~ "Corrected"
#       ),
#       concentration_f = factor(concentration, levels = sample_concentrations_sorted),
#       data_type_f = factor(data_type, levels = c("Raw", "Corrected"))
#     )
#   
#   # Create plot with fixed scale for comparison
#   comparison_data %>%
#     ggplot(aes(x = time_hrs, y = od600, color = data_type_f)) +
#     stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
#     stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA,
#                  aes(fill = data_type_f)) +
#     facet_wrap(~concentration_f, scales = "fixed") +  # Fixed scale for comparison
#     labs(
#       title = "QC: Raw vs Corrected Growth Curves",
#       subtitle = "Concentrations ordered numerically - Fixed scale for comparison",
#       x = "Time (hours)",
#       y = "OD600",
#       color = "Data Type",
#       fill = "Data Type"
#     ) +
#     theme_minimal() +
#     theme(
#       strip.text = element_text(face = "bold"),
#       legend.position = "bottom"
#     ) +
#     scale_color_manual(values = c("Raw" = "#E31A1C", "Corrected" = "#1F78B4")) +
#     scale_fill_manual(values = c("Raw" = "#E31A1C", "Corrected" = "#1F78B4"))
# }

#' Create before/after correction plot (UPDATED to handle no NP controls)
create_before_after_plot <- function(corrected_results) {
  
  # Get concentrations in proper numerical order
  sample_data <- corrected_results$corrected_data %>%
    filter(sample_type == "sample")
  
  sample_concentrations <- unique(sample_data$concentration)
  conc_numbers <- as.numeric(str_extract(sample_concentrations, "\\d+"))
  sample_concentrations_sorted <- sample_concentrations[order(conc_numbers)]
  
  cat("• Sample concentrations found:", paste(sample_concentrations_sorted, collapse = ", "), "\n")
  
  # Check if we have NP corrections
  has_np_corrections <- "np_correction" %in% names(sample_data) && 
    any(sample_data$np_correction > 0, na.rm = TRUE)
  
  if (!has_np_corrections) {
    # Simplified plot for no NP corrections
    comparison_data <- sample_data %>%
      select(time_hrs, concentration, well_id, od600_raw, od600_final) %>%
      pivot_longer(cols = c(od600_raw, od600_final), 
                   names_to = "data_type", values_to = "od600") %>%
      mutate(
        data_type = case_when(
          data_type == "od600_raw" ~ "Raw",
          data_type == "od600_final" ~ "Broth Corrected"  # Changed label
        ),
        concentration_f = factor(concentration, levels = sample_concentrations_sorted),
        data_type_f = factor(data_type, levels = c("Raw", "Broth Corrected"))
      )
    
    # Create plot with updated subtitle
    return(
      comparison_data %>%
        ggplot(aes(x = time_hrs, y = od600, color = data_type_f)) +
        stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
        stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA,
                     aes(fill = data_type_f)) +
        facet_wrap(~concentration_f, scales = "fixed") +
        labs(
          title = "QC: Raw vs Corrected Growth Curves",
          subtitle = "No NP correction applied (no NP controls in experiment)",
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
        scale_color_manual(values = c("Raw" = "#E31A1C", "Broth Corrected" = "#1F78B4")) +
        scale_fill_manual(values = c("Raw" = "#E31A1C", "Broth Corrected" = "#1F78B4"))
    )
  }
  
  # Original plot code for when NP corrections are present
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
    facet_wrap(~concentration_f, scales = "fixed") +
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

#' Create before/after plot for multi-wavelength (UPDATED to handle no NP controls)
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
  
  # Check if we have NP corrections
  has_np_corrections <- "np_correction" %in% names(sample_data) && 
    any(sample_data$np_correction > 0, na.rm = TRUE)
  
  # Create the comparison data
  comparison_data <- sample_data %>%
    select(time_hrs, concentration, well_id, !!od_raw_column, !!od_final_column) %>%
    pivot_longer(cols = c(!!od_raw_column, !!od_final_column), 
                 names_to = "data_type", values_to = "od_value") %>%
    mutate(
      data_type = case_when(
        str_detect(data_type, "_raw") ~ "Raw",
        str_detect(data_type, "_final") ~ if(has_np_corrections) "Corrected" else "Broth Corrected"
      ),
      concentration_f = factor(concentration, levels = sample_concentrations_sorted),
      data_type_f = factor(data_type, levels = c("Raw", if(has_np_corrections) "Corrected" else "Broth Corrected"))
    )
  
  # Create plot with appropriate subtitle
  comparison_data %>%
    ggplot(aes(x = time_hrs, y = od_value, color = data_type_f)) +
    stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
    stat_summary(fun.data = mean_se, geom = "ribbon", alpha = 0.2, color = NA,
                 aes(fill = data_type_f)) +
    facet_wrap(~concentration_f, scales = "fixed") +
    labs(
      title = paste("QC: Raw vs Corrected Growth Curves -", wavelength, "nm"),
      subtitle = if(has_np_corrections) {
        "Concentrations ordered numerically - Fixed scale for comparison"
      } else {
        "No NP correction applied (no NP controls in experiment)"
      },
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
    scale_color_manual(values = c("Raw" = "#E31A1C", 
                                  "Corrected" = "#1F78B4",
                                  "Broth Corrected" = "#1F78B4")) +
    scale_fill_manual(values = c("Raw" = "#E31A1C", 
                                 "Corrected" = "#1F78B4",
                                 "Broth Corrected" = "#1F78B4"))
}

#' Create simplified layout verification plot for composite
create_simplified_layout_plot <- function(data) {
  
  layout_viz <- data %>%
    distinct(well_id, sample_type, concentration, replicate_id, row_letter, col_number) %>%
    mutate(
      row_num = as.numeric(factor(row_letter, levels = LETTERS[1:8]))
    ) %>%
    ggplot(aes(x = col_number, y = 9 - row_num)) +
    geom_tile(aes(fill = sample_type), color = "white", linewidth = 0.5) +
    # NO TEXT LABELS - just the colors
    scale_x_continuous(breaks = 1:12, expand = c(0,0)) +
    scale_y_continuous(breaks = 1:8, labels = LETTERS[8:1], expand = c(0,0)) +
    scale_fill_viridis_d(name = "Sample Type", option = "E",
                         begin = 0, end = 0.8) +
    labs(
      title = "Experimental Layout",
      subtitle = "Verify this matches your setup",
      x = "Column",
      y = "Row"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      # Smaller elements for the composite view
      plot.title = element_text(size = 10),
      plot.subtitle = element_text(size = 8),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9)
    ) +
    coord_fixed()
  
  return(layout_viz)
}

#' Validate and visualize experimental layout
validate_experiment_layout <- function(processed_data) {
  
  # Handle both old and new data structures
  if (!is.null(processed_data$data)) {
    layout_data <- processed_data$data
  } else if (!is.null(processed_data$corrected_data)) {
    layout_data <- processed_data$corrected_data
  } else {
    stop("Could not find data in processed_data object")
  }
  
  layout_viz <- layout_data %>%
    distinct(well_id, sample_type, concentration, replicate_id, row_letter, col_number) %>%
    mutate(
      row_num = as.numeric(factor(row_letter, levels = LETTERS[1:8])),
      display_label = case_when(
        sample_type == "sample" ~ paste0("S", str_extract(concentration, "\\d+"), "\n", replicate_id),
        sample_type == "np_control" ~ paste0("NP", str_extract(concentration, "\\d+")),
        sample_type == "untreated_control" ~ paste0("UNT\n", well_id),
        sample_type == "blank" ~ paste0("BLK", str_extract(concentration, "\\d+")),
        sample_type == "broth" ~ "BROTH",
        sample_type == "half_broth" ~ "1/2BRTH",  # FIXED: Added missing case
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
      subtitle = "S# = Sample, NP# = NP control, UNT = Untreated, BLK# = Blank, BROTH = Broth control, 1/2BRTH = Half Broth",
      x = "Column",
      y = "Row",
      caption = "Rep_A, Rep_B, Rep_C = Biological replicates | Untreated controls shown with well IDs"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_blank()
    ) +
    coord_fixed()
  
  # Print summary (rest of function remains the same)
  cat("=== LAYOUT VALIDATION ===\n")
  
  # Sample replicates
  cat("Sample replicates by concentration:\n")
  sample_summary <- layout_data %>%
    filter(sample_type == "sample") %>%
    distinct(concentration, replicate_id, well_id) %>%
    arrange(concentration, replicate_id)
  print(sample_summary)
  
  # Check for half broth controls (NEW)
  half_broth_summary <- layout_data %>%
    filter(sample_type == "half_broth") %>%
    distinct(well_id) %>%
    arrange(well_id)
  
  if (nrow(half_broth_summary) > 0) {
    cat("\nHalf Broth control wells:\n")
    print(half_broth_summary)
  }
  
  # Untreated controls
  cat("\nUntreated control wells:\n")
  untreated_summary <- layout_data %>%
    filter(sample_type == "untreated_control") %>%
    distinct(well_id) %>%
    arrange(well_id)
  print(untreated_summary)
  
  # Blank replicates
  cat("\nBlank replicates by 'concentration':\n")
  blank_summary <- layout_data %>%
    filter(sample_type == "blank") %>%
    distinct(concentration, replicate_id, well_id) %>%
    arrange(concentration, replicate_id)
  print(blank_summary)
  
  return(list(
    plot = layout_viz,
    replicates = sample_summary,
    untreated = untreated_summary,
    blanks = blank_summary,
    half_broth = half_broth_summary  # NEW
  ))
}

#' Validate layout against data file
validate_layout_file <- function(layout_csv) {
  
  if (!file.exists(layout_csv)) {
    stop("Layout file not found: ", layout_csv)
  }
  
  cat(" VALIDATING LAYOUT FILE\n")
  cat("========================\n")
  
  layout_data <- read_csv(layout_csv, show_col_types = FALSE)
  
  # Check required columns
  required_cols <- c("Well", "Category")
  missing_cols <- setdiff(required_cols, names(layout_data))
  
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Summary
  cat("Wells defined:", nrow(layout_data), "\n")
  cat("Categories used:\n")
  print(table(layout_data$Category))
  
  if ("Concentration_Value" %in% names(layout_data)) {
    conc_summary <- layout_data %>%
      filter(!is.na(Concentration_Value)) %>%
      count(Category, Concentration_Value) %>%
      arrange(Category, Concentration_Value)
    
    cat("\nConcentration assignments:\n")
    print(conc_summary)
  }
  
  cat("\n Layout file validation complete\n")
  return(invisible(layout_data))
}

#' Create layout verification from data if processed_data not available
verify_experimental_layout_from_data <- function(data) {
  
  layout_viz <- data %>%
    distinct(well_id, sample_type, concentration, replicate_id, row_letter, col_number) %>%
    mutate(
      row_num = as.numeric(factor(row_letter, levels = LETTERS[1:8])),
      display_label = case_when(
        sample_type == "sample" ~ paste0("S", str_extract(concentration, "\\d+"), "\n", replicate_id),
        sample_type == "np_control" ~ paste0("NP", str_extract(concentration, "\\d+")),
        sample_type == "untreated_control" ~ paste0("UNT\n", well_id),
        sample_type == "blank" ~ paste0("BLK", str_extract(concentration, "\\d+")),
        sample_type == "broth" ~ "BROTH",
        sample_type == "half_broth" ~ "1/2BRTH",  # FIXED: Added missing case
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
      title = "Experimental Layout",
      subtitle = "Verify this matches your setup",
      x = "Column",
      y = "Row"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom"
    ) +
    coord_fixed()
  
  return(list(plot = layout_viz))
}

# ===== ACCESSOR FUNCTIONS =====

#' Export Growth Curve Data in Wide Format with Replicate Handling
#'
#' This function reshapes growth curve data into a wide format, including individual replicates,
#' averages, and standard deviations for each concentration. It supports any wavelength and
#' allows filtering by sample type.
#'
#' @param results A list object returned by `analyze_growth_curves()` or `analyze_growth_curves_with_summaries()`.
#' @param data_type Character. Specifies which OD data to export. Options are `"raw"`, `"broth_corrected"`, or `"corrected"` (default).
#' @param sample_types Character vector. Specifies which sample types to include. Options include `"sample"`, `"untreated_control"`, or both.
#'
#' @return A data frame in wide format with:
#' - Time points as rows
#' - Columns for each replicate (e.g., `"10_A"`, `"10_B"`)
#' - AVERAGE columns (e.g., `"AVERAGE_10"`)
#' - SD columns (e.g., `"SD_10"`) if multiple replicates exist
#'
#' @details
#' - Automatically detects the wavelength from the OD column names.
#' - Concentration columns are sorted numerically for clarity.
#' - If only one replicate exists, SD columns are filled with `NA` for consistency.
#'
#' @seealso [export_untreated_wide()], [analyze_growth_curves()]
#'
#' @examples
#' \dontrun{
#' results <- analyze_growth_curves("data/plate_reader_output.csv")
#' wide_data <- export_wide_format(results, data_type = "corrected", sample_types = c("sample", "untreated_control"))
#' }
#'
#' @export
export_wide_format <- function(results, data_type = "corrected",
                               sample_types = "sample",
                               max_retries = 3) {
  # Validate inputs
  if (is.null(results) || !is.list(results)) {
    stop("Results must be a valid analysis results list")
  }
  if (!"corrected_results" %in% names(results)) {
    stop("Results must contain corrected_results. Did you run analyze_growth_curves() first?")
  }
  valid_data_types <- c("raw", "broth_corrected", "corrected")
  if (!data_type %in% valid_data_types) {
    stop("data_type must be one of: ", paste(valid_data_types, collapse = ", "))
  }
  # Retry logic for data access
  data <- NULL
  for (attempt in 1:max_retries) {
    tryCatch({
      data <- results$corrected_results$corrected_data
      break
    }, error = function(e) {
      if (attempt == max_retries) {
        stop("Failed to access corrected data after ", max_retries, " attempts: ", e$message)
      }
      cat("• Retry", attempt, "- data access failed, retrying...\n")
      Sys.sleep(0.1)
    })
  }
  if (is.null(data)) {
    stop("Could not access corrected data from results")
  }
  # Validate data structure
  required_cols <- c("time_hrs", "well_id", "concentration", "sample_type")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Data is missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  # Detect the OD column based on data type
  od_column <- tryCatch({
    detect_od_column_for_export(data, data_type)
  }, error = function(e) {
    stop("Could not detect OD column for data_type '", data_type, "': ", e$message)
  })
  # Get the data for export with validation - KEEP sample_type column
  plot_data <- data %>%
    filter(sample_type %in% sample_types) %>%
    select(time_hrs, concentration, replicate_id, well_id, sample_type, !!sym(od_column)) %>%  # ADDED sample_type
    rename(od_value = !!sym(od_column))
  if (nrow(plot_data) == 0) {
    stop("No data found for sample_types: ", paste(sample_types, collapse = ", "))
  }
  cat("  - Sample types:", paste(sample_types, collapse = ", "), "\n")
  cat("  - Using OD column:", od_column, "\n")
  cat("  - Available replicates:", paste(unique(plot_data$replicate_id), collapse = ", "), "\n")
  # ROUND TIME VALUES and pre-aggregate to eliminate many-to-many relationships
  plot_data <- plot_data %>%
    mutate(time_hrs = round(time_hrs, 3)) %>%  # Round to 3 decimal places
    group_by(time_hrs, concentration, replicate_id, well_id, sample_type) %>%  # sample_type is now available
    summarise(od_value = mean(od_value, na.rm = TRUE), .groups = "drop")  # Pre-aggregate any duplicates
  # Get unique time points
  time_points <- sort(unique(plot_data$time_hrs))
  if (length(time_points) == 0) {
    stop("No time points found in data")
  }
  cat("  - Time points:", length(time_points), "(from", min(time_points), "to", max(time_points), "hrs)\n")
  # HANDLE UNTREATED CONTROLS (with NA concentrations)
  if ("untreated_control" %in% sample_types) {
    cat("  - Processing untreated controls (no concentration grouping)\n")
    # Filter to just untreated controls for this section
    untreated_data <- plot_data %>%
      filter(sample_type == "untreated_control")
    # Get unique replicate IDs
    replicate_ids <- sort(unique(untreated_data$replicate_id))
    replicate_ids <- replicate_ids[!is.na(replicate_ids)]
    # Create the wide format data using pivot_wider
    wide_data <- untreated_data %>%
      select(time_hrs, replicate_id, od_value) %>%
      # Add a prefix to replicate names for clarity
      mutate(replicate_name = paste("Untreated", replicate_id, sep = "_")) %>%
      select(-replicate_id) %>%
      pivot_wider(
        names_from = replicate_name,
        values_from = od_value,
        values_fn = mean  # Handle any remaining duplicates
      ) %>%
      arrange(time_hrs)
    # Add AVERAGE column
    replicate_cols <- names(wide_data)[str_starts(names(wide_data), "Untreated_")]
    if (length(replicate_cols) > 0) {
      wide_data <- wide_data %>%
        rowwise() %>%
        mutate(
          AVERAGE_Untreated = mean(c_across(all_of(replicate_cols)), na.rm = TRUE),
          SD_Untreated = if(length(replicate_cols) > 1) {
            sd(c_across(all_of(replicate_cols)), na.rm = TRUE)
          } else {
            NA_real_
          }
        ) %>%
        ungroup()
    }
    cat("  - Exported untreated data dimensions:", nrow(wide_data), "timepoints x", ncol(wide_data), "columns\n")
    cat("  - Replicates included:", paste(replicate_ids, collapse = ", "), "\n")
    return(wide_data)
  }
  # HANDLE SAMPLES WITH CONCENTRATIONS
  # Filter to just samples for this section
  sample_data <- plot_data %>%
    filter(sample_type == "sample")
  conc_raw <- unique(sample_data$concentration)
  conc_raw <- conc_raw[!is.na(conc_raw)]
  if (length(conc_raw) == 0) {
    stop("No valid concentrations found in data")
  }
  conc_numbers <- as.numeric(str_extract(conc_raw, "\\d+"))
  conc_sorted <- conc_raw[order(conc_numbers, na.last = TRUE)]
  # Get unique replicate IDs
  replicate_ids <- sort(unique(sample_data$replicate_id))
  replicate_ids <- replicate_ids[!is.na(replicate_ids)]
  # Create wide format using pivot_wider
  wide_data <- sample_data %>%
    # Create combined column names
    unite("conc_rep", concentration, replicate_id, sep = "_", remove = FALSE) %>%
    select(time_hrs, conc_rep, od_value) %>%
    pivot_wider(
      names_from = conc_rep,
      values_from = od_value,
      values_fn = mean  # Handle any remaining duplicates
    ) %>%
    arrange(time_hrs)
  # Add AVERAGE and SD columns for each concentration
  for (conc in conc_sorted) {
    # Find columns for this concentration
    conc_cols <- names(wide_data)[str_starts(names(wide_data), paste0(conc, "_"))]
    if (length(conc_cols) > 0) {
      avg_col_name <- paste("AVERAGE", conc, sep = "_")
      sd_col_name <- paste("SD", conc, sep = "_")
      wide_data <- wide_data %>%
        rowwise() %>%
        mutate(
          !!avg_col_name := mean(c_across(all_of(conc_cols)), na.rm = TRUE),
          !!sd_col_name := if(length(conc_cols) > 1) {
            sd(c_across(all_of(conc_cols)), na.rm = TRUE)
          } else {
            NA_real_
          }
        ) %>%
        ungroup()
    }
  }
  # Reorder columns: time_hrs, then individual replicates, then averages, then SDs
  time_col <- "time_hrs"
  rep_cols <- names(wide_data)[str_detect(names(wide_data), "^Conc_\\d+(Rep|UNT_)")]
  avg_cols <- names(wide_data)[str_starts(names(wide_data), "AVERAGE_")]
  sd_cols <- names(wide_data)[str_starts(names(wide_data), "SD_")]
  # Reorder columns
  col_order <- c(time_col, rep_cols, avg_cols, sd_cols)
  col_order <- intersect(col_order, names(wide_data))  # Only keep columns that exist
  wide_data <- wide_data %>%
    select(all_of(col_order))
  # Validate final result
  if (nrow(wide_data) == 0) {
    stop("Final result is empty - this indicates a data processing error")
  }
  if (ncol(wide_data) <= 1) {
    stop("Final result has no data columns - check your sample_types and data_type parameters")
  }
  # Print summary
  cat("  - Exported data dimensions:", nrow(wide_data), "timepoints x", ncol(wide_data), "columns\n")
  cat("  - Concentrations included:", paste(conc_sorted, collapse = ", "), "\n")
  cat("  - Replicates per concentration:", length(replicate_ids), "\n")
  return(wide_data)
}

#' Export Untreated Control Data in Wide Format
#'
#' This function extracts untreated control data from the results of a growth curve analysis
#' and reshapes it into a wide format, with time points as rows and concentrations as columns.
#' It supports different data types (raw, broth-corrected, or fully corrected).
#'
#' @param results A list object returned by `analyze_growth_curves()` containing processed growth curve data.
#' @param data_type Character. Specifies which OD data to export. Options are `"raw"`, `"broth_corrected"`, or `"corrected"` (default).
#'
#' @return A data frame in wide format with time points as rows and OD600 values for each untreated concentration as columns.
#'
#' @details
#' - Only untreated control samples (`sample_type == "untreated_control"`) are included.
#' - Concentration columns are sorted numerically for clarity.
#' - The function assumes a single replicate per concentration for untreated controls.
#'
#' @seealso [analyze_growth_curves()], [pivot_wider()]
#'
#' @examples
#' \dontrun{
#' results <- analyze_growth_curves("data/plate_reader_output.csv")
#' untreated_wide <- export_untreated_wide(results)
#' }
#'
#' @export
export_untreated_wide <- function(results, data_type = "corrected") {
  
  # Get the data
  data <- results$corrected_results$corrected_data
  
  # Detect the OD column based on data type
  od_column <- detect_od_column_for_export(data, data_type)
  
  # Get untreated data
  untreated_data <- data %>%
    filter(sample_type == "untreated_control") %>%
    select(time_hrs, concentration, !!sym(od_column)) %>%
    rename(od600 = !!sym(od_column))
  
  cat("  - Using OD column:", od_column, "\n")
  cat("  - Untreated control data points:", nrow(untreated_data), "\n")
  
  # Get unique time points
  time_points <- sort(unique(untreated_data$time_hrs))
  
  # Get concentrations in proper numerical order
  conc_raw <- unique(untreated_data$concentration)
  conc_numbers <- as.numeric(str_extract(conc_raw, "\\d+"))
  conc_order <- order(conc_numbers)
  conc_sorted <- conc_raw[conc_order]
  
  # Create wide format - simpler for untreated (single replicate)
  result <- untreated_data %>%
    pivot_wider(
      names_from = concentration,
      values_from = od600,
      id_cols = time_hrs
    ) %>%
    arrange(time_hrs)
  
  # Reorder columns to have time first, then concentrations in numerical order
  time_col <- "time_hrs"
  conc_cols <- intersect(conc_sorted, names(result))
  result <- result %>%
    select(all_of(c(time_col, conc_cols)))
  
  cat("  - Exported untreated data:", nrow(result), "timepoints x", ncol(result), "columns\n")
  cat("  - Concentrations included:", paste(conc_cols, collapse = ", "), "\n")
  
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


#' Get Data in Colleague's Preferred Format
#'
#' This function processes analysis results and returns them in a format suitable for sharing with colleagues.
#' It supports both single and multi-wavelength data and can optionally export the results to CSV files.
#'
#' @param results A list containing the results from the main analysis. Can include single or multiple wavelengths.
#' @param include_untreated Logical. If `TRUE`, includes untreated control samples in the output. Default is `TRUE`.
#' @param export_to_csv Logical. If `TRUE`, exports the formatted data to CSV files. Default is `FALSE`.
#' @param wavelength Character or `NULL`. Specify a wavelength (e.g., `"600"`) to extract data for a specific wavelength,
#' `"all"` to extract all available wavelengths, or leave `NULL` to prompt for selection.
#'
#' @return A list containing formatted sample data. If `include_untreated` is `TRUE`, untreated control data is also included.
#' If `wavelength = "all"`, the list will contain entries for each wavelength.
#'
#' @details
#' - If multi-wavelength data is detected and `wavelength` is `NULL`, the function prompts the user to specify a wavelength.
#' - If `wavelength = "all"`, data for all available wavelengths is returned and optionally exported.
#' - For single-wavelength data, the function delegates to `get_colleague_format_single()`.
#'
#' @seealso [get_colleague_format_single()], [export_wide_format()]
#'
#' @examples
#' \dontrun{
#' get_colleague_format(results, wavelength = "600")
#' get_colleague_format(results, wavelength = "all", export_to_csv = TRUE)
#' }
#'
#' @export
get_colleague_format <- function(results, include_untreated = TRUE, 
                                 export_to_csv = FALSE, wavelength = NULL,
                                 validate_inputs = TRUE) {  # ADD VALIDATION FLAG
  
  if (validate_inputs) {
    # Validate main inputs
    if (is.null(results) || !is.list(results)) {
      stop("Results must be a valid analysis results list")
    }
    
    if (!is.logical(include_untreated)) {
      stop("include_untreated must be TRUE or FALSE")
    }
    
    if (!is.logical(export_to_csv)) {
      stop("export_to_csv must be TRUE or FALSE")
    }
  }
  
  # Detect if this is multi-wavelength results
  if ("wavelength_600" %in% names(results) || "wavelength_420" %in% names(results)) {
    
    wavelength_keys <- names(results)[str_detect(names(results), "^wavelength_")]
    available_wavelengths <- str_extract(wavelength_keys, "\\d+")
    
    if (is.null(wavelength)) {
      cat("• Multi-wavelength results detected. Specify wavelength:\n")
      for (i in seq_along(wavelength_keys)) {
        wl <- available_wavelengths[i]
        cat("  get_colleague_format(results, wavelength = '", wl, "')  # ", wl, " nm\n")
      }
      cat("\n• Or get all wavelengths:\n")
      cat("  get_colleague_format(results, wavelength = 'all')\n")
      return(invisible())
      
    } else if (wavelength == "all") {
      cat("• Exporting all wavelengths\n")
      
      # Export each wavelength separately with better error handling
      all_results <- list()
      
      for (i in seq_along(wavelength_keys)) {
        wl_key <- wavelength_keys[i]
        wl <- available_wavelengths[i]
        
        cat("• Processing", wl, "nm...\n")
        
        tryCatch({
          single_result <- results[[wl_key]]
          wl_data <- export_wide_format(single_result, data_type = "corrected", 
                                        sample_types = "sample")
          
          all_results[[paste0("samples_", wl, "nm")]] <- wl_data
          
          if (include_untreated) {
            untreated_data <- export_wide_format(single_result, data_type = "corrected", 
                                                 sample_types = "untreated_control")
            all_results[[paste0("untreated_", wl, "nm")]] <- untreated_data
          }
        }, error = function(e) {
          cat("• Error processing", wl, "nm:", e$message, "\n")
          cat("• Skipping this wavelength\n")
        })
      }
      
      # Export to CSV if requested
      if (export_to_csv && length(all_results) > 0) {
        for (name in names(all_results)) {
          tryCatch({
            filename <- paste0("growth_curve_data_", name, ".csv")
            write_csv(all_results[[name]], filename)
            cat("• Exported:", filename, "\n")
          }, error = function(e) {
            cat("• Error exporting", name, ":", e$message, "\n")
          })
        }
      }
      
      cat("• Export complete for", length(all_results), "datasets\n")
      return(all_results)
      
    } else {
      # Specific wavelength requested with validation
      wavelength_key <- paste0("wavelength_", wavelength)
      
      if (!wavelength_key %in% names(results)) {
        stop("Wavelength ", wavelength, " not found. Available: ", 
             paste(available_wavelengths, collapse = ", "))
      }
      
      cat("• Exporting", wavelength, "nm data\n")
      
      # Use the single wavelength with error handling
      tryCatch({
        single_result <- results[[wavelength_key]]
        colleague_data <- get_colleague_format_single(single_result, include_untreated, export_to_csv, wavelength)
        return(colleague_data)
      }, error = function(e) {
        stop("Error processing wavelength ", wavelength, ": ", e$message)
      })
    }
    
  } else {
    # Single wavelength results - use original function with error handling
    tryCatch({
      colleague_data <- get_colleague_format_single(results, include_untreated, export_to_csv)
      return(colleague_data)
    }, error = function(e) {
      stop("Error processing single wavelength results: ", e$message)
    })
  }
}

#' Export Single-Wavelength Data in Colleague Format
#'
#' This function formats and optionally exports single-wavelength analysis results
#' into a wide format suitable for sharing with colleagues. It supports exporting
#' both sample and untreated control data.
#'
#' @param results A list or data structure containing the results for a single wavelength.
#' @param include_untreated Logical. If `TRUE`, includes untreated control samples in the output. Default is `TRUE`.
#' @param export_to_csv Logical. If `TRUE`, exports the formatted data to CSV files. Default is `FALSE`.
#' @param wavelength Character or `NULL`. Used for labeling output files (e.g., `"600"` for 600nm). Optional.
#'
#' @return A list with one or two elements:
#' \describe{
#'   \item{samples}{A data frame of sample data in wide format.}
#'   \item{untreated}{(Optional) A data frame of untreated control data in wide format.}
#' }
#'
#' @details
#' - Uses `export_wide_format()` to reshape the data.
#' - If `export_to_csv` is `TRUE`, writes CSV files named according to the wavelength.
#' - Designed to be called internally by `get_colleague_format()` for single-wavelength cases.
#'
#' @seealso [get_colleague_format()], [export_wide_format()]
#'
#' @examples
#' \dontrun{
#' get_colleague_format_single(results, wavelength = "600")
#' get_colleague_format_single(results, include_untreated = FALSE, export_to_csv = TRUE)
#' }
#'
#' @export
get_colleague_format_single <- function(results, include_untreated = TRUE, 
                                        export_to_csv = FALSE, wavelength = NULL) {
  
  wl_label <- if (!is.null(wavelength)) paste0("_", wavelength, "nm") else ""
  
  cat("• === EXPORTING DATA IN COLLEAGUE FORMAT", toupper(wl_label), " ===\n")
  
  # Get sample data
  sample_data <- export_wide_format(results, data_type = "corrected", 
                                    sample_types = "sample")
  
  cat("• Sample data exported:", nrow(sample_data), "timepoints x", 
      ncol(sample_data) - 1, "measurement columns\n")
  
  output <- list(samples = sample_data)
  
  # Get untreated data if requested
  if (include_untreated) {
    untreated_data <- export_wide_format(results, data_type = "corrected", 
                                         sample_types = "untreated_control")
    
    cat("• Untreated data exported:", nrow(untreated_data), "timepoints x", 
        ncol(untreated_data) - 1, "measurement columns\n")
    
    output$untreated <- untreated_data
  }
  
  # Export to CSV if requested
  if (export_to_csv) {
    base_name <- paste0("growth_curve_data", wl_label)
    
    write_csv(sample_data, paste0(base_name, "_samples.csv"))
    cat("• Samples exported to:", paste0(base_name, "_samples.csv"), "\n")
    
    if (include_untreated) {
      write_csv(untreated_data, paste0(base_name, "_untreated.csv"))
      cat("• Untreated exported to:", paste0(base_name, "_untreated.csv"), "\n")
    }
  }
  
  cat("• === EXPORT COMPLETE ===\n")
  
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


#' Extract Data for a Specific Concentration
#'
#' This function retrieves growth curve data for a specified concentration from the analysis results.
#' It supports filtering by sample type, selecting the data correction level, and choosing between long or wide format.
#'
#' @param results A list returned by `analyze_growth_curves()` or one of its wrappers.
#' @param concentration Character. The concentration to extract (e.g., `"Conc_5"`).
#' @param sample_type Character. Which sample type to include: `"sample"`, `"untreated_control"`, or `"both"`. Default is `"sample"`.
#' @param data_type Character. Which OD data to extract. Options are `"raw"`, `"broth_corrected"`, or `"corrected"` (default).
#' @param format Character. Output format: `"long"` (default) or `"wide"`.
#'
#' @return A data frame containing the requested data. If `format = "wide"`, the output includes one column per sample-replicate combination.
#' Returns `NULL` if no data is found for the specified concentration.
#'
#' @details
#' - Automatically selects the appropriate OD column based on the `data_type`.
#' - If no data is found for the requested concentration, the function prints available options.
#' - In wide format, columns are named using the pattern `"sampletype_replicate"` (e.g., `"sample_Rep_A"`).
#'
#' @seealso [plot_concentration()], [export_wide_format()]
#'
#' @examples
#' \dontrun{
#' results <- analyze_growth_curves("data/plate_reader_output.csv")
#' data_long <- get_concentration_data(results, concentration = "10", format = "long")
#' data_wide <- get_concentration_data(results, concentration = "10", format = "wide")
#' }
#'
#' @export
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


#' Display Composite Plate Layout Plot
#'
#' This function displays a composite 2×2 panel plot showing the initial and final
#' plate layouts, both raw and corrected. It supports both single and multi-wavelength
#' results and allows users to specify which wavelength to display.
#'
#' @param results A list returned by `analyze_growth_curves()` or one of its wrappers.
#' @param wavelength Optional. Character string specifying the wavelength to display (e.g., `"600"`).
#' Use `"all"` to display all available wavelengths. If `NULL`, the function prompts the user.
#'
#' @return Invisibly returns a list of ggplot objects if `wavelength = "all"`, otherwise prints the selected composite plot.
#'
#' @details
#' - For multi-wavelength results, the function looks for elements named like `"wavelength_600"`, `"wavelength_420"`, etc.
#' - If no wavelength is specified, the function lists available options.
#' - The composite plot typically includes raw initial, corrected initial, raw final, and corrected final plate layouts.
#'
#' @seealso [show_plate_endpoint()], [show_growth_curves()]
#'
#' @examples
#' \dontrun{
#' results <- analyze_growth_curves("data/plate_reader_output.csv")
#' show_plate_composite(results) # Auto-detects format
#' show_plate_composite(results, wavelength = "600")
#' show_plate_composite(results, wavelength = "all")
#' }
#'
#' @export
show_plate_composite <- function(results, wavelength = NULL, include_layout = TRUE, max_time_hrs = NULL) {  # NEW PARAMETER
  
  if ("wavelength_600" %in% names(results) || "wavelength_420" %in% names(results)) {
    
    wavelength_keys <- names(results)[str_detect(names(results), "^wavelength_")]
    available_wavelengths <- str_extract(wavelength_keys, "\\d+")
    
    if (is.null(wavelength)) {
      cat("• Multi-wavelength results detected. Specify wavelength:\n")
      for (wl in available_wavelengths) {
        cat("  show_plate_composite(results, wavelength = '", wl, "')  # ", wl, " nm\n")
      }
      cat("  show_plate_composite(results, wavelength = 'all')  # Show all wavelengths\n")
      return(invisible())
      
    } else if (wavelength == "all") {
      cat("• Showing composite plate layouts for all wavelengths\n")
      
      plots_list <- list()
      
      for (i in seq_along(wavelength_keys)) {
        wl_key <- wavelength_keys[i]
        wl <- available_wavelengths[i]
        
        cat("• Displaying", wl, "nm composite plate\n")
        
        if (!is.null(max_time_hrs)) {
          # Apply time filter and recreate composite
          temp_results <- results[[wl_key]]$corrected_results
          temp_results$corrected_data <- temp_results$corrected_data %>%
            filter(time_hrs <= max_time_hrs)
          if (!is.null(temp_results$raw_data)) {
            temp_results$raw_data <- temp_results$raw_data %>%
              filter(time_hrs <= max_time_hrs)
          }
          
          updated_composite <- plot_plate_heatmap_composite_multi(temp_results, wl)
          composite_plot <- if(include_layout) updated_composite$combined_with_layout else updated_composite$combined_2x2
        } else {
          # Use existing plot or recreate if needed
          if (include_layout && !"combined_with_layout" %in% names(results[[wl_key]]$plots$plate_composite)) {
            updated_composite <- plot_plate_heatmap_composite_multi(results[[wl_key]]$corrected_results, wl)
            composite_plot <- updated_composite$combined_with_layout
          } else if (include_layout) {
            composite_plot <- results[[wl_key]]$plots$plate_composite$combined_with_layout
          } else {
            composite_plot <- results[[wl_key]]$plots$plate_composite$combined_2x2
          }
        }
        
        print(composite_plot)
        plots_list[[paste0("wavelength_", wl)]] <- composite_plot
      }
      
      return(invisible(plots_list))
      
    } else {
      wavelength_key <- paste0("wavelength_", wavelength)
      
      if (!wavelength_key %in% names(results)) {
        stop("Wavelength ", wavelength, " not found. Available: ", 
             paste(available_wavelengths, collapse = ", "))
      }
      
      if (!is.null(max_time_hrs)) {
        # Apply time filter and recreate composite
        temp_results <- results[[wavelength_key]]$corrected_results
        temp_results$corrected_data <- temp_results$corrected_data %>%
          filter(time_hrs <= max_time_hrs)
        if (!is.null(temp_results$raw_data)) {
          temp_results$raw_data <- temp_results$raw_data %>%
            filter(time_hrs <= max_time_hrs)
        }
        
        updated_composite <- plot_plate_heatmap_composite_multi(temp_results, wavelength)
        composite_plot <- if(include_layout) updated_composite$combined_with_layout else updated_composite$combined_2x2
      } else {
        # Use existing or recreate as before
        if (include_layout && !"combined_with_layout" %in% names(results[[wavelength_key]]$plots$plate_composite)) {
          updated_composite <- plot_plate_heatmap_composite_multi(results[[wavelength_key]]$corrected_results, wavelength)
          composite_plot <- updated_composite$combined_with_layout
        } else if (include_layout) {
          composite_plot <- results[[wavelength_key]]$plots$plate_composite$combined_with_layout
        } else {
          composite_plot <- results[[wavelength_key]]$plots$plate_composite$combined_2x2
        }
      }
      
      print(composite_plot)
    }
    
  } else {
    # Single wavelength
    if (!is.null(max_time_hrs)) {
      # Apply time filter and recreate composite
      temp_results <- results$corrected_results
      temp_results$corrected_data <- temp_results$corrected_data %>%
        filter(time_hrs <= max_time_hrs)
      if (!is.null(temp_results$raw_data)) {
        temp_results$raw_data <- temp_results$raw_data %>%
          filter(time_hrs <= max_time_hrs)
      }
      
      updated_composite <- plot_plate_heatmap_composite(temp_results)
      composite_plot <- if(include_layout) updated_composite$combined_with_layout else updated_composite$combined_2x2
    } else {
      # Use existing or recreate as before
      if (include_layout && !"combined_with_layout" %in% names(results$plots$plate_composite)) {
        updated_composite <- plot_plate_heatmap_composite(results$corrected_results)
        composite_plot <- updated_composite$combined_with_layout
      } else if (include_layout) {
        composite_plot <- results$plots$plate_composite$combined_with_layout
      } else {
        composite_plot <- results$plots$plate_composite$combined_2x2
      }
    }
    
    print(composite_plot)
  }
}

#' Display Final Plate Layout from Growth Curve Results
#'
#' This function displays the final plate layout plot from the results of `analyze_growth_curves()`.
#' It supports both single and multi-wavelength formats and allows users to specify which
#' wavelength to display.
#'
#' @param results A list returned by `analyze_growth_curves()` or one of its wrappers.
#' @param wavelength Optional. Character string specifying the wavelength to display (e.g., `"600"`).
#' Use `"all"` to display all available wavelengths. If `NULL`, the function prompts the user.
#'
#' @return Invisibly returns a list of ggplot objects if `wavelength = "all"`, otherwise prints the selected plot.
#'
#' @details
#' - For multi-wavelength results, the function looks for elements named like `"wavelength_600"`, `"wavelength_420"`, etc.
#' - If no wavelength is specified, the function lists available options.
#' - If a single wavelength is detected, the corresponding final plate layout is shown directly.
#'
#' @seealso [show_growth_curves()], [analyze_growth_curves()]
#'
#' @examples
#' \dontrun{
#' results <- analyze_growth_curves("data/plate_reader_output.csv")
#' show_plate_endpoint(results) # Auto-detects format
#' show_plate_endpoint(results, wavelength = "600")
#' show_plate_endpoint(results, wavelength = "all")
#' }
#'
#' @export
show_plate_endpoint <- function(results, wavelength = NULL) {
  
  if ("wavelength_600" %in% names(results) || "wavelength_420" %in% names(results)) {
    
    wavelength_keys <- names(results)[str_detect(names(results), "^wavelength_")]
    available_wavelengths <- str_extract(wavelength_keys, "\\d+")
    
    if (is.null(wavelength)) {
      cat("• Multi-wavelength results detected. Specify wavelength:\n")
      for (wl in available_wavelengths) {
        cat("  show_plate_endpoint(results, wavelength = '", wl, "')  # ", wl, " nm\n")
      }
      cat("  show_plate_endpoint(results, wavelength = 'all')  # Show all wavelengths\n")
      return(invisible())
      
    } else if (wavelength == "all") {
      cat("• Showing final plate layouts for all wavelengths\n")
      
      plots_list <- list()
      
      for (i in seq_along(wavelength_keys)) {
        wl_key <- wavelength_keys[i]
        wl <- available_wavelengths[i]
        
        cat("• Displaying", wl, "nm final plate\n")
        
        # Get the plot and add wavelength to title
        wl_plot <- results[[wl_key]]$plots$final_plate +
          labs(title = paste("Final Plate Layout -", wl, "nm"))
        
        print(wl_plot)
        plots_list[[paste0("wavelength_", wl)]] <- wl_plot
      }
      
      return(invisible(plots_list))
      
    } else {
      wavelength_key <- paste0("wavelength_", wavelength)
      
      if (!wavelength_key %in% names(results)) {
        stop("Wavelength ", wavelength, " not found. Available: ", 
             paste(available_wavelengths, collapse = ", "))
      }
      
      print(results[[wavelength_key]]$plots$final_plate)
    }
    
  } else {
    print(results$plots$final_plate)
  }
}

#' Display the initial plate layout  
show_plate_initial_condition <- function(results, wavelength = NULL) {
  
  if ("wavelength_600" %in% names(results) || "wavelength_420" %in% names(results)) {
    
    wavelength_keys <- names(results)[str_detect(names(results), "^wavelength_")]
    available_wavelengths <- str_extract(wavelength_keys, "\\d+")
    
    if (is.null(wavelength)) {
      cat("• Multi-wavelength results detected. Specify wavelength:\n")
      for (wl in available_wavelengths) {
        cat("  show_plate_initial_condition(results, wavelength = '", wl, "')  # ", wl, " nm\n")
      }
      cat("  show_plate_initial_condition(results, wavelength = 'all')  # Show all wavelengths\n")
      return(invisible())
      
    } else if (wavelength == "all") {
      cat("• Showing initial plate layouts for all wavelengths\n")
      
      plots_list <- list()
      
      for (i in seq_along(wavelength_keys)) {
        wl_key <- wavelength_keys[i]
        wl <- available_wavelengths[i]
        
        cat("• Displaying", wl, "nm initial plate\n")
        
        wl_plot <- results[[wl_key]]$plots$initial_plate +
          labs(title = paste("Initial Plate Layout -", wl, "nm"))
        
        print(wl_plot)
        plots_list[[paste0("wavelength_", wl)]] <- wl_plot
      }
      
      return(invisible(plots_list))
      
    } else {
      wavelength_key <- paste0("wavelength_", wavelength)
      
      if (!wavelength_key %in% names(results)) {
        stop("Wavelength ", wavelength, " not found. Available: ", 
             paste(available_wavelengths, collapse = ", "))
      }
      
      print(results[[wavelength_key]]$plots$initial_plate)
    }
    
  } else {
    print(results$plots$initial_plate)
  }
}


#' Display Growth Curve Plots from Analysis Results
#'
#' This function displays growth curve plots from the results of `analyze_growth_curves()`.
#' It automatically detects whether the input contains single or multiple wavelengths and
#' adapts accordingly. For multi-wavelength results, users can specify a wavelength or
#' choose to display all.
#'
#' @param results A list returned by `analyze_growth_curves()` or one of its wrappers.
#' @param wavelength Optional. Character string specifying the wavelength to display (e.g., `"600"`).
#' Use `"all"` to display all available wavelengths. If `NULL`, the function prompts the user.
#'
#' @return Invisibly returns a list of ggplot objects if `wavelength = "all"`, otherwise prints the selected plot.
#'
#' @details
#' - For multi-wavelength results, the function looks for elements named like `"wavelength_600"`, `"wavelength_420"`, etc.
#' - If no wavelength is specified, the function lists available options.
#' - If a single wavelength is detected, the corresponding plot is shown directly.
#'
#' @seealso [analyze_growth_curves()], [ggplot2::ggplot()]
#'
#' @examples
#' \dontrun{
#' results <- analyze_growth_curves("data/plate_reader_output.csv")
#' show_growth_curves(results) # Auto-detects format
#' show_growth_curves(results, wavelength = "600")
#' show_growth_curves(results, wavelength = "all")
#' }
#'
#' @export
show_growth_curves <- function(results, wavelength = NULL, 
                               max_time_hrs = NULL) {  
  
  # Detect if this is multi-wavelength results
  if ("wavelength_600" %in% names(results) || "wavelength_420" %in% names(results)) {
    
    wavelength_keys <- names(results)[str_detect(names(results), "^wavelength_")]
    available_wavelengths <- str_extract(wavelength_keys, "\\d+")
    
    if (is.null(wavelength)) {
      cat("• Multi-wavelength results detected. Specify wavelength:\n")
      for (wl in available_wavelengths) {
        cat("  show_growth_curves(results, wavelength = '", wl, "')  # ", wl, " nm\n")
      }
      cat("  show_growth_curves(results, wavelength = 'all')  # Show all wavelengths\n")
      return(invisible())
      
    } else if (wavelength == "all") {
      cat("• Showing growth curves for all wavelengths\n")
      
      # Create plots for each wavelength
      plots_list <- list()
      
      for (i in seq_along(wavelength_keys)) {
        wl_key <- wavelength_keys[i]
        wl <- available_wavelengths[i]
        
        cat("• Displaying", wl, "nm growth curves\n")
        
        # Get the plot and add wavelength to title, apply time filter
        if (!is.null(max_time_hrs)) {
          # Create a modified version with time filter
          temp_results <- results[[wl_key]]$corrected_results
          temp_results$corrected_data <- temp_results$corrected_data %>%
            filter(time_hrs <= max_time_hrs)
          temp_results$replicate_stats <- temp_results$replicate_stats %>%
            filter(time_hrs <= max_time_hrs)
          
          wl_plot <- plot_growth_curves_multi(temp_results, wl) +
            labs(title = paste("Growth Curves -", wl, "nm (Corrected) - Time ≤", max_time_hrs, "hrs"))
        } else {
          wl_plot <- results[[wl_key]]$plots$growth_curves +
            labs(title = paste("Growth Curves -", wl, "nm (Corrected)"))
        }
        
        print(wl_plot)
        plots_list[[paste0("wavelength_", wl)]] <- wl_plot
      }
      
      return(invisible(plots_list))
      
    } else {
      # Specific wavelength requested
      wavelength_key <- paste0("wavelength_", wavelength)
      
      if (!wavelength_key %in% names(results)) {
        stop("Wavelength ", wavelength, " not found. Available: ", 
             paste(available_wavelengths, collapse = ", "))
      }
      
      if (!is.null(max_time_hrs)) {
        # Apply time filter
        temp_results <- results[[wavelength_key]]$corrected_results
        temp_results$corrected_data <- temp_results$corrected_data %>%
          filter(time_hrs <= max_time_hrs)
        temp_results$replicate_stats <- temp_results$replicate_stats %>%
          filter(time_hrs <= max_time_hrs)
        
        wl_plot <- plot_growth_curves_multi(temp_results, wavelength) +
          labs(title = paste("Growth Curves -", wavelength, "nm (Corrected) - Time ≤", max_time_hrs, "hrs"))
        print(wl_plot)
      } else {
        print(results[[wavelength_key]]$plots$growth_curves)
      }
    }
    
  } else {
    # Single wavelength results
    if (!is.null(max_time_hrs)) {
      # Apply time filter
      temp_results <- results$corrected_results
      temp_results$corrected_data <- temp_results$corrected_data %>%
        filter(time_hrs <= max_time_hrs)
      temp_results$replicate_stats <- temp_results$replicate_stats %>%
        filter(time_hrs <= max_time_hrs)
      
      growth_plot <- plot_growth_curves(temp_results) +
        labs(title = paste("Growth Curves (Corrected) - Time ≤", max_time_hrs, "hrs"))
      print(growth_plot)
    } else {
      print(results$plots$growth_curves)
    }
  }
}

#' Show experimental layout validation
show_layout_validation <- function(results) {
  print(results$plots$qc_plots$layout_validation)
}

#' Display Quality Control (QC) Plots from Growth Curve Analysis
#'
#' This function displays a set of diagnostic plots used to assess the quality of growth curve data
#' and correction procedures. It supports both single and multi-wavelength results and allows users
#' to view QC plots for a specific wavelength or all wavelengths.
#'
#' @param results A list returned by `analyze_growth_curves()` or one of its wrappers.
#' @param wavelength Optional. Character string specifying the wavelength to display (e.g., `"600"`).
#' Use `"all"` to display QC plots for all available wavelengths. If `NULL`, the function prompts the user.
#'
#' @return Invisibly prints a set of QC plots to the console.
#'
#' @details
#' The following QC plots are displayed for each wavelength:
#' - **Broth Stability**: Checks consistency of broth-only wells over time.
#' - **Blank Stability**: Assesses baseline noise in blank wells.
#' - **NP Kinetics**: Visualizes nanoparticle behavior over time.
#' - **Before vs After Correction**: Compares raw and corrected OD values.
#'
#' @seealso [analyze_growth_curves()], [quick_check()]
#'
#' @examples
#' \dontrun{
#' results <- analyze_growth_curves("data/plate_reader_output.csv")
#' show_qc_plots(results)
#' show_qc_plots(results, wavelength = "600")
#' show_qc_plots(results, wavelength = "all")
#' }
#'
#' @export
show_qc_plots <- function(results, wavelength = NULL) {
  
  if ("wavelength_600" %in% names(results) || "wavelength_420" %in% names(results)) {
    
    wavelength_keys <- names(results)[str_detect(names(results), "^wavelength_")]
    available_wavelengths <- str_extract(wavelength_keys, "\\d+")
    
    if (is.null(wavelength)) {
      cat("• Multi-wavelength results detected. Specify wavelength:\n")
      for (wl in available_wavelengths) {
        cat("  show_qc_plots(results, wavelength = '", wl, "')  # ", wl, " nm\n")
      }
      cat("  show_qc_plots(results, wavelength = 'all')  # Show all wavelengths\n")
      return(invisible())
      
    } else if (wavelength == "all") {
      cat("• Showing QC plots for all wavelengths\n")
      
      for (i in seq_along(wavelength_keys)) {
        wl_key <- wavelength_keys[i]
        wl <- available_wavelengths[i]
        
        cat("\n Quality Control Plots -", wl, "nm:\n")
        
        cat("1. Broth Contamination Detection:\n")
        print(results[[wl_key]]$plots$qc_plots$broth_contamination)
        
        cat("\n2. NP Kinetics:\n")
        print(results[[wl_key]]$plots$qc_plots$np_kinetics)
        
        cat("\n3. Before vs After Correction:\n")
        print(results[[wl_key]]$plots$qc_plots$before_after)
        
        cat("\n4. Temperature Profile:\n")  # NEW
        print(results[[wl_key]]$plots$qc_plots$temperature)  # NEW
      }
      
    } else {
      wavelength_key <- paste0("wavelength_", wavelength)
      
      if (!wavelength_key %in% names(results)) {
        stop("Wavelength ", wavelength, " not found. Available: ", 
             paste(available_wavelengths, collapse = ", "))
      }
      
      qc_plots <- results[[wavelength_key]]$plots$qc_plots
      
      cat(" Quality Control Plots:\n\n")
      
      cat("1. Broth Contamination Detection:\n")
      print(qc_plots$broth_contamination)
      
      cat("\n2. NP Kinetics:\n")
      print(qc_plots$np_kinetics)
      
      cat("\n3. Before vs After Correction:\n")
      print(qc_plots$before_after)
      
      cat("\n4. Temperature Profile:\n")  # NEW
      print(qc_plots$temperature)  # NEW
    }
    
  } else {
    # Single wavelength
    cat(" Quality Control Plots:\n\n")
    
    cat("1. Broth Contamination Detection:\n")
    print(results$plots$qc_plots$broth_contamination)
    
    cat("\n2. NP Kinetics:\n")
    print(results$plots$qc_plots$np_kinetics)
    
    cat("\n3. Before vs After Correction:\n")
    print(results$plots$qc_plots$before_after)
    
    cat("\n4. Temperature Profile:\n")  # NEW
    print(results$plots$qc_plots$temperature)  # NEW
  }
}


#' Extract Final Corrected Growth Curve Data
#'
#' This function retrieves the final corrected data in long format from the results
#' of a growth curve analysis.
#'
#' @param results A list returned by `analyze_growth_curves()` or one of its wrappers.
#'
#' @return A data frame containing the corrected growth curve data in long format.
#'
#' @seealso [analyze_growth_curves()], [export_wide_format()]
#'
#' @examples
#' \dontrun{
#' results <- analyze_growth_curves("data/plate_reader_output.csv")
#' corrected_data <- get_corrected_data(results)
#' head(corrected_data)
#' }
#'
#' @export
get_corrected_data <- function(results) {
  results$corrected_results$corrected_data
}

#' Get the raw data (no corrections)
get_raw_data <- function(results) {
  results$corrected_results$raw_data
}


#' Extract Experiment Metadata
#'
#' This function retrieves metadata associated with the experiment from the analysis results.
#' Metadata may include information such as experiment date, plate layout, instrument settings,
#' or user-defined annotations.
#'
#' @param results A list returned by `analyze_growth_curves()` or one of its wrappers.
#'
#' @return A list or data frame containing experiment metadata, depending on how it was stored.
#'
#' @seealso [analyze_growth_curves()], [get_corrected_data()]
#'
#' @examples
#' \dontrun{
#' results <- analyze_growth_curves("data/plate_reader_output.csv")
#' metadata <- get_experiment_metadata(results)
#' str(metadata)
#' }
#'
#' @export
get_experiment_metadata <- function(results) {
  results$processed_data$metadata
}

#' Show experiment overview
show_experiment_overview <- function(results) {
  if (!"summaries" %in% names(results)) {
    cat("️  Summaries not available. Run analyze_growth_curves_with_summaries() first.\n")
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
      dplyr::first()
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


#' Quick Summary Check of Growth Curve Results
#'
#' This function provides a quick diagnostic summary of the processed growth curve data,
#' including data size, time range, sample type distribution, and basic quality checks.
#' It supports both single and multi-wavelength result formats.
#'
#' @param results A list returned by `analyze_growth_curves()` or one of its wrappers.
#'
#' @return Invisibly prints a summary of the dataset to the console.
#'
#' @details
#' - For multi-wavelength results, the function iterates over each wavelength and reports key metrics.
#' - For single-wavelength results, it also checks for potential issues such as negative OD values or unusually high readings.
#' - This function is intended for quick inspection and debugging.
#'
#' @seealso [analyze_growth_curves()], [get_corrected_data()]
#'
#' @examples
#' \dontrun{
#' results <- analyze_growth_curves("data/plate_reader_output.csv")
#' quick_check(results)
#' }
#'
#' @export
quick_check <- function(results) {
  
  cat("• QUICK DATA CHECK\n")
  cat(strrep("=", 30), "\n")
  
  # Detect if this is multi-wavelength results
  if ("wavelength_600" %in% names(results) || "wavelength_420" %in% names(results)) {
    cat("• Multi-wavelength results detected\n")
    
    # Show info for each wavelength
    wavelength_keys <- names(results)[str_detect(names(results), "^wavelength_")]
    
    for (wl_key in wavelength_keys) {
      wavelength <- str_extract(wl_key, "\\d+")
      cat("\n• ", wavelength, " nm:\n")
      
      data <- results[[wl_key]]$corrected_results$corrected_data
      
      cat("  - Data size:", nrow(data), "rows,", length(unique(data$well_id)), "wells\n")
      cat("  - Time range:", round(min(data$time_hrs), 2), "to", 
          round(max(data$time_hrs), 2), "hours\n")
      
      # Sample type breakdown
      sample_counts <- data %>%
        distinct(well_id, sample_type) %>%
        count(sample_type)
      
      cat("  - Wells by type:")
      for (i in 1:nrow(sample_counts)) {
        cat(" ", sample_counts$sample_type[i], "(", sample_counts$n[i], ")")
      }
      cat("\n")
    }
    
    cat("\n• Use quick_check(results$wavelength_600) for individual wavelength details\n")
    
  } else {
    # Single wavelength results
    data <- results$corrected_results$corrected_data
    
    cat("• Data size:", nrow(data), "rows,", length(unique(data$well_id)), "wells\n")
    cat("• Time range:", round(min(data$time_hrs), 2), "to", 
        round(max(data$time_hrs), 2), "hours\n")
    
    # Sample type breakdown
    sample_counts <- data %>%
      distinct(well_id, sample_type) %>%
      count(sample_type)
    
    cat("• Wells by type:\n")
    for (i in 1:nrow(sample_counts)) {
      cat("   ", sample_counts$sample_type[i], ":", sample_counts$n[i], "\n")
    }
    
    # Check for issues
    neg_count <- sum(data$od600_final < 0, na.rm = TRUE)
    high_count <- sum(data$od600_final > 2, na.rm = TRUE)
    
    if (neg_count > 0 || high_count > 0) {
      cat("• Potential issues:\n")
      if (neg_count > 0) cat("   Negative values:", neg_count, "\n")
      if (high_count > 0) cat("   Very high values (>2 OD):", high_count, "\n")
    } else {
      cat("• No obvious data issues detected\n")
    }
  }
}

#' Adapt single wavelength results for standard accessor functions
adapt_single_wavelength_for_accessors <- function(single_result) {
  
  wavelength <- single_result$wavelength
  
  # Get the corrected data and rename columns to standard format
  corrected_data <- single_result$corrected_results$corrected_data
  
  # CLEAN THE DATA FIRST
  corrected_data <- clean_corrected_data(corrected_data)
  
  # Rename wavelength-specific columns to standard od600 format
  od_raw_col <- paste0("od", wavelength, "_raw")
  od_broth_col <- paste0("od", wavelength, "_broth_corrected")
  od_final_col <- paste0("od", wavelength, "_final")
  
  if (od_raw_col %in% names(corrected_data)) {
    corrected_data <- corrected_data %>% rename(od600_raw = !!od_raw_col)
  }
  if (od_broth_col %in% names(corrected_data)) {
    corrected_data <- corrected_data %>% rename(od600_broth_corrected = !!od_broth_col)
  }
  if (od_final_col %in% names(corrected_data)) {
    corrected_data <- corrected_data %>% rename(od600_final = !!od_final_col)
  }
  
  # Add od600 column for compatibility
  if (!"od600" %in% names(corrected_data) && "od600_raw" %in% names(corrected_data)) {
    corrected_data <- corrected_data %>% mutate(od600 = od600_raw)
  }
  
  # Update the corrected results
  adapted_corrected_results <- single_result$corrected_results
  adapted_corrected_results$corrected_data <- corrected_data
  
  # Clean and remove wavelength column from replicate stats
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


# ===== HELPER FUNCTIONS ====
#' Convert time to decimal hours - handles both HH:MM:SS and decimal formats
convert_time_to_hours <- function(time_values) {
  
  sapply(time_values, function(time_val) {
    if (is.na(time_val) || time_val == "") {
      return(NA_real_)
    }
    
    time_str <- as.character(time_val)
    
    # Check if it's already a decimal number
    if (str_detect(time_str, "^[0-9]*\\.?[0-9]+$")) {
      return(as.numeric(time_str))
    }
    
    # Check if it's HH:MM:SS format
    if (str_detect(time_str, "^\\d{1,2}:\\d{2}(:\\d{2})?$")) {
      time_parts <- str_split(time_str, ":")[[1]]
      
      hours <- as.numeric(time_parts[1])
      minutes <- as.numeric(time_parts[2])
      seconds <- if(length(time_parts) == 3) as.numeric(time_parts[3]) else 0
      
      return(hours + minutes/60 + seconds/3600)
    }
    
    # Try to parse as numeric if other formats fail
    numeric_val <- suppressWarnings(as.numeric(time_str))
    return(numeric_val)
  })
}

#' Extract Results for a Specific Wavelength
#'
#' This helper function retrieves the results corresponding to a specific wavelength
#' from a multi-wavelength analysis result. If the input is already a single-wavelength
#' result, it returns the input unchanged.
#'
#' @param results A list returned by `analyze_growth_curves()` or one of its wrappers.
#' @param wavelength Character. The wavelength to extract (e.g., `"600"`).
#'
#' @return A list containing the results for the specified wavelength.
#' If the input is already a single-wavelength result, it is returned as-is.
#'
#' @details
#' - For multi-wavelength results, the function looks for elements named like `"wavelength_600"`.
#' - If the specified wavelength is not found, an informative error is raised listing available options.
#'
#' @seealso [analyze_growth_curves()], [show_growth_curves()]
#'
#' @examples
#' \dontrun{
#' results <- analyze_growth_curves("data/plate_reader_output.csv")
#' wl600 <- get_wavelength(results, "600")
#' }
#'
#' @export
get_wavelength <- function(results, wavelength) {
  wavelength_key <- paste0("wavelength_", wavelength)
  
  if (wavelength_key %in% names(results)) {
    return(results[[wavelength_key]])
  } else if ("corrected_results" %in% names(results)) {
    # Already single wavelength
    return(results)
  } else {
    available <- names(results)[str_detect(names(results), "^wavelength_")]
    stop("Wavelength ", wavelength, " not found. Available: ", paste(available, collapse = ", "))
  }
}

#' Test function to verify Cerillo compatibility
test_cerillo_compatibility <- function(cerillo_file, layout_file = NULL) {
  
  cat(" TESTING CERILLO COMPATIBILITY\n")
  cat("================================\n")
  
  # Test detection
  cat("1. Testing plate reader detection...\n")
  detected_type <- detect_plate_reader_with_preview(cerillo_file)
  
  if (detected_type != "cerillo") {
    cat(" Detection failed\n")
    return(FALSE)
  }
  
  # Test processing
  cat("\n2. Testing basic processing...\n")
  tryCatch({
    if (is.null(layout_file)) {
      results <- analyze_growth_curves(cerillo_file)
    } else {
      results <- analyze_growth_curves(cerillo_file, layout_csv = layout_file)
    }
    cat(" Processing successful\n")
  }, error = function(e) {
    cat(" Processing failed:", e$message, "\n")
    return(FALSE)
  })
  
  # Test key functions
  cat("\n3. Testing key functions...\n")
  
  test_functions <- list(
    "Growth curves" = function() show_growth_curves(results),
    "Plate endpoint" = function() show_plate_endpoint(results),
    "QC plots" = function() show_qc_plots(results),
    "Export format" = function() get_colleague_format(results),
    "Concentration data" = function() {
      concs <- unique(results$corrected_results$corrected_data$concentration)
      get_concentration_data(results, concs[1])
    }
  )
  
  for (test_name in names(test_functions)) {
    tryCatch({
      test_functions[[test_name]]()
      cat("", test_name, "works\n")
    }, error = function(e) {
      cat("", test_name, "failed:", e$message, "\n")
    })
  }
  
  cat("\n COMPATIBILITY TEST COMPLETE\n")
  return(TRUE)
}

#' Show differences between Cerillo and Synergy data structures
compare_data_structures <- function(cerillo_file, synergy_file) {
  
  cat(" COMPARING DATA STRUCTURES\n")
  cat("============================\n")
  
  # Process both files
  cerillo_results <- analyze_growth_curves(cerillo_file)
  synergy_results <- analyze_growth_curves(synergy_file)
  
  # Compare structures
  cat("Cerillo data columns:\n")
  cat(paste(names(cerillo_results$corrected_results$corrected_data), collapse = ", "), "\n\n")
  
  cat("Synergy data columns:\n")
  cat(paste(names(synergy_results$corrected_results$corrected_data), collapse = ", "), "\n\n")
  
  # Compare metadata
  cat("Cerillo metadata:\n")
  str(cerillo_results$processed_data$metadata)
  
  cat("\nSynergy metadata:\n")
  str(synergy_results$processed_data$metadata)
  
  return(list(cerillo = cerillo_results, synergy = synergy_results))
}

#' Comprehensive data cleaning for final results
clean_final_results <- function(results) {
  
  # Clean corrected_data
  if (!is.null(results$corrected_results$corrected_data)) {
    results$corrected_results$corrected_data <- clean_corrected_data(results$corrected_results$corrected_data)
  }
  
  # Clean raw_data if it exists
  if (!is.null(results$corrected_results$raw_data)) {
    results$corrected_results$raw_data <- clean_corrected_data(results$corrected_results$raw_data)
  }
  
  # Clean replicate_stats if it exists
  if (!is.null(results$corrected_results$replicate_stats)) {
    # Check for list columns in replicate stats
    list_columns <- sapply(results$corrected_results$replicate_stats, function(x) any(sapply(x, is.list)))
    
    if (any(list_columns)) {
      for (col_name in names(list_columns)[list_columns]) {
        if (col_name %in% c("mean_od", "sd_od", "se_od", "min_od", "max_od")) {
          results$corrected_results$replicate_stats[[col_name]] <- as.numeric(unlist(results$corrected_results$replicate_stats[[col_name]]))
        } else {
          results$corrected_results$replicate_stats[[col_name]] <- as.character(unlist(results$corrected_results$replicate_stats[[col_name]]))
        }
      }
    }
  }
  
  return(results)
}

#' Remove duplicate time points keeping the one with more replicates
fix_duplicate_timepoints <- function(replicate_stats) {
  
  # Find and remove duplicates
  cleaned_stats <- replicate_stats %>%
    group_by(sample_type, concentration, time_point) %>%
    # If multiple entries for same time_point, keep the one with most replicates
    slice_max(n_replicates, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  removed_rows = nrow(replicate_stats) - nrow(cleaned_stats)
  if (removed_rows > 0) {
    cat("• Removed", removed_rows, "duplicate time point entries\n")
  }
  
  return(cleaned_stats)
}

# ===== SAFE STATISTICAL FUNCTIONS =====

#' Safe range calculation that handles empty vectors
safe_range <- function(x, na.rm = TRUE) {
  if (length(x) == 0 || all(is.na(x))) {
    return(c(NA_real_, NA_real_))
  }
  suppressWarnings(range(x, na.rm = na.rm))
}

#' Safe min calculation
safe_min <- function(x, na.rm = TRUE) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  suppressWarnings(min(x, na.rm = na.rm))
}

#' Safe max calculation
safe_max <- function(x, na.rm = TRUE) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  suppressWarnings(max(x, na.rm = na.rm))
}

#' Safe summary statistics for groups
safe_summarise <- function(.data, ...) {
  suppressWarnings(.data %>% summarise(...))
}

# ===== MAIN ANALYSIS FUNCTIONS =====

#' Analyze with custom plate layout
analyze_with_custom_layout <- function(data_file, layout_file, 
                                       method = "standard", export = TRUE) {
  
  cat(" ANALYSIS WITH CUSTOM LAYOUT\n")
  cat("==============================\n")
  cat("Data file:", basename(data_file), "\n")
  cat("Layout file:", basename(layout_file), "\n\n")
  
  results <- analyze_growth_curves(
    file_path = data_file,
    layout_csv = layout_file,
    correction_method = method,
    export_summaries = export,
    run_diagnostics = TRUE
  )
  
  return(results)
}

#' Enhanced main analysis function for multiple wavelengths with method options
# Modified analyze_growth_curves_multi to accept layout_csv parameter
analyze_growth_curves_multi <- function(file_path, layout_type = "default", layout_csv = NULL,
                                        correction_method = "standard", 
                                        threshold = TRUE, 
                                        orientation = "vertical",
                                        max_time_hrs = NULL) { 
  
  # Detect plate reader type first
  reader_type <- detect_plate_reader_type(file_path)
  
  if (reader_type == "cerillo") {
    cat("\n")
    cat("• === CERILLO PLATE READER ANALYSIS ===\n")
    cat("• File:", basename(file_path), "\n")
    cat("• Correction method:", toupper(correction_method), "\n")
    cat("• Thresholding:", if(threshold) "ENABLED" else "DISABLED", "\n")
    if (!is.null(layout_csv)) {
      # FIXED: Check type before using basename
      if (is.data.frame(layout_csv)) {
        cat("• Custom layout: data frame from plate editor\n")
      } else if (is.character(layout_csv)) {
        cat("• Custom layout:", basename(layout_csv), "\n")
      } else {
        cat("• Custom layout: provided\n")
      }
    }
    cat("\n")
    
    # Process Cerillo data (single wavelength, 600nm)
    cat("• Step 1: Processing Cerillo data...\n")
    processed_data <- process_cerillo_file(file_path, layout_type, 
                                           layout_csv, orientation)  # ADDED orientation
    # Apply time cutoff if specified
    if (!is.null(max_time_hrs)) {
      processed_data$data <- apply_time_cutoff(processed_data$data, max_time_hrs)
    }
    
    # Apply corrections
    cat("• Step 2: Applying corrections...\n")
    corrected_results <- process_with_corrections(processed_data, 
                                                  method = correction_method, 
                                                  threshold = threshold)
    
    # Create visualizations
    cat("• Step 3: Creating visualizations...\n")
    plots <- list(
      growth_curves = plot_growth_curves(corrected_results),
      initial_plate = plot_plate_heatmap(corrected_results, "first", "corrected"),
      final_plate = plot_plate_heatmap(corrected_results, "last", "corrected"),
      plate_composite = plot_plate_heatmap_composite(corrected_results),
      concentrations_panel = plot_concentrations_panel_single(corrected_results, NULL, "sample", "corrected", FALSE),
      qc_plots = create_qc_plots(corrected_results, processed_data)
    )
    
    # Return in single wavelength format (since Cerillo is single wavelength)
    results <- list(
      "wavelength_600" = list(
        processed_data = processed_data,
        corrected_results = corrected_results,
        plots = plots,
        wavelength = "600",
        correction_method = correction_method,
        threshold = threshold
      )
    )
    
    cat("\n• === CERILLO ANALYSIS COMPLETE ===\n")
    cat("• Single wavelength (600nm) processed\n")
    cat("• Access results: results$wavelength_600\n\n")
    
    return(results)
    
  } else {
    # Original Synergy HTX multi-wavelength processing
    cat("\n")
    cat("• === SYNERGY HTX MULTI-WAVELENGTH ANALYSIS ===\n")
    cat("• File:", basename(file_path), "\n")
    cat("• Correction method:", toupper(correction_method), "\n")
    cat("• Thresholding:", if(threshold) "ENABLED" else "DISABLED", "\n")
    if (!is.null(layout_csv)) {
      # FIXED: Check type before using basename
      if (is.data.frame(layout_csv)) {
        cat("• Custom layout: data frame from plate editor\n")
      } else if (is.character(layout_csv)) {
        cat("• Custom layout:", basename(layout_csv), "\n")
      } else {
        cat("• Custom layout: provided\n")
      }
    }
    cat("\n")
    
    # Process raw data with multiple wavelengths AND custom layout
    cat("• Step 1: Processing multi-wavelength data...\n")
    processed_data <- process_synergy_file_multi(file_path, 
                                                 layout_type = layout_type, 
                                                 layout_csv = layout_csv,
                                                 orientation = orientation)  # ADDED orientation
    # Process each wavelength separately
    results_by_wavelength <- list()
    
    # Apply time cutoff to each wavelength if specified
    if (!is.null(max_time_hrs)) {
      for (wavelength_name in names(processed_data$wavelength_data)) {
        processed_data$wavelength_data[[wavelength_name]]$data <- 
          apply_time_cutoff(processed_data$wavelength_data[[wavelength_name]]$data, max_time_hrs)
      }
    }
    
    for (wavelength_name in names(processed_data$wavelength_data)) {
      
      wavelength_processed <- processed_data$wavelength_data[[wavelength_name]]
      wavelength <- wavelength_processed$wavelength
      
      cat("• Step 2: Processing", wavelength, "nm data...\n")
      
      # Apply corrections for this wavelength with method options
      corrected_results <- process_with_corrections_multi(wavelength_processed, wavelength,
                                                          method = correction_method,
                                                          threshold = threshold)
      
      # Create visualizations for this wavelength
      cat("• Step 3: Creating", wavelength, "nm visualizations...\n")
      plots <- list(
        growth_curves = plot_growth_curves_multi(corrected_results, wavelength),
        initial_plate = plot_plate_heatmap_multi(corrected_results, "first", "corrected", wavelength),
        final_plate = plot_plate_heatmap_multi(corrected_results, "last", "corrected", wavelength),
        plate_composite = plot_plate_heatmap_composite_multi(corrected_results, wavelength),
        concentrations_panel = plot_concentrations_panel_single(corrected_results, NULL, "sample", "corrected", FALSE, wavelength),
        qc_plots = create_qc_plots_multi(corrected_results, wavelength_processed, wavelength)
      )
      
      results_by_wavelength[[wavelength_name]] <- list(
        processed_data = wavelength_processed,
        corrected_results = corrected_results,
        plots = plots,
        wavelength = wavelength,
        correction_method = correction_method,
        threshold = threshold
      )
      
      cat("  ", wavelength, "nm analysis complete\n")
    }
    
    cat("\n• === MULTI-WAVELENGTH ANALYSIS COMPLETE ===\n")
    cat("• Wavelengths processed:", length(results_by_wavelength), "\n")
    cat("• Access results: results$wavelength_600, results$wavelength_420, etc.\n\n")
    
    return(results_by_wavelength)
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
                                                 layout_csv = NULL,
                                                 export_summaries = FALSE, 
                                                 run_diagnostics = TRUE) {
  
  # Run main analysis
  results <- analyze_growth_curves(file_path, layout_type, layout_csv)
  
  # Run diagnostics first
  if (run_diagnostics) {
    cat("4️⃣  Running data diagnostics...\n")
    diagnostics <- diagnose_data_issues(results$corrected_results)
    results$diagnostics <- diagnostics
  }
  
  # Generate summaries
  cat("\n5️⃣  Generating summaries...\n")
  suppressMessages({
    summaries <- summarize_replicate_results(results$corrected_results)
  })
  
  # Create report with actual layout data
  layout_data <- results$processed_data$layout
  display_summaries(summaries, layout_data)
  
  # Export if requested
  if (export_summaries) {
    export_filename <- paste0(tools::file_path_sans_ext(basename(file_path)), 
                              "_summaries.xlsx")
    cat(" Exporting summaries to Excel...\n")
    export_summaries_to_excel(summaries, export_filename)
  }
  
  results$summaries <- summaries
  
  cat("\n Analysis complete! Your results are ready.\n")
  
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


#' Analyze Growth Curves from Plate Reader Data
#'
#' This is the main analysis function that handles both single and multi-wavelength
#' growth curve data files. It performs data processing, optional diagnostics, and
#' summary generation.
#'
#' @param file_path Character. Path to the input data file (e.g., CSV or Excel) containing growth curve measurements.
#' @param layout_type Character. Specifies the layout type used in the experiment. Default is `"default"`.
#' @param export_summaries Logical. If `TRUE`, generates and exports summary statistics to an Excel file. Default is `FALSE`.
#' @param run_diagnostics Logical. If `TRUE`, runs diagnostics on the corrected results. Default is `TRUE`.
#' @param correction_method Character. Method used for correcting raw data. Default is `"traditional"`.
#' @param threshold Logical. If `TRUE`, applies thresholding during correction. Default is `TRUE`.
#'
#' @return
#' - If the input contains a single wavelength, returns a list with processed results, diagnostics (if enabled),
#'   and summaries (if enabled).
#' - If the input contains multiple wavelengths, returns a named list of results for each wavelength.
#'
#' @details
#' - Internally calls `analyze_growth_curves_multi()` to process the input file.
#' - For single-wavelength data, adapts the result for compatibility with downstream functions.
#' - Diagnostics are generated using `diagnose_data_issues()`.
#' - Summaries are generated using `summarize_replicate_results()` and optionally exported via `export_summaries_to_excel()`.
#'
#' @seealso [analyze_growth_curves_multi()], [diagnose_data_issues()], [summarize_replicate_results()], [export_summaries_to_excel()]
#'
#' @examples
#' \dontrun{
#' analyze_growth_curves("data/plate_reader_output.csv")
#' analyze_growth_curves("data/plate_reader_output.csv", export_summaries = TRUE)
#' }
#'
#' @export
analyze_growth_curves <- function(file_path, layout_type = "default", 
                                  layout_csv = NULL,
                                  export_summaries = FALSE, 
                                  run_diagnostics = TRUE,
                                  correction_method = "standard",
                                  threshold = TRUE,
                                  orientation = "vertical",
                                  max_time_hrs = NULL) {  # NEW PARAMETER
  
  suppressWarnings({
    
    cat("\n")
    cat("• === GROWTH CURVE ANALYSIS ===\n")
    cat("• File:", basename(file_path), "\n")
    cat("• Correction method:", toupper(correction_method), "\n")
    cat("• Thresholding:", if(threshold) "ENABLED" else "DISABLED", "\n")
    if (!is.null(max_time_hrs)) {
      cat("• Time cutoff:", max_time_hrs, "hours\n")  # NEW
    }
    if (!is.null(layout_csv)) {
      if (is.data.frame(layout_csv)) {
        cat("• Custom layout: data frame from plate editor\n")
      } else if (is.character(layout_csv)) {
        cat("• Custom layout:", basename(layout_csv), "\n")
      } else {
        cat("• Custom layout: provided\n")
      }
    }
    cat("\n")
    
    # Run multi-wavelength analysis
    multi_results <- 
      analyze_growth_curves_multi(file_path, layout_type, layout_csv,
                                  correction_method, threshold, orientation, max_time_hrs)  # PASS PARAMETER
    
    # Rest of function remains the same...
    n_wavelengths <- length(multi_results)
    
    if (n_wavelengths == 1) {
      wavelength_key <- names(multi_results)[1]
      single_result <- multi_results[[wavelength_key]]
      
      cat("• Single wavelength detected - adapting for standard accessor functions\n")
      
      adapted_result <- adapt_single_wavelength_for_accessors(single_result)
      adapted_result <- clean_final_results(adapted_result)
      
      if (run_diagnostics) {
        cat("• Running diagnostics...\n")
        adapted_result$diagnostics <- diagnose_data_issues(adapted_result$corrected_results)
      }
      
      if (export_summaries) {
        cat("• Generating summaries...\n")
        adapted_result$summaries <- summarize_replicate_results(adapted_result$corrected_results)
        
        layout_data <- adapted_result$processed_data$layout
        display_summaries(adapted_result$summaries, layout_data)
        
        export_filename <- paste0(tools::file_path_sans_ext(basename(file_path)), "_summaries.xlsx")
        export_summaries_to_excel(adapted_result$summaries, export_filename)
      }
      
      adapted_result$correction_method <- correction_method
      adapted_result$threshold <- threshold
      adapted_result$max_time_hrs <- max_time_hrs  # STORE PARAMETER
      
      return(adapted_result)
      
    } else {
      cat("• Multi-wavelength data detected\n")
      cat("• Access individual wavelengths: results$wavelength_600, results$wavelength_420, etc.\n")
      
      for (wl_name in names(multi_results)) {
        multi_results[[wl_name]] <- clean_final_results(multi_results[[wl_name]])
      }
      
      if (export_summaries || run_diagnostics) {
        cat("• Note: For multi-wavelength summaries, process individual wavelengths:\n")
        for (wl_name in names(multi_results)) {
          wl <- str_extract(wl_name, "\\d+")
          cat("  - analyze_growth_curves_with_summaries(results$", wl_name, ")\n")
        }
      }
      
      return(multi_results)
    }
  })
}

#' Analyze Growth Curves Using Robust Correction Method
#'
#' This is a convenience wrapper around `analyze_growth_curves()` that applies
#' the `"robust"` correction method with thresholding enabled. It simplifies
#' analysis setup for users who prefer a statistically robust approach.
#'
#' @param file_path Character. Path to the input data file (e.g., CSV or Excel) containing growth curve measurements.
#' @param layout_type Character. Specifies the layout type used in the experiment. Default is `"default"`.
#' @param export_summaries Logical. If `TRUE`, generates and exports summary statistics to an Excel file. Default is `FALSE`.
#' @param run_diagnostics Logical. If `TRUE`, runs diagnostics on the corrected results. Default is `TRUE`.
#'
#' @return A list of processed results, either for a single wavelength or multiple wavelengths, depending on the input data.
#'
#' @seealso [analyze_growth_curves()]
#'
#' @examples
#' \dontrun{
#' analyze_growth_curves_robust("data/plate_reader_output.csv")
#' }
#'
#' @export
analyze_growth_curves_robust <- function(file_path, layout_type = "default", 
                                         layout_csv = NULL,
                                         export_summaries = FALSE, 
                                         run_diagnostics = TRUE) {
  analyze_growth_curves(
    file_path = file_path,
    layout_type = layout_type,
    layout_csv = layout_csv,
    export_summaries = export_summaries,
    run_diagnostics = run_diagnostics,
    correction_method = "robust",
    threshold = TRUE
  )
}


#' Analyze Growth Curves Using Traditional Correction Method
#'
#' This is a convenience wrapper around `analyze_growth_curves()` that applies
#' the `"traditional"` correction method without thresholding. It reflects the
#' default approach and is suitable for general use cases.
#'
#' @param file_path Character. Path to the input data file (e.g., CSV or Excel) containing growth curve measurements.
#' @param layout_type Character. Specifies the layout type used in the experiment. Default is `"default"`.
#' @param export_summaries Logical. If `TRUE`, generates and exports summary statistics to an Excel file. Default is `FALSE`.
#' @param run_diagnostics Logical. If `TRUE`, runs diagnostics on the corrected results. Default is `TRUE`.
#'
#' @return A list of processed results, either for a single wavelength or multiple wavelengths, depending on the input data.
#'
#' @seealso [analyze_growth_curves()]
#'
#' @examples
#' \dontrun{
#' analyze_growth_curves_traditional("data/plate_reader_output.csv")
#' }
#'
#' @export
analyze_growth_curves_traditional <- function(file_path, layout_type = "default", 
                                              layout_csv = NULL,
                                              export_summaries = FALSE, 
                                              run_diagnostics = TRUE) {
  analyze_growth_curves(
    file_path = file_path,
    layout_type = layout_type,
    layout_csv = layout_csv,
    export_summaries = export_summaries,
    run_diagnostics = run_diagnostics,
    correction_method = "traditional",
    threshold = FALSE
  )
}

#' New convenience function for standard method
analyze_growth_curves_standard <- function(file_path, layout_type = "default", 
                                           layout_csv = NULL,
                                           export_summaries = FALSE, 
                                           run_diagnostics = TRUE) {
  analyze_growth_curves(
    file_path = file_path,
    layout_type = layout_type,
    layout_csv = layout_csv,
    export_summaries = export_summaries,
    run_diagnostics = run_diagnostics,
    correction_method = "standard",
    threshold = TRUE
  )
}


#' Run Full Growth Curve Analysis with Summaries
#'
#' This is a convenience wrapper around `analyze_growth_curves()` that simplifies
#' running a full analysis with diagnostics and optional summary export.
#' It automatically enables diagnostics and applies thresholding based on the
#' correction method selected.
#'
#' @param file_path Character. Path to the input data file (e.g., CSV or Excel) containing growth curve measurements.
#' @param export_summaries Logical. If `TRUE`, generates and exports summary statistics to an Excel file. Default is `TRUE`.
#' @param method Character. Correction method to use. Options are:
#'   - `"traditional"`: Single broth well, raw NP values (original colleague method)
#'   - `"robust"`: Outlier detection, median corrections  
#'   - `"standard"`: Contamination-safe with time-matched NP correction (statistician recommended)
#'   Default is `"standard"`.
#' @param threshold Logical. If `TRUE`, applies thresholding to prevent negative values. 
#'   If `NULL`, automatically determined based on method (enabled for robust/standard, disabled for traditional/statistician).
#'
#' @return A list of processed results, either for a single wavelength or multiple wavelengths, depending on the input data.
#'
#' @seealso [analyze_growth_curves()]
#'
#' @examples
#' \dontrun{
#' # Use the recommended standard method
#' run_analysis("data/plate_reader_output.csv")
#' 
#' # Compare different methods
#' results_traditional <- run_analysis("data/file.csv", method = "traditional")
#' results_robust <- run_analysis("data/file.csv", method = "robust")
#' results_standard <- run_analysis("data/file.csv", method = "standard")
#' 
#' # Export summaries for further analysis
#' run_analysis("data/file.csv", method = "standard", export_summaries = TRUE)
#' }
#'
#' @export
run_analysis <- function(file_path, layout_csv = NULL, export_summaries = TRUE, 
                         method = "standard", threshold = NULL, 
                         orientation = "vertical", max_time_hrs = NULL) {  # NEW PARAMETER
  
  # Validate method
  valid_methods <- c("traditional", "robust", "standard", "halfbroth")
  if (!method %in% valid_methods) {
    stop("Method must be one of: ", paste(valid_methods, collapse = ", "))
  }
  
  # Auto-determine thresholding if not specified
  if (is.null(threshold)) {
    threshold <- switch(method,
                        "traditional" = FALSE,
                        "robust" = TRUE,
                        "standard" = TRUE,
                        "halfbroth" = TRUE
    )
  }
  
  cat(" Running full analysis with", toupper(method), "method\n")
  cat("   Thresholding:", if(threshold) "ENABLED" else "DISABLED", "\n")
  cat("   Export summaries:", if(export_summaries) "YES" else "NO", "\n")
  if (!is.null(max_time_hrs)) {
    cat("   Time cutoff:", max_time_hrs, "hours\n")  # SHOW NEW PARAMETER
  }
  if (!is.null(layout_csv)) {
    if (is.data.frame(layout_csv)) {
      cat("   Custom layout: data frame from plate editor\n")
    } else {
      cat("   Custom layout:", basename(layout_csv), "\n")
    }
  }
  cat("\n")
  
  analyze_growth_curves(
    file_path, 
    layout_csv = layout_csv,
    export_summaries = export_summaries, 
    run_diagnostics = TRUE,
    correction_method = method,
    threshold = threshold,
    orientation = orientation,
    max_time_hrs = max_time_hrs  # PASS NEW PARAMETER
  )
}

#' Quick analysis shortcuts for each method
#' 
#' These convenience functions provide one-line access to each correction method
#' with appropriate default settings.

#' @rdname run_analysis
#' @export
run_traditional_analysis <- function(file_path, layout_csv = NULL, export_summaries = TRUE) {
  run_analysis(file_path, layout_csv, export_summaries, method = "traditional", threshold = FALSE)
}

#' @rdname run_analysis  
#' @export
run_robust_analysis <- function(file_path, layout_csv = NULL, export_summaries = TRUE) {
  run_analysis(file_path, layout_csv, export_summaries, method = "robust", threshold = TRUE)
}

#' @rdname run_analysis
#' @export  
run_standard_analysis <- function(file_path, layout_csv = NULL, export_summaries = TRUE) {
  run_analysis(file_path, layout_csv, export_summaries, method = "standard", threshold = TRUE)
}

#' Compare multiple correction methods on the same dataset
#'
#' This function runs all correction methods on the same file and returns
#' a named list with the results, making it easy to compare approaches.
#'
#' @param file_path Character. Path to the input data file.
#' @param export_summaries Logical. Whether to export summaries for each method.
#'
#' @return A named list with results from all four methods.
#'
#' @examples
#' \dontrun{
#' # Compare all methods
#' all_results <- compare_correction_methods("data/plate_reader_output.csv")
#' 
#' # Access individual results
#' traditional_data <- get_corrected_data(all_results$traditional)
#' standard_data <- get_corrected_data(all_results$standard)
#' 
#' # Compare final OD values
#' show_growth_curves(all_results$traditional)
#' show_growth_curves(all_results$standard)
#' }
#'
#' @export
compare_correction_methods <- function(file_path, export_summaries = FALSE) {
  
  cat(" COMPARING ALL CORRECTION METHODS\n")
  cat("===================================\n\n")
  
  methods <- c("traditional", "robust", "standard")
  results <- list()
  
  for (method in methods) {
    cat(" Running", toupper(method), "method...\n")
    
    tryCatch({
      results[[method]] <- run_analysis(
        file_path, 
        export_summaries = export_summaries, 
        method = method
      )
      cat("", str_to_title(method), "method completed successfully\n\n")
    }, error = function(e) {
      cat("", str_to_title(method), "method failed:", e$message, "\n\n")
      results[[method]] <<- NULL
    })
  }
  
  # Summary of successful methods
  successful_methods <- names(results)[!sapply(results, is.null)]
  cat(" COMPARISON COMPLETE\n")
  cat("Successful methods:", paste(successful_methods, collapse = ", "), "\n")
  cat("Failed methods:", paste(setdiff(methods, successful_methods), collapse = ", "), "\n\n")
  
  if (export_summaries) {
    comparison_filename <- paste0(tools::file_path_sans_ext(basename(file_path)), 
                                  "_method_comparison.xlsx")
    cat(" Consider manually combining summaries into:", comparison_filename, "\n")
  }
  
  cat(" USAGE:\n")
  cat("  show_growth_curves(results$traditional)\n")
  cat("  show_growth_curves(results$standard) \n")
  cat("  show_plate_endpoint(results$robust)\n")
  cat("  get_colleague_format(results$statistician)\n\n")
  
  return(results)
}

#' Complete workflow with custom layout
run_custom_analysis <- function(data_file, layout_file = NULL, 
                                method = "standard", export = TRUE) {
  
  if (is.null(layout_file)) {
    cat(" TIP: Create a custom layout with launch_plate_editor()\n")
    cat("Using default layout...\n\n")
    
    results <- run_analysis(data_file, NULL, export, method)
  } else {
    results <- analyze_with_custom_layout(data_file, layout_file, method, export)
  }
  
  return(results)
}

# ===== ADDED CSV FUNCTIONALITY ====
read_synergy_htx_csv_file <- function(file_path, sheet_name = NULL) {
  
  cat(" Reading Synergy HTX CSV file:", basename(file_path), "\n")
  
  # Read entire CSV file
  all_data <- suppressMessages(suppressWarnings(
    read_csv(file_path, col_names = FALSE, col_types = cols(.default = "c"))
  ))
  
  # Extract metadata (rows 1-25)
  metadata <- extract_metadata(all_data[1:25, 1:10])
  
  # Find data boundaries
  data_boundaries <- find_data_boundaries(all_data)
  
  # Read kinetic data
  kinetic_data <- read_kinetic_data_csv(file_path, data_boundaries)
  
  # Process and clean kinetic data
  kinetic_long <- process_kinetic_data_csv(kinetic_data, metadata)
  
  cat(" Successfully read", nrow(kinetic_long), "data points from", 
      length(unique(kinetic_long$well_id)), "wells\n")
  
  return(list(
    metadata = metadata,
    kinetic_data = kinetic_long,
    experiment_start = create_experiment_datetime(metadata)
  ))
}

#' Read Synergy HTX CSV file and extract multiple wavelengths
read_synergy_htx_csv_file_multi <- function(file_path, sheet_name = NULL) {
  
  cat("• Reading Synergy HTX CSV file:", basename(file_path), "\n")
  
  # Read entire CSV file
  all_data <- suppressMessages(suppressWarnings(
    read_csv(file_path, col_names = FALSE, col_types = cols(.default = "c"))
  ))
  
  # Extract metadata (rows 1-25)
  metadata <- extract_metadata(all_data[1:25, 1:10])
  
  # Find all wavelength sections
  wavelength_sections <- find_wavelength_sections_csv(all_data)
  
  # Read and process each wavelength
  wavelength_data <- list()
  experiment_start <- create_experiment_datetime(metadata)
  
  for (section_name in names(wavelength_sections)) {
    wavelength_info <- wavelength_sections[[section_name]]
    wavelength <- wavelength_info$wavelength
    
    # Read kinetic data for this wavelength
    kinetic_data <- read_wavelength_kinetic_data_csv(file_path, wavelength_info)
    
    # Process and clean kinetic data
    kinetic_long <- process_wavelength_kinetic_data_csv(kinetic_data, metadata, wavelength)
    
    wavelength_data[[section_name]] <- kinetic_long
    
    cat("   Processed", nrow(kinetic_long), "data points for", wavelength, "nm\n")
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

#' Read kinetic data section from CSV file
read_kinetic_data_csv <- function(file_path, boundaries) {
  suppressMessages(suppressWarnings(
    read_csv(
      file_path,
      skip = boundaries$start - 1,
      n_max = boundaries$end - boundaries$start + 1,
      col_names = FALSE,
      col_types = cols(.default = "c")
    )
  ))
}

#' Read kinetic data for a specific wavelength from CSV
read_wavelength_kinetic_data_csv <- function(file_path, wavelength_info) {
  
  cat("• Reading", wavelength_info$wavelength, "nm data (rows", 
      wavelength_info$data_start, "to", wavelength_info$data_end, ")\n")
  
  suppressMessages(suppressWarnings(
    read_csv(
      file_path,
      skip = wavelength_info$data_start - 1,
      n_max = wavelength_info$data_end - wavelength_info$data_start + 1,
      col_names = FALSE,
      col_types = cols(.default = "c")
    )
  ))
}

#' Process raw kinetic data from CSV into long format
process_kinetic_data_csv <- function(kinetic_data, metadata) {
  
  # Set column names
  n_cols <- ncol(kinetic_data)
  col_names <- c("time_hrs", "time_formatted", "temperature", 
                 paste0(rep(LETTERS[1:8], each = 12), 
                        sprintf("%02d", rep(1:12, 8))))
  col_names <- col_names[1:n_cols]
  names(kinetic_data) <- col_names
  
  # Create experiment start datetime
  experiment_start <- create_experiment_datetime(metadata)
  
  # Process to long format with CSV-specific time handling
  suppressWarnings({
    kinetic_long <- kinetic_data %>%
      mutate(
        # Handle time - could be HH:MM:SS format or decimal hours
        time_hrs_clean = convert_time_to_hours(time_hrs),  # <- CHECK THIS
        temperature_clean = as.numeric(temperature)
      ) %>%
      filter(!is.na(time_hrs_clean)) %>%
      mutate(datetime = experiment_start + seconds(time_hrs_clean * 3600)) %>%
      select(time_hrs_clean, temperature_clean, datetime, A01:H12) %>%
      pivot_longer(
        cols = A01:H12,
        names_to = "well_id",
        values_to = "od600_raw"
      ) %>%
      mutate(od600 = as.numeric(od600_raw)) %>%
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

#' Process wavelength kinetic data from CSV with adaptive column handling
process_wavelength_kinetic_data_csv <- function(kinetic_data, metadata, wavelength) {
  
  n_cols <- ncol(kinetic_data)
  cat("  - Processing", n_cols, "columns for", wavelength, "nm\n")
  
  # Check if first row is header
  first_row <- kinetic_data[1, ] %>% unlist() %>% na.omit() %>% as.character()
  has_header <- any(str_detect(first_row, "Time|T"))
  
  if (has_header) {
    kinetic_data <- kinetic_data[-1, ]
    cat("  - Removed header row\n")
    cat("  - Header was:", paste(head(first_row, 6), collapse = ", "), "\n")
  }
  
  # ADAPTIVE COLUMN DETECTION (same logic as Excel version)
  if (n_cols == 98) {
    cat("  - 98 columns detected - checking if Col1=Time, Col2=Temp\n")
    
    col1_sample <- convert_time_to_hours(kinetic_data[[1]][1:3])
    col2_sample <- suppressWarnings(as.numeric(kinetic_data[[2]][1:3]))
    
    # Col1 should be time (0-24 hours), Col2 should be temp (30-50C)
    col1_is_time <- all(!is.na(col1_sample)) && all(col1_sample >= 0 & col1_sample <= 24)
    col2_is_temp <- all(!is.na(col2_sample)) && all(col2_sample >= 30 & col2_sample <= 50)
    
    if (col1_is_time && col2_is_temp) {
      cat("  - Standard format: Col1=Time, Col2=Temp - using as-is\n")
      use_data <- kinetic_data
      final_n_cols <- n_cols
    } else {
      cat("  - WARNING: 98 columns but Col1/Col2 don't look like Time/Temp\n")
      cat("  - Col1 sample:", paste(col1_sample, collapse = ", "), "\n")
      cat("  - Col2 sample:", paste(col2_sample, collapse = ", "), "\n")
      use_data <- kinetic_data
      final_n_cols <- n_cols
    }
    
  } else if (n_cols == 99) {
    cat("  - 99 columns detected - checking if Col2=Time, Col3=Temp (Col1=extra)\n")
    
    col1_sample <- kinetic_data[[1]][1:3]
    col2_sample <- convert_time_to_hours(kinetic_data[[2]][1:3])
    col3_sample <- suppressWarnings(as.numeric(kinetic_data[[3]][1:3]))
    
    cat("  - Col1 (potential extra):", paste(col1_sample, collapse = ", "), "\n")
    cat("  - Col2 (potential time):", paste(col2_sample, collapse = ", "), "\n")
    cat("  - Col3 (potential temp):", paste(col3_sample, collapse = ", "), "\n")
    
    # Col2 should be time (0-24 hours), Col3 should be temp (30-50C)
    col2_is_time <- all(!is.na(col2_sample)) && all(col2_sample >= 0 & col2_sample <= 24)
    col3_is_temp <- all(!is.na(col3_sample)) && all(col3_sample >= 30 & col3_sample <= 50)
    
    if (col2_is_time && col3_is_temp) {
      cat("  - Extra column format detected: dropping Col1, using Col2=Time, Col3=Temp\n")
      use_data <- kinetic_data[, -1]  # Drop first column
      final_n_cols <- 98
    } else {
      cat("  - WARNING: 99 columns but Col2/Col3 don't look like Time/Temp\n")
      cat("  - Assuming standard format and hoping for the best\n")
      use_data <- kinetic_data
      final_n_cols <- n_cols
    }
    
  } else {
    cat("  - Unexpected column count:", n_cols, "- using standard processing\n")
    use_data <- kinetic_data
    final_n_cols <- n_cols
  }
  
  # NOW PROCEED WITH STANDARD COLUMN ASSIGNMENT
  if (final_n_cols >= 98) {
    col_names <- c("time_hrs", "temperature", 
                   paste0(rep(LETTERS[1:8], each = 12), 1:12))
    col_names <- col_names[1:final_n_cols]
    well_cols <- col_names[3:98]  # A1-H12
    
  } else if (final_n_cols >= 14) {
    col_names <- c("time_hrs", "temperature", paste0("Col_", 3:final_n_cols))
    well_cols <- col_names[3:length(col_names)]
    
  } else {
    stop("Unexpected number of columns after processing: ", final_n_cols)
  }
  
  names(use_data) <- col_names
  
  cat("  - Final format: Time=", paste(use_data$time_hrs[1:2], collapse = ", "), "\n")
  cat("  - Final format: Temp=", paste(use_data$temperature[1:2], collapse = ", "), "\n")
  cat("  - Well columns identified:", length(well_cols), "\n")
  
  # Create experiment start datetime
  experiment_start <- create_experiment_datetime(metadata)
  
  # Process to long format with CSV time handling
  suppressWarnings({
    kinetic_clean <- use_data %>%
      mutate(
        time_hrs_clean = convert_time_to_hours(time_hrs),  # Use CSV time converter
        temperature_clean = as.numeric(temperature)
      ) %>%
      filter(!is.na(time_hrs_clean))
    
    # Verify final ranges make sense
    time_range <- range(kinetic_clean$time_hrs_clean, na.rm = TRUE)
    temp_range <- range(kinetic_clean$temperature_clean, na.rm = TRUE)
    cat("  - Final time range:", round(time_range[1], 3), "to", round(time_range[2], 3), "hours\n")
    cat("  - Final temp range:", round(temp_range[1], 1), "to", round(temp_range[2], 1), "C\n")
    
    kinetic_long <- kinetic_clean %>%
      mutate(datetime = experiment_start + seconds(time_hrs_clean * 3600)) %>%
      select(time_hrs_clean, temperature_clean, datetime, all_of(well_cols)) %>%
      pivot_longer(
        cols = all_of(well_cols),
        names_to = "well_id_raw",
        values_to = paste0("od", wavelength, "_raw")
      ) %>%
      mutate(
        !!paste0("od", wavelength, "_raw") := as.numeric(.data[[paste0("od", wavelength, "_raw")]]),
        !!paste0("od", wavelength) := .data[[paste0("od", wavelength, "_raw")]],
        well_id = case_when(
          str_detect(well_id_raw, "^[A-H]\\d{1}$") ~ str_replace(well_id_raw, "^([A-H])(\\d)$", "\\10\\2"),
          str_detect(well_id_raw, "^[A-H]\\d{2}$") ~ well_id_raw,
          TRUE ~ well_id_raw
        )
      ) %>%
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
  
  return(kinetic_long)
}

#' Find all wavelength sections in CSV data
find_wavelength_sections_csv <- function(all_data) {
  
  wavelength_sections <- list()
  
  # Look for wavelength markers
  for (i in 1:nrow(all_data)) {
    first_cell <- all_data[[i, 1]]
    if (!is.na(first_cell)) {
      # Check if it's a wavelength marker
      if (str_detect(as.character(first_cell), "^(420|600|\\d{3})$")) {
        wavelength <- as.character(first_cell)
        
        cat("• Found wavelength section:", wavelength, "nm at row", i, "\n")
        
        # Look for the header row (should contain "Time", "T", A1, A2, etc.)
        header_row <- NA
        data_start_row <- NA
        
        for (j in (i + 1):(i + 5)) {  # Check next few rows
          if (j <= nrow(all_data)) {
            # Check if this looks like a header row
            row_content <- all_data[j, ] %>% unlist() %>% na.omit() %>% as.character()
            
            # Look for Time, temperature, and well identifiers
            has_time <- any(str_detect(row_content, "Time"))
            has_temp <- any(str_detect(row_content, "T|temp"))
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
                # Also check for HH:MM:SS format
                if (str_detect(as.character(first_data_cell), "^\\d{1,2}:\\d{2}(:\\d{2})?$")) {
                  data_start_row <- j
                  cat("  - Found data start (time format) at row", data_start_row, "\n")
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


# ===== CERILLO PLATE READER FUNCTIONS =====

#' Read Cerillo CSV file and extract kinetic data
read_cerillo_csv_file <- function(file_path) {
  
  cat(" Reading Cerillo CSV file:", basename(file_path), "\n")
  
  # Read entire CSV file
  all_data <- suppressMessages(suppressWarnings(
    read_csv(file_path, col_names = FALSE, col_types = cols(.default = "c"))
  ))
  
  # Extract metadata from header rows
  metadata <- extract_cerillo_metadata(all_data)
  
  # Find where the actual data starts (after the header section)
  data_start_row <- find_cerillo_data_start(all_data)
  
  # Read the data section
  kinetic_data <- read_cerillo_kinetic_data(file_path, data_start_row)
  
  # Process into long format
  kinetic_long <- process_cerillo_kinetic_data(kinetic_data, metadata)
  
  cat(" Successfully read", nrow(kinetic_long), "data points from", 
      length(unique(kinetic_long$well_id)), "wells\n")
  
  return(list(
    metadata = metadata,
    kinetic_data = kinetic_long,
    experiment_start = create_cerillo_experiment_datetime(metadata)
  ))
}

#' Extract metadata from Cerillo header
#' Safe metadata extraction for Cerillo files
extract_cerillo_metadata <- function(all_data) {
  
  metadata <- list()
  
  # Check if we have enough columns
  if (ncol(all_data) < 2) {
    cat("• Warning: File has only", ncol(all_data), "columns, may not be a valid Cerillo file\n")
    return(metadata)
  }
  
  # Look for metadata in the first ~15 rows
  for (i in 1:min(15, nrow(all_data))) {
    row_data <- all_data[i, ] %>% unlist() %>% na.omit() %>% as.character()
    
    if (length(row_data) >= 2) {
      # Check for key:value pairs
      if (str_detect(row_data[1], ":$")) {
        key <- str_remove(row_data[1], ":$")
        value <- row_data[2]
        
        # Clean up known metadata fields
        metadata[[key]] <- switch(key,
                                  "Serial Number" = value,
                                  "Exp ID" = value,
                                  "Mode" = value,
                                  "Data Schema" = value,
                                  "Experiment Name" = value,
                                  "Measurement Mode" = value,
                                  "Interval (s)" = as.numeric(value),
                                  "Start Timestamp" = as.numeric(value),
                                  "User" = value,
                                  "Plate Type" = value,
                                  value  # default
        )
      }
    }
  }
  
  cat("• Extracted metadata:", length(metadata), "fields\n")
  return(metadata)
}

#' Find where the actual data starts in Cerillo file
find_cerillo_data_start <- function(all_data) {
  
  # Check if we have enough data
  if (nrow(all_data) < 10) {
    stop("File appears to be empty or too small to be a valid Cerillo file")
  }
  
  # Look for the row with "Date and Time" which should be the header
  for (i in 1:min(30, nrow(all_data))) {  # Check more rows
    first_cell <- all_data[[i, 1]]
    if (!is.na(first_cell) && str_detect(as.character(first_cell), "Date and Time")) {
      cat("• Found data header at row", i, "\n")
      return(i + 1)  # Data starts on next row
    }
  }
  
  # Fallback: look for timestamp pattern
  for (i in 1:min(30, nrow(all_data))) {
    first_cell <- all_data[[i, 1]]
    if (!is.na(first_cell) && str_detect(as.character(first_cell), "\\d{2}/\\d{2}/\\d{4}")) {
      cat("• Found data start at row", i, "(timestamp pattern)\n")
      return(i)
    }
  }
  
  # Additional fallback: look for Duration header
  for (i in 1:min(30, nrow(all_data))) {
    row_data <- all_data[i, ] %>% unlist() %>% na.omit() %>% as.character()
    if (any(str_detect(row_data, "Duration.*Hours")) || any(str_detect(row_data, "Duration.*Minutes"))) {
      cat("• Found data header at row", i, "(Duration pattern)\n")
      return(i + 1)
    }
  }
  
  stop("Could not find data start in Cerillo file. This may not be a valid Cerillo output file.")
}

#' Read Cerillo kinetic data section
read_cerillo_kinetic_data <- function(file_path, data_start_row) {
  
  # Read from data start to end of file
  kinetic_data <- suppressMessages(suppressWarnings(
    read_csv(file_path, skip = data_start_row - 1, col_types = cols(.default = "c"))
  ))
  
  cat("• Read", nrow(kinetic_data), "data rows with", ncol(kinetic_data), "columns\n")
  
  return(kinetic_data)
}

#' Process Cerillo kinetic data into long format
process_cerillo_kinetic_data <- function(kinetic_data, metadata) {
  
  # Clean column names
  kinetic_data <- kinetic_data %>% clean_names()
  
  cat("• Column names after cleaning:", paste(head(names(kinetic_data), 10), collapse = ", "), "\n")
  
  # For Excel files, the structure might be very different
  # First column might be date/time, second might be duration, etc.
  
  # Try to identify the key columns
  all_cols <- names(kinetic_data)
  
  # Look for time column (duration in hours)
  time_col <- NULL
  
  # Check column 2 or 3 for time values (often where duration is)
  for (idx in c(2, 3)) {
    if (idx <= length(all_cols)) {
      col_name <- all_cols[idx]
      col_values <- suppressWarnings(as.numeric(kinetic_data[[col_name]]))
      if (sum(!is.na(col_values)) > 0) {
        # Check if values look like time (incrementing, starting near 0)
        if (min(col_values, na.rm = TRUE) >= 0 && 
            max(col_values, na.rm = TRUE) < 1000 &&
            cor(seq_along(col_values[!is.na(col_values)]), 
                col_values[!is.na(col_values)], 
                use = "complete.obs") > 0.9) {
          time_col <- col_name
          cat("• Using column '", col_name, "' as time (hours)\n")
          break
        }
      }
    }
  }
  
  # If no time column found, use row numbers
  if (is.null(time_col)) {
    kinetic_data <- kinetic_data %>%
      mutate(time_hrs_generated = (row_number() - 1) * 0.25)  # Assume 15-minute intervals
    time_col <- "time_hrs_generated"
    cat("• Warning: No time column found, using generated time based on 15-minute intervals\n")
  }
  
  # Look for temperature column
  temp_col <- NULL
  for (idx in 4:min(10, length(all_cols))) {
    col_name <- all_cols[idx]
    col_values <- suppressWarnings(as.numeric(kinetic_data[[col_name]]))
    if (sum(!is.na(col_values)) > 0) {
      temp_range <- range(col_values, na.rm = TRUE)
      # Temperature should be between 20-50C and relatively stable
      if (temp_range[1] >= 20 && temp_range[2] <= 50 && 
          (temp_range[2] - temp_range[1]) < 10) {
        temp_col <- col_name
        cat("• Using column '", col_name, "' as temperature (range: ", 
            round(temp_range[1], 1), "-", round(temp_range[2], 1), "C)\n")
        break
      }
    }
  }
  
  # If no temperature column found, use default
  if (is.null(temp_col)) {
    kinetic_data <- kinetic_data %>%
      mutate(temp_default = 37.0)
    temp_col <- "temp_default"
    cat("• Warning: No temperature column found, using default 37C\n")
  }
  
  # Determine where well data starts
  # Usually it's after metadata columns (date, time, duration, temp, etc.)
  # Let's be conservative and start looking after column 5
  well_start_idx <- 6
  
  # But check if we have enough columns
  if (length(all_cols) < well_start_idx + 10) {
    well_start_idx <- max(3, length(all_cols) - 96)  # Leave room for 96 wells
  }
  
  well_columns <- all_cols[well_start_idx:min(length(all_cols), well_start_idx + 95)]
  cat("• Assuming well data starts at column", well_start_idx, "\n")
  cat("• Processing", length(well_columns), "well columns\n")
  
  # Create experiment start datetime
  experiment_start <- create_cerillo_experiment_datetime(metadata)
  
  # Process to long format
  suppressWarnings({
    kinetic_clean <- kinetic_data %>%
      mutate(
        time_hrs = as.numeric(.data[[time_col]]),
        temperature = as.numeric(.data[[temp_col]]),
        datetime = experiment_start + seconds(time_hrs * 3600)
      ) %>%
      filter(!is.na(time_hrs))
    
    # Convert to long format
    kinetic_long <- kinetic_clean %>%
      select(time_hrs, temperature, datetime, all_of(well_columns)) %>%
      pivot_longer(
        cols = all_of(well_columns),
        names_to = "well_id_raw",
        values_to = "od600_raw"
      ) %>%
      mutate(
        od600_raw = as.numeric(od600_raw),
        od600 = od600_raw,
        # Convert column names to standard well IDs
        well_id = convert_cleaned_names_to_well_ids(well_id_raw)
      ) %>%
      filter(!is.na(od600_raw), is.finite(od600_raw)) %>%
      arrange(datetime, well_id) %>%
      group_by(well_id) %>%
      mutate(
        time_point = row_number(),
        time_elapsed = time_hrs - min(time_hrs, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      select(time_hrs, datetime, well_id, od600, od600_raw, time_point, temperature, time_elapsed)
  })
  
  cat("• Processed", nrow(kinetic_long), "data points\n")
  cat("• Unique wells:", length(unique(kinetic_long$well_id)), "\n")
  cat("• Time points per well:", length(unique(kinetic_long$time_point)), "\n")
  
  return(kinetic_long)
}

#' Convert cleaned column names back to standard well IDs
convert_cleaned_names_to_well_ids <- function(cleaned_names) {
  
  # For Cerillo Excel files, the columns after cleaning might be like "x0_0029_6"
  # We need to map these to A01, A02, ... H12 based on their position
  
  # First check if they already look like well IDs
  if (any(str_detect(cleaned_names, "^[A-Ha-h][0-9]{1,2}$"))) {
    # They already look like well IDs, just standardize them
    return(standardize_well_id(cleaned_names))
  }
  
  # Otherwise, generate well IDs based on position
  # 96-well plate has 8 rows (A-H) and 12 columns (1-12)
  n_wells <- length(cleaned_names)
  
  if (n_wells > 96) {
    cat("• Warning: More than 96 well columns found (", n_wells, "). Using first 96.\n")
    cleaned_names <- cleaned_names[1:96]
    n_wells <- 96
  }
  
  # Generate standard well IDs in order
  rows <- rep(LETTERS[1:8], each = 12)
  cols <- rep(1:12, 8)
  standard_wells <- paste0(rows[1:n_wells], sprintf("%02d", cols[1:n_wells]))
  
  cat("• Mapping", n_wells, "columns to standard well IDs (A01-H12 format)\n")
  
  return(standard_wells)
}

#' Standardize well IDs to A01, A02, etc. format
standardize_well_id <- function(well_ids) {
  well_ids %>%
    str_to_upper() %>%
    str_replace("^([A-H])(\\d)$", "\\10\\2") %>%  # A1 -> A01
    str_replace("^([A-H])(\\d{2})$", "\\1\\2")    # A01 -> A01 (no change)
}

#' Create experiment datetime from Cerillo metadata
create_cerillo_experiment_datetime <- function(metadata) {
  
  # Try to use Start Timestamp if available
  if ("Start Timestamp" %in% names(metadata) && !is.na(metadata[["Start Timestamp"]])) {
    start_timestamp <- metadata[["Start Timestamp"]]
    
    tryCatch({
      # Convert Unix timestamp to datetime
      datetime <- as.POSIXct(start_timestamp, origin = "1970-01-01", tz = "UTC")
      cat(" Cerillo datetime from timestamp:", as.character(datetime), "\n")
      return(datetime)
    }, error = function(e) {
      cat("️ Could not parse Start Timestamp:", start_timestamp, "\n")
    })
  }
  
  # Fallback: try to extract from experiment name if it contains date info
  if ("Experiment Name" %in% names(metadata)) {
    exp_name <- metadata[["Experiment Name"]]
    
    # Look for date pattern in experiment name like "2025-07-08"
    date_match <- str_extract(exp_name, "\\d{4}-\\d{2}-\\d{2}")
    if (!is.na(date_match)) {
      tryCatch({
        datetime <- ymd_hms(paste(date_match, "00:00:00"))
        cat(" Cerillo datetime from experiment name:", as.character(datetime), "\n")
        return(datetime)
      }, error = function(e) {
        cat("️ Could not parse date from experiment name\n")
      })
    }
  }
  
  # Final fallback
  cat("️ Using default datetime for Cerillo data\n")
  return(ymd_hms("2025-01-01 00:00:00"))
}

#' Process Cerillo file with layout integration
process_cerillo_file <- 
  function(file_path, layout_type = "default", 
           layout_csv = NULL, orientation = "vertical") {
    
    cat(" Processing Cerillo file with layout integration\n")
    
    # Detect file type and read accordingly
    file_ext <- tools::file_ext(tolower(file_path))
    
    if (file_ext == "csv") {
      cerillo_data <- read_cerillo_csv_file(file_path)
    } else if (file_ext %in% c("xlsx", "xls")) {
      cerillo_data <- read_cerillo_excel_file(file_path)
    } else {
      stop("Unsupported file format for Cerillo data. Please use .csv, .xlsx, or .xls files.")
    }
    
    # Create layout - use custom if provided, otherwise default
    if (!is.null(layout_csv)) {
      layout <- process_custom_layout(layout_csv)
      
      # Create summary
      layout_summary <- layout %>%
        filter(!is.na(concentration)) %>%
        group_by(concentration, sample_type) %>%
        summarise(
          wells = paste(well_id, collapse = ", "),
          n_replicates = n(),
          .groups = "drop"
        )
      
      layout_info <- list(
        layout = layout,
        summary = layout_summary
      )
      
      cat(" Custom layout loaded for Cerillo:", nrow(layout), "wells configured\n")
      
    } else {
      # Use default layout with orientation
      layout_info <- create_experiment_layout(layout_type, NULL, orientation)
    }
    
    # Merge data with layout
    complete_data <- cerillo_data$kinetic_data %>%
      left_join(layout_info$layout, by = "well_id") %>%
      mutate(
        row_letter = ifelse(is.na(row_letter), str_extract(well_id, "^[A-H]"), row_letter),
        col_number = ifelse(is.na(col_number), as.numeric(str_extract(well_id, "\\d+$")), col_number)
      )
    
    # Verify the merge worked
    missing_layout <- complete_data %>% 
      filter(is.na(sample_type)) %>% 
      distinct(well_id) %>% 
      nrow()
    
    if (missing_layout > 0) {
      cat("• ️ Warning:", missing_layout, "wells missing layout information\n")
    }
    
    # Create summary
    data_summary <- list(
      experiment_start = cerillo_data$experiment_start,
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
    
    return(list(
      metadata = cerillo_data$metadata,
      data = complete_data,
      layout = layout_info$layout,
      summary = data_summary
    ))
  }

#' Detect plate reader type from file content
detect_plate_reader_type <- function(file_path) {
  
  file_ext <- tools::file_ext(tolower(file_path))
  
  cat("• Analyzing file content to determine plate reader type...\n")
  
  if (file_ext == "csv") {
    # For CSV files, read first few lines as text
    first_lines <- readLines(file_path, n = 15)
    content_to_check <- paste(first_lines, collapse = "\n")
    
  } else if (file_ext %in% c("xlsx", "xls")) {
    # For Excel files, read first few rows as text
    tryCatch({
      first_data <- suppressMessages(suppressWarnings(
        read_excel(file_path, col_names = FALSE, n_max = 15)
      ))
      
      # Convert to character and collapse for pattern matching
      content_to_check <- first_data %>%
        mutate(across(everything(), as.character)) %>%
        unite("combined", everything(), sep = " ", na.rm = TRUE) %>%
        pull(combined) %>%
        paste(collapse = "\n")
      
    }, error = function(e) {
      cat("• Warning: Could not read Excel file properly, defaulting to Synergy HTX\n")
      return("synergy_htx")
    })
    
  } else {
    cat("• Unknown file extension, defaulting to Synergy HTX\n")
    return("synergy_htx")
  }
  
  # Define patterns that are specific to each plate reader
  cerillo_patterns <- c(
    "Serial Number:",
    "Exp ID:",
    "Data Schema:",
    "Measurement Mode:",
    "OPTICAL_DENSITY",
    "Start Timestamp:",
    "Scheduled End Timestamp:",
    "Interval \\(s\\):",
    "UNIX Timestamp"  # This column name is very Cerillo-specific
  )
  
  synergy_patterns <- c(
    "Software Version",
    "Experiment File Path:",
    "Protocol File Path:",
    "Plate Number",
    "Date.*Time",  # Synergy format
    "Reader Type:",
    "Reader Serial Number:",
    "Reading Type",
    "Kinetic",
    "Filter Set"
  )
  
  # Check for Cerillo patterns first (more specific)
  cerillo_matches <- sum(sapply(cerillo_patterns, function(pattern) {
    length(str_extract_all(content_to_check, pattern)[[1]])
  }))
  
  # Check for Synergy patterns
  synergy_matches <- sum(sapply(synergy_patterns, function(pattern) {
    length(str_extract_all(content_to_check, pattern)[[1]])
  }))
  
  cat("• Cerillo pattern matches:", cerillo_matches, "\n")
  cat("• Synergy pattern matches:", synergy_matches, "\n")
  
  # Decision logic
  if (cerillo_matches >= 3) {
    cat("• Cerillo plate reader detected\n")
    return("cerillo")
  } else if (synergy_matches >= 2) {
    cat("• Synergy HTX plate reader detected\n")
    return("synergy_htx")
  } else {
    # Additional heuristics for edge cases
    
    # Look for specific column headers that are distinctive
    if (str_detect(content_to_check, "Duration \\(Hours\\).*Duration \\(Minutes\\).*UNIX Timestamp")) {
      cat("• Cerillo detected by column headers\n")
      return("cerillo")
    }
    
    # Look for Synergy-specific time format patterns
    if (str_detect(content_to_check, "Time.*T.*A1.*A2") || 
        str_detect(content_to_check, "\\d{3}\\s*nm")) {  # Wavelength markers
      cat("• Synergy HTX detected by data structure\n")
      return("synergy_htx")
    }
    
    # Final fallback - check file structure
    if (file_ext == "csv") {
      # CSV files with timestamps in MM/DD/YYYY format might be Cerillo
      if (str_detect(content_to_check, "\\d{2}/\\d{2}/\\d{4}.*\\d{1,2}:\\d{2}.*[AP]M")) {
        cat("• Cerillo detected by timestamp format\n")
        return("cerillo")
      }
    }
    
    # Default to Synergy HTX if unclear
    cat("• Ambiguous format - defaulting to Synergy HTX\n")
    return("synergy_htx")
  }
}

#' Enhanced detection with file preview
detect_plate_reader_with_preview <- function(file_path) {
  
  cat("=== PLATE READER DETECTION ===\n")
  cat("File:", basename(file_path), "\n")
  cat("Extension:", tools::file_ext(file_path), "\n\n")
  
  # Get the detection result
  result <- detect_plate_reader_type(file_path)
  
  # Show a preview of the file content for verification
  cat("\n• File content preview:\n")
  file_ext <- tools::file_ext(tolower(file_path))
  
  if (file_ext == "csv") {
    preview_lines <- readLines(file_path, n = 8)
    for (i in seq_along(preview_lines)) {
      cat(sprintf("%2d: %s\n", i, str_trunc(preview_lines[i], 80)))
    }
  } else if (file_ext %in% c("xlsx", "xls")) {
    tryCatch({
      preview_data <- suppressMessages(suppressWarnings(
        read_excel(file_path, col_names = FALSE, n_max = 8)
      ))
      
      for (i in 1:nrow(preview_data)) {
        row_content <- preview_data[i, ] %>% 
          unlist() %>% 
          na.omit() %>% 
          as.character() %>%
          str_trunc(60) %>%
          paste(collapse = " | ")
        cat(sprintf("%2d: %s\n", i, row_content))
      }
    }, error = function(e) {
      cat("Could not preview Excel file\n")
    })
  }
  
  cat("\n• Detection result:", toupper(result), "\n")
  cat("==============================\n\n")
  
  return(result)
}

#' Manual override function for difficult cases
force_plate_reader_type <- function(file_path, reader_type, ...) {
  
  valid_types <- c("synergy_htx", "cerillo")
  if (!reader_type %in% valid_types) {
    stop("reader_type must be one of: ", paste(valid_types, collapse = ", "))
  }
  
  cat(" Forcing plate reader type to:", toupper(reader_type), "\n")
  
  if (reader_type == "cerillo") {
    return(process_cerillo_file(file_path, ...))
  } else {
    # Call the original Synergy processing logic
    return(process_synergy_file_original(file_path, ...))
  }
}

#' Backup the original process_synergy_file function
process_synergy_file_original <- function(file_path, sheet_name = NULL, layout_type = "default", layout_csv = NULL) {
  
  # Detect file type
  file_ext <- tools::file_ext(tolower(file_path))
  
  if (file_ext == "csv") {
    # Read CSV data
    synergy_data <- read_synergy_htx_csv_file(file_path)
  } else if (file_ext %in% c("xlsx", "xls")) {
    # Read Excel data
    synergy_data <- read_synergy_htx_file(file_path, sheet_name)
  } else {
    stop("Unsupported file format. Please use .xlsx, .xls, or .csv files.")
  }
  
  # [Rest of original function unchanged...]
  # FIXED: Create layout - use custom if provided, otherwise default
  if (!is.null(layout_csv)) {
    layout <- process_custom_layout(layout_csv)  # Use your flexible layout function
    
    # Create summary
    layout_summary <- layout %>%
      filter(!is.na(concentration)) %>%
      group_by(concentration, sample_type) %>%
      summarise(
        wells = paste(well_id, collapse = ", "),
        n_replicates = n(),
        .groups = "drop"
      )
    
    layout_info <- list(
      layout = layout,
      summary = layout_summary
    )
    
    cat(" Custom layout loaded:", nrow(layout), "wells configured\n")
    
  } else {
    # Use default layout
    layout_info <- create_experiment_layout(layout_type)
  }
  
  # Merge data with layout
  complete_data <- synergy_data$kinetic_data %>%
    left_join(layout_info$layout, by = "well_id") %>%
    mutate(
      row_letter = ifelse(is.na(row_letter), str_extract(well_id, "^[A-H]"), row_letter),
      col_number = ifelse(is.na(col_number), as.numeric(str_extract(well_id, "\\d+$")), col_number)
    )
  
  # Verify the merge worked
  missing_layout <- complete_data %>% 
    filter(is.na(sample_type)) %>% 
    distinct(well_id) %>% 
    nrow()
  
  if (missing_layout > 0) {
    cat("• ️ Warning:", missing_layout, "wells missing layout information\n")
  }
  
  # Create summary
  data_summary <- create_data_summary(synergy_data, complete_data, layout_info)
  
  return(list(
    metadata = synergy_data$metadata,
    data = complete_data,
    layout = layout_info$layout,
    summary = data_summary
  ))
}

# ===== META FUNCTIONS ====
#' Update Function Parameters in Source Code
#'
#' This function updates the default parameters of a given R function by modifying its source code.
#' It extracts the source code of the function, updates the formals (arguments), and returns a new function
#' with the updated parameters.
#' @param fun A function object whose parameters are to be updated. The function must have source reference information.
#' @param new_params A named list of new or updated default parameter values.
#' @return A new function object with updated default parameters.
#' @details This function relies on the source reference (`srcref`) of the input function. If the function
#' was not parsed from a file or lacks source reference, the update will fail. The function modifies the
#' source code textually and evaluates it in a temporary environment to return the updated function.
#' @examples
#' my_fun <- function(x = 1, y = 2) { x + y }
#' updated_fun <- update_function_parameters(my_fun, list(x = 10))
#' updated_fun() # Returns 12
#'
#' @export
update_function_parameters <- function(fun, new_params = list()) {
  # Get source reference
  srcref <- getSrcref(fun)
  if (is.null(srcref)) {
    stop("Function source not available. Function may not have been parsed from a file.")
  }
  
  # Extract source code
  srcfile <- attr(srcref, "srcfile")
  if (is.null(srcfile) || is.null(srcfile$lines)) {
    stop("Source file information not available.")
  }
  
  start_line <- srcref[1]
  end_line <- srcref[3]
  src_lines <- srcfile$lines[start_line:end_line]
  
  # Remove any NA lines
  src_lines <- src_lines[!is.na(src_lines)]
  full_text <- paste(src_lines, collapse = "\n")
  
  # Update formals
  updated_formals <- formals(fun)
  for (param in names(new_params)) {
    updated_formals[[param]] <- new_params[[param]]
  }
  
  # Create formals string
  if (length(updated_formals) == 0) {
    formals_str <- ""
  } else {
    formals_parts <- mapply(function(name, val) {
      if (missing(val) || (is.symbol(val) && identical(val, quote(expr = )))) {
        name
      } else {
        paste0(name, " = ", paste(deparse(val, control = "all"), collapse = ""))
      }
    }, names(updated_formals), updated_formals, USE.NAMES = FALSE)
    formals_str <- paste(formals_parts, collapse = ", ")
  }
  
  # Pattern that works with your format
  pattern <- "(.*?function\\s*\\()([^)]*)(\\)\\s*\\{.*)"
  
  if (grepl(pattern, full_text, perl = TRUE)) {
    new_text <- gsub(pattern, paste0("\\1", formals_str, "\\3"), full_text, perl = TRUE)
  } else {
    stop("Could not parse function definition")
  }
  
  # Parse and evaluate - but we need to handle the assignment
  # Since the new_text includes "test_func <- function...", we need to evaluate it
  # and then extract the function
  temp_env <- new.env()
  eval(parse(text = new_text), envir = temp_env)
  
  # Get the function name from the original text
  func_name <- gsub("^\\s*(\\w+)\\s*<-.*", "\\1", strsplit(new_text, "\n")[[1]][1])
  new_fun <- get(func_name, envir = temp_env)
  
  return(new_fun)
}


# ===== Actual Data Processing ===== 

setup_workspace("D:\\Desktop_Laptop_Xfer\\Nano-Enabled_Antimicrobials_Working_Group\\Stabryla_Bacterial_Evolution\\NP_Expts")

layout_ev <- launch_plate_editor()

r1 <- analyze_growth_curves(
  file_path = "25-08-06_Entero_Ag_cube_Day9_JC RAW.xlsx",
  layout_csv = layout_ev,
  orientation = "vertical"
)
show_qc_plots(r1, wavelength = "600")
show_growth_curves(r1, wavelength = "600")
show_concentrations_panel(r1, wavelength = "600", max_time_hrs = 15)
show_plate_composite(r1, wavelength = "600")
show_plate_composite(r1, wavelength = "600", include_layout = F, max_time_hrs = 15)
get_colleague_format(r1)


r2 <- analyze_growth_curves(
  file_path = "25-08-07_Entero_Ag_cube_Day10_JC RAW.csv",
  layout_csv = layout_ev,
  orientation = "vertical"
)
show_qc_plots(r2)
show_growth_curves(r2)
show_concentrations_panel(r2)
show_plate_composite(r2)
show_plate_composite(r2, include_layout = F)

list.files(pattern = "Day11")
#[1] "25-08-08_E coli_Ag_cube_Day11_JC RAW.csv"
#[2] "25-08-08_Entero_Ag_cube_Day11_JC RAW.csv"
#[3] "25-08-08_Kleb_Ag_cube_Day11_KK.csv"      
#[4] "25-08-08_Staph_Ag_cube_Day11_KK.csv" 

r11a <- analyze_growth_curves(
  file_path = "25-08-08_E coli_Ag_cube_Day11_JC RAW.csv",
  layout_csv = layout_ev,
  orientation = "vertical"
)
show_qc_plots(r11a)
show_growth_curves(r11a)
show_concentrations_panel(r11a)
show_plate_composite(r11a)
show_plate_composite(r11a, include_layout = F)


r11b <- analyze_growth_curves(
  file_path = "25-08-08_Entero_Ag_cube_Day11_JC RAW.csv",
  layout_csv = layout_ev,
  orientation = "vertical"
)
show_qc_plots(r11b)
show_growth_curves(r11b)
show_concentrations_panel(r11b)
show_plate_composite(r11b)
show_plate_composite(r11b, include_layout = F)

r11c <- analyze_growth_curves(
  file_path = "25-08-08_Kleb_Ag_cube_Day11_KK.csv",
  layout_csv = layout_ev,
  orientation = "vertical"
)
show_qc_plots(r11c)
show_growth_curves(r11c)
show_concentrations_panel(r11c)
show_plate_composite(r11c)
show_plate_composite(r11c, include_layout = F)


r11d <- analyze_growth_curves(
  file_path = "25-08-08_Staph_Ag_cube_Day11_KK.csv",
  layout_csv = layout_ev,
  orientation = "vertical"
)
show_qc_plots(r11d)
show_temperature(r11d)
show_growth_curves(r11d)
show_concentrations_panel(r11d)
show_plate_composite(r11d)
show_plate_composite(r11d, include_layout = F)


list.files(pattern = "Day12")
#[1] "2025-08-09_E coli_cube_Day12_LS.csv"     
#[2] "2025-08-09_Ent_cube_Day12_LS.csv"        
#[3] "2025-08-09_Kleb_bad_Ag_cube_Day12_LS.csv"
#[4] "2025-08-09_Stap_Ag_cube_Day12_LS.csv"    
#[5] "25-08-09_Kleb_Ag_cube_Day12_LS.xlsx" 

r12a <- analyze_growth_curves(
  file_path = "2025-08-09_E coli_cube_Day12_LS.csv",
  layout_csv = layout_ev,
  orientation = "vertical"
)
show_qc_plots(r12a)
show_growth_curves(r12a)
show_concentrations_panel(r12a)
show_plate_composite(r12a)
show_plate_composite(r12a, include_layout = F)

formals(analyze_growth_curves)
r12b <- analyze_growth_curves(
  file_path = "2025-08-09_Ent_cube_Day12_LS.csv" ,
  layout_csv = layout_ev,
  orientation = "vertical",
  max_time_hrs = 18
)
show_qc_plots(r12b)
show_growth_curves(r12b)
show_concentrations_panel(r12b)
show_plate_composite(r12b)
show_plate_composite(r12b, include_layout = F)


r12b2 <- analyze_growth_curves(
  file_path = "2025-08-09_Ent_cube_Day12_LS.csv" ,
  layout_csv = layout_ev,
  orientation = "vertical"  )
show_growth_curves(r12b2)

r12c <- analyze_growth_curves(
  file_path = "25-08-09_Kleb_Ag_cube_Day12_LS.xlsx",
  layout_csv = layout_ev,
  orientation = "vertical"
)
show_qc_plots(r12c, wavelength = "600")
show_growth_curves(r12c, wavelength = "600")
show_concentrations_panel(r12c, wavelength = "600")
plot_growth_curves_multi(r12c$wavelength_600$corrected_results, wavelength = "600")
plot_growth_curves_multi(r12c$wavelength_460$corrected_results, wavelength = "460")

show_plate_composite(r12c, wavelength = "600", include_layout = F)

r12d <- analyze_growth_curves(
  file_path = "2025-08-09_Stap_Ag_cube_Day12_LS.csv",
  layout_csv = layout_ev,
  orientation = "vertical",
  max_time_hrs = 14
)
show_qc_plots(r12d)
show_temperature(r12d, max_time_hrs = 5)
show_growth_curves(r12d)
show_concentrations_panel(r12d)
show_plate_composite(r12d)
show_plate_composite(r12d, include_layout = F)



