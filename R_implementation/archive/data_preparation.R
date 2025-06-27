# Data Preparation and Validation for Food Web Diagram
# This script helps prepare and validate data files for the food web diagram
# Optimized for efficiency and robustness

# Load required libraries - using minimal imports for efficiency
library(readr)      # For reading files
library(dplyr)      # For data manipulation
library(tools)      # For file utilities

# Source the main functions
source("foodweb_diagram.R")

# Function to check if a file exists and is readable
check_file <- function(file_path) {
  if (!file.exists(file_path)) {
    cat("ERROR: File does not exist:", file_path, "\n")
    return(FALSE)
  }
  
  if (!file_access(file_path, mode = 4)) {
    cat("ERROR: File is not readable:", file_path, "\n")
    return(FALSE)
  }
  
  cat("File exists and is readable:", file_path, "\n")
  return(TRUE)
}

# Function to validate the ecosystem file
validate_ecosystem_file <- function(file_path) {
  if (!check_file(file_path)) {
    return(FALSE)
  }
  
  # Try to read the file
  tryCatch({
    data <- read.delim(file_path, header = TRUE, stringsAsFactors = FALSE)
    
    # Check required columns
    required_cols <- c("Group", "type", "B", "P.B", "Q.B")
    missing_cols <- required_cols[!required_cols %in% names(data)]
    
    if (length(missing_cols) > 0) {
      cat("ERROR: Missing required columns in ecosystem file:", paste(missing_cols, collapse = ", "), "\n")
      return(FALSE)
    }
    
    # Check data types
    if (!is.numeric(data$type) || !is.numeric(data$B) || !is.numeric(data$P.B) || !is.numeric(data$Q.B)) {
      cat("ERROR: Non-numeric values found in numeric columns of ecosystem file\n")
      return(FALSE)
    }
    
    # Check for empty group names
    if (any(data$Group == "")) {
      cat("WARNING: Empty group names found in ecosystem file\n")
    }
    
    cat("Ecosystem file is valid:", file_path, "\n")
    cat("Number of groups:", nrow(data), "\n")
    cat("Group types: Producers:", sum(data$type == 1), 
        ", Consumers:", sum(data$type == 0), 
        ", Detritus:", sum(data$type == 2), "\n")
    
    return(TRUE)
  }, error = function(e) {
    cat("ERROR: Failed to read ecosystem file:", e$message, "\n")
    return(FALSE)
  })
}

# Function to validate the diet file
validate_diet_file <- function(file_path, eco_file_path) {
  if (!check_file(file_path)) {
    return(FALSE)
  }
  
  # Try to read the files
  tryCatch({
    diet_data <- read.delim(file_path, header = TRUE, stringsAsFactors = FALSE)
    eco_data <- read.delim(eco_file_path, header = TRUE, stringsAsFactors = FALSE)
    
    # Check if the number of columns matches the number of groups plus 1 (for the Group column)
    if (ncol(diet_data) != nrow(eco_data) + 1) {
      cat("ERROR: Number of columns in diet file does not match number of groups in ecosystem file\n")
      cat("Diet file columns:", ncol(diet_data) - 1, ", Ecosystem file groups:", nrow(eco_data), "\n")
      return(FALSE)
    }
    
    # Check if the number of rows matches the number of groups
    if (nrow(diet_data) != nrow(eco_data)) {
      cat("ERROR: Number of rows in diet file does not match number of groups in ecosystem file\n")
      cat("Diet file rows:", nrow(diet_data), ", Ecosystem file groups:", nrow(eco_data), "\n")
      return(FALSE)
    }
    
    # Check if the group names match
    diet_groups <- diet_data$Group
    eco_groups <- eco_data$Group
    
    if (!all(diet_groups %in% eco_groups) || !all(eco_groups %in% diet_groups)) {
      cat("ERROR: Group names in diet file do not match group names in ecosystem file\n")
      missing_in_diet <- eco_groups[!eco_groups %in% diet_groups]
      missing_in_eco <- diet_groups[!diet_groups %in% eco_groups]
      
      if (length(missing_in_diet) > 0) {
        cat("Groups in ecosystem file but not in diet file:", paste(missing_in_diet, collapse = ", "), "\n")
      }
      
      if (length(missing_in_eco) > 0) {
        cat("Groups in diet file but not in ecosystem file:", paste(missing_in_eco, collapse = ", "), "\n")
      }
      
      return(FALSE)
    }
    
    # Check if diet proportions are valid (between 0 and 1)
    diet_matrix <- as.matrix(diet_data[, -1])
    if (any(diet_matrix < 0, na.rm = TRUE) || any(diet_matrix > 1, na.rm = TRUE)) {
      cat("ERROR: Diet proportions must be between 0 and 1\n")
      return(FALSE)
    }
    
    # Check if diet proportions sum to approximately 1 for each predator
    row_sums <- rowSums(diet_matrix)
    consumers <- eco_data$type == 0
    consumer_names <- eco_data$Group[consumers]
    
    for (i in 1:length(diet_groups)) {
      if (diet_groups[i] %in% consumer_names) {
        if (abs(row_sums[i] - 1) > 0.01) {
          cat("WARNING: Diet proportions for", diet_groups[i], "sum to", row_sums[i], "instead of 1\n")
        }
      }
    }
    
    cat("Diet file is valid:", file_path, "\n")
    cat("Number of feeding relationships:", sum(diet_matrix > 0), "\n")
    
    return(TRUE)
  }, error = function(e) {
    cat("ERROR: Failed to read diet file:", e$message, "\n")
    return(FALSE)
  })
}

# Function to validate the trophic levels file
validate_trophic_levels_file <- function(file_path, eco_file_path) {
  if (!check_file(file_path)) {
    return(FALSE)
  }
  
  # Try to read the files
  tryCatch({
    tl_data <- read.delim(file_path, header = TRUE, stringsAsFactors = FALSE)
    eco_data <- read.delim(eco_file_path, header = TRUE, stringsAsFactors = FALSE)
    
    # Check required columns
    if (!("GROUP" %in% names(tl_data)) || !("Trophic_Level" %in% names(tl_data))) {
      cat("ERROR: Missing required columns in trophic levels file: GROUP and/or Trophic_Level\n")
      return(FALSE)
    }
    
    # Check if the number of rows matches the number of groups
    if (nrow(tl_data) != nrow(eco_data)) {
      cat("ERROR: Number of rows in trophic levels file does not match number of groups in ecosystem file\n")
      cat("Trophic levels file rows:", nrow(tl_data), ", Ecosystem file groups:", nrow(eco_data), "\n")
      return(FALSE)
    }
    
    # Check if the group names match
    tl_groups <- tl_data$GROUP
    eco_groups <- eco_data$Group
    
    if (!all(tl_groups %in% eco_groups) || !all(eco_groups %in% tl_groups)) {
      cat("ERROR: Group names in trophic levels file do not match group names in ecosystem file\n")
      missing_in_tl <- eco_groups[!eco_groups %in% tl_groups]
      missing_in_eco <- tl_groups[!tl_groups %in% eco_groups]
      
      if (length(missing_in_tl) > 0) {
        cat("Groups in ecosystem file but not in trophic levels file:", paste(missing_in_tl, collapse = ", "), "\n")
      }
      
      if (length(missing_in_eco) > 0) {
        cat("Groups in trophic levels file but not in ecosystem file:", paste(missing_in_eco, collapse = ", "), "\n")
      }
      
      return(FALSE)
    }
    
    # Check if trophic levels are valid (greater than 0)
    if (any(tl_data$Trophic_Level <= 0, na.rm = TRUE)) {
      cat("ERROR: Trophic levels must be greater than 0\n")
      return(FALSE)
    }
    
    cat("Trophic levels file is valid:", file_path, "\n")
    cat("Trophic level range:", min(tl_data$Trophic_Level), "to", max(tl_data$Trophic_Level), "\n")
    
    return(TRUE)
  }, error = function(e) {
    cat("ERROR: Failed to read trophic levels file:", e$message, "\n")
    return(FALSE)
  })
}

# Function to validate the x-positions file
validate_xpos_file <- function(file_path, eco_file_path) {
  if (!check_file(file_path)) {
    return(FALSE)
  }
  
  # Try to read the files
  tryCatch({
    xpos_data <- read.delim(file_path, header = TRUE, stringsAsFactors = FALSE)
    eco_data <- read.delim(eco_file_path, header = TRUE, stringsAsFactors = FALSE)
    
    # Check required columns
    if (!("Gstr0" %in% names(xpos_data)) || !("xpos" %in% names(xpos_data))) {
      cat("ERROR: Missing required columns in x-positions file: Gstr0 and/or xpos\n")
      return(FALSE)
    }
    
    # Check if the number of rows matches the number of groups
    if (nrow(xpos_data) != nrow(eco_data)) {
      cat("ERROR: Number of rows in x-positions file does not match number of groups in ecosystem file\n")
      cat("X-positions file rows:", nrow(xpos_data), ", Ecosystem file groups:", nrow(eco_data), "\n")
      return(FALSE)
    }
    
    # Check if x-positions are valid (between 0 and 1 or -9999 for excluded groups)
    valid_xpos <- xpos_data$xpos >= 0 & xpos_data$xpos <= 1 | xpos_data$xpos == -9999
    if (!all(valid_xpos)) {
      cat("ERROR: X-positions must be between 0 and 1 or -9999 for excluded groups\n")
      return(FALSE)
    }
    
    # Count excluded groups
    excluded <- sum(xpos_data$xpos == -9999)
    
    cat("X-positions file is valid:", file_path, "\n")
    cat("Number of groups with valid positions:", nrow(xpos_data) - excluded, "\n")
    cat("Number of excluded groups:", excluded, "\n")
    
    return(TRUE)
  }, error = function(e) {
    cat("ERROR: Failed to read x-positions file:", e$message, "\n")
    return(FALSE)
  })
}

# Function to validate all files
validate_all_files <- function(key_file, diet_file, tl_file, xpos_file) {
  cat("Validating all data files...\n\n")
  
  # Check if files exist first
  files_exist <- all(file.exists(c(key_file, diet_file, tl_file, xpos_file)))
  if (!files_exist) {
    cat("ERROR: One or more files do not exist\n")
    for (file in c(key_file, diet_file, tl_file, xpos_file)) {
      if (!file.exists(file)) {
        cat("  Missing file:", file, "\n")
      }
    }
    return(FALSE)
  }
  
  # Validate files
  eco_valid <- validate_ecosystem_file(key_file)
  diet_valid <- if (eco_valid) validate_diet_file(diet_file, key_file) else FALSE
  tl_valid <- if (eco_valid) validate_trophic_levels_file(tl_file, key_file) else FALSE
  xpos_valid <- if (eco_valid) validate_xpos_file(xpos_file, key_file) else FALSE
  
  all_valid <- eco_valid && diet_valid && tl_valid && xpos_valid
  
  cat("\nValidation summary:\n")
  cat("Ecosystem file:", if (eco_valid) "VALID" else "INVALID", "\n")
  cat("Diet file:", if (diet_valid) "VALID" else "INVALID", "\n")
  cat("Trophic levels file:", if (tl_valid) "VALID" else "INVALID", "\n")
  cat("X-positions file:", if (xpos_valid) "VALID" else "INVALID", "\n")
  cat("Overall:", if (all_valid) "ALL FILES VALID" else "SOME FILES INVALID", "\n")
  
  return(all_valid)
}

# Function to create a data directory and sample files if needed
create_sample_data <- function() {
  # Create data directory if it doesn't exist
  if (!dir.exists("data")) {
    dir.create("data")
    cat("Created data directory\n")
  }
  
  # Check if files already exist
  key_file <- "data/sample_key.out"
  diet_file <- "data/sample_diets.out"
  tl_file <- "data/sample_trophic_levels.out"
  xpos_file <- "data/sample_xpos.txt"
  
  files_exist <- file.exists(key_file) && file.exists(diet_file) && 
                file.exists(tl_file) && file.exists(xpos_file)
  
  if (files_exist) {
    cat("Sample data files already exist\n")
    return(list(
      key_file = key_file,
      diet_file = diet_file,
      tl_file = tl_file,
      xpos_file = xpos_file
    ))
  }
  
  # Create sample data
  cat("Creating sample data files...\n")
  
  # Define groups
  groups <- c(
    "Phytoplankton", "Zooplankton", "Small Fish", "Medium Fish", 
    "Large Fish", "Sharks", "Seabirds", "Detritus"
  )
  
  # Create ecosystem file
  eco_data <- data.frame(
    Group = groups,
    type = c(1, 0, 0, 0, 0, 0, 0, 2),  # 1=producer, 0=consumer, 2=detritus
    B = c(10, 5, 2, 1, 0.5, 0.2, 0.1, 20),
    P.B = c(100, 50, 10, 5, 2, 1, 0.5, 0),
    Q.B = c(0, 200, 50, 20, 10, 5, 200, 0),
    EE = c(0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 1),
    P.Q = c(0, 0.25, 0.2, 0.25, 0.2, 0.2, 0.0025, 0),
    Accum = rep(0, 8),
    Export = rep(0, 8),
    Fishery = c(0, 0, 0.1, 0.2, 0.3, 0.1, 0, 0),
    Unassimilated = c(0, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0)
  )
  
  # Create diet matrix
  diet_matrix <- matrix(0, nrow = 8, ncol = 8)
  colnames(diet_matrix) <- groups
  rownames(diet_matrix) <- groups
  
  # Set diet compositions
  diet_matrix["Phytoplankton", "Zooplankton"] <- 0.8
  diet_matrix["Detritus", "Zooplankton"] <- 0.2
  
  diet_matrix["Zooplankton", "Small Fish"] <- 0.9
  diet_matrix["Detritus", "Small Fish"] <- 0.1
  
  diet_matrix["Small Fish", "Medium Fish"] <- 0.7
  diet_matrix["Zooplankton", "Medium Fish"] <- 0.3
  
  diet_matrix["Medium Fish", "Large Fish"] <- 0.8
  diet_matrix["Small Fish", "Large Fish"] <- 0.2
  
  diet_matrix["Large Fish", "Sharks"] <- 0.5
  diet_matrix["Medium Fish", "Sharks"] <- 0.3
  diet_matrix["Small Fish", "Sharks"] <- 0.2
  
  diet_matrix["Small Fish", "Seabirds"] <- 0.4
  diet_matrix["Medium Fish", "Seabirds"] <- 0.6
  
  # Convert to data frame for writing
  diet_df <- as.data.frame(diet_matrix)
  diet_df$Group <- groups
  diet_df <- diet_df[, c("Group", groups)]
  
  # Create trophic levels
  tl_data <- data.frame(
    GROUP = groups,
    Trophic_Level = c(1, 2.0, 3.0, 3.7, 4.2, 4.5, 3.8, 1),
    Omnivory_Index = c(0, 0.1, 0.1, 0.2, 0.3, 0.4, 0.2, 0)
  )
  
  # Create x-positions
  xpos_data <- data.frame(
    Gstr0 = groups,
    xpos = c(0.2, 0.4, 0.6, 0.5, 0.3, 0.1, 0.8, -9999)  # -9999 for detritus (excluded)
  )
  
  # Write files
  write.table(eco_data, key_file, sep = "\t", row.names = FALSE, quote = FALSE)
  write.table(diet_df, diet_file, sep = "\t", row.names = FALSE, quote = FALSE)
  write.table(tl_data, tl_file, sep = "\t", row.names = FALSE, quote = FALSE)
  write.table(xpos_data, xpos_file, sep = "\t", row.names = FALSE, quote = FALSE)
  
  cat("Sample data files created:\n")
  cat("- Ecosystem file:", key_file, "\n")
  cat("- Diet file:", diet_file, "\n")
  cat("- Trophic levels file:", tl_file, "\n")
  cat("- X-positions file:", xpos_file, "\n")
  
  return(list(
    key_file = key_file,
    diet_file = diet_file,
    tl_file = tl_file,
    xpos_file = xpos_file
  ))
}

# Example usage
if (interactive()) {
  cat("Data Preparation and Validation Tool\n")
  cat("===================================\n\n")
  
  # Check if the real data files exist
  real_files_exist <- file.exists("data/HG04-key-adj.out") && 
                     file.exists("data/HG04-diets-adj.out") && 
                     file.exists("data/HG04-key-adj-out-trophic_levels.out") && 
                     file.exists("data/HG04-xpos0.txt")
  
  if (real_files_exist) {
    cat("Real data files found. Validating...\n\n")
    validate_all_files(
      key_file = "data/HG04-key-adj.out",
      diet_file = "data/HG04-diets-adj.out",
      tl_file = "data/HG04-key-adj-out-trophic_levels.out",
      xpos_file = "data/HG04-xpos0.txt"
    )
  } else {
    cat("Real data files not found. Creating sample data...\n\n")
    sample_files <- create_sample_data()
    
    cat("\nValidating sample data files...\n\n")
    validate_all_files(
      key_file = sample_files$key_file,
      diet_file = sample_files$diet_file,
      tl_file = sample_files$tl_file,
      xpos_file = sample_files$xpos_file
    )
    
    cat("\nYou can use these sample files to test the food web diagram code.\n")
    cat("To use them, run:\n\n")
    cat('result <- create_foodweb_diagram(\n')
    cat('  key_file = "', sample_files$key_file, '",\n', sep = "")
    cat('  diet_file = "', sample_files$diet_file, '",\n', sep = "")
    cat('  tl_file = "', sample_files$tl_file, '",\n', sep = "")
    cat('  xpos_file = "', sample_files$xpos_file, '",\n', sep = "")
    cat('  output_file = "sample_foodweb.png"\n')
    cat(')\n')
  }
}