# Script to install required packages for the food web diagram

# List of required packages
required_packages <- c(
  # Core data handling and visualization
  "readr",
  "dplyr",
  "ggplot2",
  "gridExtra",
  "ggforce",
  "grid",
  "scales",
  
  # Color palettes
  "viridis",
  "RColorBrewer",
  "wesanderson",
  "ggsci",
  
  # Command line interface
  "optparse",
  
  # Network visualization (optional)
  "igraph",
  "ggraph",
  "tidygraph",
  "patchwork",
  "reshape2",
  
  # Parallel processing (for optimization)
  "parallel",
  "future",
  "future.apply"
)

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  for (package in packages) {
    if (!requireNamespace(package, quietly = TRUE)) {
      cat("Installing package:", package, "\n")
      install.packages(package, repos = "https://cloud.r-project.org")
    } else {
      cat("Package already installed:", package, "\n")
    }
  }
}

# Install packages
install_if_missing(required_packages)

cat("\nAll required packages installed successfully!\n")