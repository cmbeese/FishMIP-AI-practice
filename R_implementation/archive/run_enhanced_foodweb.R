#!/usr/bin/env Rscript
# Run script for the enhanced food web diagram in R
# This script provides a command-line interface to generate food web diagrams
# with various customization options.

# Load required libraries
library(optparse)

# Source the food web diagram implementation with correct path
source("R_implementation/foodweb_diagram.R")

# Create output directory if it doesn't exist
if (!dir.exists("images")) {
  dir.create("images", recursive = TRUE)
  cat("Created output directory: images\n")
}

# Run the food web diagram creation
cat("Generating enhanced food web diagrams...\n")

# Create the version with trophic level coloring
cat("Generating diagram with trophic level coloring...\n")
result_trophic <- create_foodweb_diagram(
  key_file = "data/HG04-key-adj.out",
  diet_file = "data/HG04-diets-adj.out",
  tl_file = "data/HG04-key-adj-out-trophic_levels.out",
  xpos_file = "data/HG04-xpos0.txt",
  output_file = "images/R-trophic.png",
  box_scale_factor = 0.18,  # Scaling factor for node sizes based on biomass
  min_box_size = 0.04,      # Minimum node size to ensure visibility
  max_box_size = 0.4,       # Maximum node size to prevent overcrowding
  arrow_scale = 1.0,        # Scaling factor for arrow widths
  color_scheme = "trophic", # Color nodes by trophic level
  text_contrast = TRUE,     # Add contrast to text labels for better readability
  font_size = 3.5,          # Base font size for text labels
  title = "Hauraki Gulf Food Web - Colored by Trophic Level"
)

# Create the version with functional group coloring
cat("Generating diagram with functional group coloring...\n")
result_functional <- create_foodweb_diagram(
  key_file = "data/HG04-key-adj.out",
  diet_file = "data/HG04-diets-adj.out",
  tl_file = "data/HG04-key-adj-out-trophic_levels.out",
  xpos_file = "data/HG04-xpos0.txt",
  output_file = "images/R-functional.png",
  box_scale_factor = 0.18,      # Scaling factor for node sizes based on biomass
  min_box_size = 0.04,          # Minimum node size to ensure visibility
  max_box_size = 0.4,           # Maximum node size to prevent overcrowding
  arrow_scale = 1.0,            # Scaling factor for arrow widths
  color_scheme = "functional",  # Color nodes by functional group
  text_contrast = TRUE,         # Add contrast to text labels for better readability
  font_size = 3.5,              # Base font size for text labels
  title = "Hauraki Gulf Food Web - Colored by Functional Group"
)

# Create the version with habitat-based coloring
cat("Generating diagram with habitat-based coloring...\n")
result_habitat <- create_foodweb_diagram(
  key_file = "data/HG04-key-adj.out",
  diet_file = "data/HG04-diets-adj.out",
  tl_file = "data/HG04-key-adj-out-trophic_levels.out",
  xpos_file = "data/HG04-xpos0.txt",
  output_file = "images/R-habitat.png",
  box_scale_factor = 0.18,   # Scaling factor for node sizes based on biomass
  min_box_size = 0.04,       # Minimum node size to ensure visibility
  max_box_size = 0.4,        # Maximum node size to prevent overcrowding
  arrow_scale = 1.0,         # Scaling factor for arrow widths
  color_scheme = "habitat",  # Color nodes by habitat
  text_contrast = TRUE,      # Add contrast to text labels for better readability
  font_size = 3.5,           # Base font size for text labels
  title = "Hauraki Gulf Food Web - Colored by Habitat"
)

cat("R implementation completed successfully!\n")
cat("Enhanced food web diagrams with improved vertical spacing and arrow rendering.\n")
cat("Added grayscale correlation with relationship strength.\n")
cat("Optimized node distribution and improved visual clarity.\n")
cat("Done!\n")