#!/usr/bin/env Rscript
# Run script for the enhanced food web diagram in R
# This script provides a command-line interface to generate enhanced food web diagrams
# with various customization options.

# Load required libraries
library(optparse)

# Source the enhanced food web diagram implementation
source("R_implementation/enhanced_foodweb_diagram.R")

# Define command-line options
option_list <- list(
  make_option("--key-file", type="character", default="data/HG04-key-adj.out",
              help="Path to the ecosystem file"),
  make_option("--diet-file", type="character", default="data/HG04-diets-adj.out",
              help="Path to the diet composition file"),
  make_option("--tl-file", type="character", default="data/HG04-key-adj-out-trophic_levels.out",
              help="Path to the trophic levels file"),
  make_option("--xpos-file", type="character", default="data/HG04-xpos0.txt",
              help="Path to the x-positions file"),
  make_option("--output-file", type="character", default="images/R-enhanced-diagram.png",
              help="Path to save the output image"),
  make_option("--color-scheme", type="character", default="trophic",
              help="Color scheme for nodes (trophic, functional, habitat, or none)"),
  make_option("--palette", type="character", default="default",
              help="Color palette to use (default, nature, brewer, viridis, wes, npg)"),
  make_option("--box-scale", type="double", default=0.18,
              help="Scaling factor for node sizes"),
  make_option("--min-box-size", type="double", default=0.04,
              help="Minimum node size"),
  make_option("--max-box-size", type="double", default=0.4,
              help="Maximum node size"),
  make_option("--arrow-scale", type="double", default=1.0,
              help="Scaling factor for arrow widths"),
  make_option("--text-contrast", type="logical", default=TRUE,
              help="Add contrast to text labels"),
  make_option("--font-size", type="double", default=3.5,
              help="Base font size for text labels"),
  make_option("--title", type="character", default=NULL,
              help="Title for the diagram"),
  make_option("--all", type="logical", default=FALSE, action="store_true",
              help="Generate all color scheme variants")
)

# Parse command-line arguments
opt_parser <- OptionParser(option_list=option_list)
opts <- parse_args(opt_parser)

# Create output directory if it doesn't exist
output_dir <- dirname(opts$output_file)
if (output_dir != "" && !dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

# Run the diagram creation
if (opts$all) {
  # Generate all color scheme variants
  color_schemes <- c("trophic", "functional", "habitat")  # Removed "none" which was causing an error
  
  # Debug output
  cat("Generating all color scheme variants:", paste(color_schemes, collapse = ", "), "\n")
  for (scheme in color_schemes) {
    # Create output filename based on color scheme
    base_name <- tools::file_path_sans_ext(opts$output_file)
    ext <- tools::file_ext(opts$output_file)
    output_file <- paste0(base_name, "-", scheme, ".", ext)
    
    # Debug output
    cat("Output file for", scheme, "scheme:", output_file, "\n")
    
    # Create title based on color scheme
    if (!is.null(opts$title)) {
      title <- paste0(opts$title, " - ", tools::toTitleCase(scheme), " Coloring")
    } else {
      title <- paste0("Food Web Diagram - ", tools::toTitleCase(scheme), " Coloring")
    }
    
    # Run the diagram creation
    cat("Generating diagram with", scheme, "coloring...\n")
    result <- create_enhanced_foodweb_diagram(
      key_file = opts$key_file,
      diet_file = opts$diet_file,
      tl_file = opts$tl_file,
      xpos_file = opts$xpos_file,
      output_file = output_file,
      box_scale_factor = opts$box_scale,
      min_box_size = opts$min_box_size,
      max_box_size = opts$max_box_size,
      arrow_scale = opts$arrow_scale,
      color_scheme = scheme,
      palette_name = opts$palette,
      text_contrast = opts$text_contrast,
      font_size = opts$font_size,
      title = title
    )
    
    cat("Successfully created diagram:", output_file, "\n")
  }
} else {
  # Run the diagram creation with specified arguments
  result <- create_enhanced_foodweb_diagram(
    key_file = opts$key_file,
    diet_file = opts$diet_file,
    tl_file = opts$tl_file,
    xpos_file = opts$xpos_file,
    output_file = opts$output_file,
    box_scale_factor = opts$box_scale,
    min_box_size = opts$min_box_size,
    max_box_size = opts$max_box_size,
    arrow_scale = opts$arrow_scale,
    color_scheme = opts$color_scheme,
    palette_name = opts$palette,
    text_contrast = opts$text_contrast,
    font_size = opts$font_size,
    title = opts$title
  )
  
  cat("Successfully created diagram:", opts$output_file, "\n")
}

cat("Done!\n")