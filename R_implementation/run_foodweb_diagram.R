#!/usr/bin/env Rscript
# Food web diagram generator - Command line interface

# Load required library
library(optparse)

# Determine script location and source the implementation file
if (file.exists("R_implementation/foodweb_diagram.R")) {
  source("R_implementation/foodweb_diagram.R")
  main_dir <- "."
} else {
  source("foodweb_diagram.R")
  main_dir <- ".."
}

# Check if running in batch mode
is_batch_mode <- !interactive() && !exists("Rscript_command")

if (is_batch_mode) {
  # In batch mode, just run with defaults and generate all variants
  run_foodweb_diagram(all = TRUE)
  quit(save = "no", status = 0)
} else {
  # For interactive or Rscript mode, use command-line arguments
  data_dir <- file.path(main_dir, "data")
  images_dir <- file.path(main_dir, "images")
  
  # Define command-line options
  option_list <- list(
    make_option("--key-file", type="character", default=file.path(data_dir, "HG04-key-adj.out"),
                help="Path to the ecosystem file"),
    make_option("--diet-file", type="character", default=file.path(data_dir, "HG04-diets-adj.out"),
                help="Path to the diet composition file"),
    make_option("--tl-file", type="character", default=file.path(data_dir, "HG04-key-adj-out-trophic_levels.out"),
                help="Path to the trophic levels file"),
    make_option("--xpos-file", type="character", default=file.path(data_dir, "HG04-xpos0.txt"),
                help="Path to the x-positions file"),
    make_option("--output-file", type="character", default=file.path(images_dir, "R-diagram.png"),
                help="Path to save the output image"),
    make_option("--color-scheme", type="character", default="trophic",
                help="Color scheme for nodes (trophic, functional, habitat)"),
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
  
  # Parse arguments and run
  opts <- parse_args(OptionParser(option_list=option_list))
  
  # Create output directory if needed
  output_dir <- dirname(opts$output_file)
  if (output_dir != "" && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Run with parsed arguments
  run_foodweb_diagram(
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
    text_contrast = opts$text_contrast,
    font_size = opts$font_size,
    title = opts$title,
    all = opts$all
  )
}