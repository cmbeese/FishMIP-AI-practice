# Modified version of foodweb_diagram.R that doesn't auto-run

# Source the original file but capture and discard its output
# to prevent automatic execution
temp <- capture.output({
  source("R_implementation/foodweb_diagram.R")
})

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
  box_scale_factor = 0.18,
  min_box_size = 0.04,
  max_box_size = 0.4,
  arrow_scale = 1.0,
  color_scheme = "trophic",
  text_contrast = TRUE,
  font_size = 3.5,
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
  box_scale_factor = 0.18,
  min_box_size = 0.04,
  max_box_size = 0.4,
  arrow_scale = 1.0,
  color_scheme = "functional",
  text_contrast = TRUE,
  font_size = 3.5,
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
  box_scale_factor = 0.18,
  min_box_size = 0.04,
  max_box_size = 0.4,
  arrow_scale = 1.0,
  color_scheme = "habitat",
  text_contrast = TRUE,
  font_size = 3.5,
  title = "Hauraki Gulf Food Web - Colored by Habitat"
)

cat("R implementation completed successfully!\n")
cat("Enhanced food web diagrams with improved vertical spacing and arrow rendering.\n")
cat("Added grayscale correlation with relationship strength.\n")
cat("Optimized node distribution and improved visual clarity.\n")
cat("Done!\n")