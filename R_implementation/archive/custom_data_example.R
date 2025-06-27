# Example of Using the Food Web Diagram with Custom Data
# This script demonstrates how to create a food web diagram with your own data

# Load required libraries
library(tidyverse)
library(igraph)
library(ggplot2)
library(ggraph)
library(tidygraph)

# Source the main functions
source("foodweb_diagram.R")
source("data_preparation.R")

# Step 1: Create directories for custom data and images
custom_dir <- "custom_data"
if (!dir.exists(custom_dir)) {
  dir.create(custom_dir)
  cat("Created directory for custom data:", custom_dir, "\n")
}

images_dir <- "images"
if (!dir.exists(images_dir)) {
  dir.create(images_dir)
  cat("Created images directory for output files\n")
}

# Step 2: Create custom data files
# This example creates a simple marine food web with 10 species

# Define the groups
groups <- c(
  "Phytoplankton", 
  "Zooplankton", 
  "Small Pelagic Fish", 
  "Medium Pelagic Fish",
  "Large Pelagic Fish", 
  "Benthic Invertebrates", 
  "Demersal Fish", 
  "Sharks", 
  "Seabirds", 
  "Detritus"
)

# Create ecosystem file
cat("Creating custom ecosystem file...\n")
eco_data <- data.frame(
  Group = groups,
  type = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 2),  # 1=producer, 0=consumer, 2=detritus
  B = c(100, 50, 20, 10, 5, 30, 15, 2, 1, 200),  # Biomass
  P.B = c(80, 40, 15, 8, 3, 20, 10, 2, 1, 0),    # Production/Biomass
  Q.B = c(0, 150, 80, 40, 20, 60, 30, 10, 100, 0),  # Consumption/Biomass
  EE = c(0.8, 0.7, 0.6, 0.5, 0.4, 0.7, 0.6, 0.3, 0.2, 1),  # Ecotrophic Efficiency
  P.Q = c(0, 0.27, 0.19, 0.2, 0.15, 0.33, 0.33, 0.2, 0.01, 0),  # Production/Consumption
  Accum = rep(0, 10),  # Accumulation
  Export = rep(0, 10),  # Export
  Fishery = c(0, 0, 0.1, 0.2, 0.3, 0.1, 0.2, 0.1, 0, 0),  # Fishery
  Unassimilated = c(0, 0.2, 0.2, 0.2, 0.2, 0.3, 0.2, 0.2, 0.3, 0)  # Unassimilated
)

# Write ecosystem file
eco_file <- file.path(custom_dir, "custom_ecosystem.out")
write.table(eco_data, eco_file, sep = "\t", row.names = FALSE, quote = FALSE)

# Create diet matrix
cat("Creating custom diet matrix...\n")
diet_matrix <- matrix(0, nrow = 10, ncol = 10)
colnames(diet_matrix) <- groups
rownames(diet_matrix) <- groups

# Set diet compositions
# Zooplankton eats phytoplankton and detritus
diet_matrix["Phytoplankton", "Zooplankton"] <- 0.8
diet_matrix["Detritus", "Zooplankton"] <- 0.2

# Small Pelagic Fish eat zooplankton
diet_matrix["Zooplankton", "Small Pelagic Fish"] <- 0.9
diet_matrix["Detritus", "Small Pelagic Fish"] <- 0.1

# Medium Pelagic Fish eat small pelagic fish and zooplankton
diet_matrix["Small Pelagic Fish", "Medium Pelagic Fish"] <- 0.7
diet_matrix["Zooplankton", "Medium Pelagic Fish"] <- 0.3

# Large Pelagic Fish eat medium pelagic fish and small pelagic fish
diet_matrix["Medium Pelagic Fish", "Large Pelagic Fish"] <- 0.7
diet_matrix["Small Pelagic Fish", "Large Pelagic Fish"] <- 0.3

# Benthic Invertebrates eat detritus and phytoplankton
diet_matrix["Detritus", "Benthic Invertebrates"] <- 0.7
diet_matrix["Phytoplankton", "Benthic Invertebrates"] <- 0.3

# Demersal Fish eat benthic invertebrates and small pelagic fish
diet_matrix["Benthic Invertebrates", "Demersal Fish"] <- 0.8
diet_matrix["Small Pelagic Fish", "Demersal Fish"] <- 0.2

# Sharks eat large pelagic fish, medium pelagic fish, and demersal fish
diet_matrix["Large Pelagic Fish", "Sharks"] <- 0.4
diet_matrix["Medium Pelagic Fish", "Sharks"] <- 0.3
diet_matrix["Demersal Fish", "Sharks"] <- 0.3

# Seabirds eat small pelagic fish and medium pelagic fish
diet_matrix["Small Pelagic Fish", "Seabirds"] <- 0.6
diet_matrix["Medium Pelagic Fish", "Seabirds"] <- 0.4

# Convert to data frame for writing
diet_df <- as.data.frame(diet_matrix)
diet_df$Group <- groups
diet_df <- diet_df[, c("Group", groups)]

# Write diet file
diet_file <- file.path(custom_dir, "custom_diets.out")
write.table(diet_df, diet_file, sep = "\t", row.names = FALSE, quote = FALSE)

# Calculate trophic levels
cat("Calculating trophic levels...\n")
# Function to calculate trophic levels from diet matrix
calculate_trophic_levels <- function(diet_matrix, type) {
  n <- nrow(diet_matrix)
  TL <- rep(1, n)  # Initialize all to 1
  
  # Set producers and detritus to trophic level 1
  producers <- which(type == 1 | type == 2)
  TL[producers] <- 1
  
  # Calculate trophic levels for consumers
  consumers <- which(type == 0)
  
  # Iterative calculation (converges after a few iterations)
  for (iter in 1:10) {
    for (i in consumers) {
      # TL = 1 + weighted average of prey TLs
      prey_proportions <- diet_matrix[, i]
      if (sum(prey_proportions) > 0) {
        TL[i] <- 1 + sum(prey_proportions * TL) / sum(prey_proportions)
      }
    }
  }
  
  return(TL)
}

TL <- calculate_trophic_levels(diet_matrix, eco_data$type)

# Create trophic levels file
tl_data <- data.frame(
  GROUP = groups,
  Trophic_Level = TL,
  Omnivory_Index = c(0, 0.1, 0.1, 0.2, 0.3, 0.2, 0.3, 0.4, 0.2, 0)
)

# Write trophic levels file
tl_file <- file.path(custom_dir, "custom_trophic_levels.out")
write.table(tl_data, tl_file, sep = "\t", row.names = FALSE, quote = FALSE)

# Create x-positions file
cat("Creating x-positions file...\n")
xpos_data <- data.frame(
  Gstr0 = groups,
  xpos = c(0.2, 0.4, 0.6, 0.5, 0.3, 0.8, 0.7, 0.1, 0.9, -9999)  # -9999 for detritus (excluded)
)

# Write x-positions file
xpos_file <- file.path(custom_dir, "custom_xpos.txt")
write.table(xpos_data, xpos_file, sep = "\t", row.names = FALSE, quote = FALSE)

# Step 3: Validate the custom data files
cat("\n=== Validating Custom Data Files ===\n\n")
validation_result <- validate_all_files(eco_file, diet_file, tl_file, xpos_file)

if (!validation_result) {
  cat("\nWARNING: Data validation failed. The visualization may not work correctly.\n")
  cat("Press Enter to continue anyway, or Ctrl+C to abort...\n")
  invisible(readline())
}

# Step 4: Create the food web diagram
cat("\n=== Creating Food Web Diagram with Custom Data ===\n\n")
output_file <- file.path(custom_dir, "custom_foodweb.png")

result <- create_foodweb_diagram(
  key_file = eco_file,
  diet_file = diet_file,
  tl_file = tl_file,
  xpos_file = xpos_file,
  output_file = output_file
)

cat("Food web diagram created:", output_file, "\n")

# Step 5: Create a ggplot2 visualization
cat("\n=== Creating ggplot2 Visualization ===\n\n")

# Extract data from the result
groups <- result$groups
xpos <- result$xpos
TL <- result$TL
B <- result$B
DD <- result$DD
QQ <- result$QQ

# Filter out groups with invalid positions (e.g., -9999)
valid_idx <- which(xpos > -999)
groups <- groups[valid_idx]
xpos <- xpos[valid_idx]
TL <- TL[valid_idx]
B <- result$B[valid_idx]
DD <- DD[valid_idx, valid_idx]
QQ <- QQ[valid_idx, valid_idx]

# Create nodes dataframe
nodes_df <- data.frame(
  id = 1:length(groups),
  label = groups,
  x = xpos,
  y = TL,
  biomass = B,
  type = ifelse(TL == 1, "Producer", 
         ifelse(TL > 3.5, "Top Predator", 
         ifelse(TL > 2.5, "Mid-level Consumer", "Primary Consumer")))
)

# Create edges dataframe with a threshold
create_edges <- function(threshold = 0.02) {
  edges_list <- list()
  edge_count <- 0
  
  for (i in 1:length(groups)) {
    for (j in 1:length(groups)) {
      if (DD[i, j] > threshold) {
        edge_count <- edge_count + 1
        edges_list[[edge_count]] <- data.frame(
          from = i,
          to = j,
          weight = DD[i, j],
          flow = QQ[i, j]
        )
      }
    }
  }
  
  if (edge_count > 0) {
    return(do.call(rbind, edges_list))
  } else {
    return(data.frame(from = integer(0), to = integer(0), weight = numeric(0), flow = numeric(0)))
  }
}

edges_df <- create_edges(threshold = 0.02)

# Create the graph
graph <- tbl_graph(nodes = nodes_df, edges = edges_df, directed = TRUE)

# Create ggplot2 visualization
p <- ggraph(graph, layout = "manual", x = nodes_df$x, y = nodes_df$y) +
  geom_edge_arc(aes(width = weight, alpha = weight), 
               arrow = arrow(length = unit(2, 'mm')), 
               end_cap = circle(3, 'mm'),
               start_cap = circle(3, 'mm')) +
  geom_node_point(aes(size = biomass, color = type), shape = 16) +
  geom_node_text(aes(label = label), repel = TRUE, size = 3) +
  scale_edge_width(range = c(0.1, 2)) +
  scale_edge_alpha(range = c(0.1, 0.8)) +
  scale_size(range = c(2, 10)) +
  scale_color_viridis_d() +
  labs(title = "Custom Food Web",
       subtitle = "Node size represents biomass, edge width represents diet proportion") +
  theme_graph() +
  theme(legend.position = "right")

# Save the ggplot2 visualization
ggplot_file <- file.path(images_dir, "custom_foodweb_ggplot.png")
ggsave(ggplot_file, p, width = 12, height = 8, dpi = 100)
cat("ggplot2 visualization saved to:", ggplot_file, "\n")

# Step 6: Summary
cat("\n=== Summary ===\n\n")
cat("Custom food web diagram creation complete!\n\n")
cat("Files created:\n")
cat("1. Custom data files in", custom_dir, "directory:\n")
cat("   - custom_ecosystem.out\n")
cat("   - custom_diets.out\n")
cat("   - custom_trophic_levels.out\n")
cat("   - custom_xpos.txt\n")
cat("2. Visualizations in", images_dir, "directory:\n")
cat("   -", output_file, "(Basic food web diagram)\n")
cat("   -", ggplot_file, "(ggplot2 visualization)\n")

cat("\nNext steps:\n")
cat("- Modify the custom data files to represent your own ecosystem\n")
cat("- Use the advanced_visualization.R script for more visualization options\n")
cat("- Explore the complete_example.R script for a comprehensive workflow\n")

cat("\nThank you for using the Food Web Diagram tool!\n")