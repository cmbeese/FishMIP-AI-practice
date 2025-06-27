# Complete Example for Hauraki Gulf Food Web Diagram in R
# This script demonstrates the complete workflow from data validation to visualization

# Load required libraries
library(tidyverse)
library(igraph)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(viridis)
library(patchwork)

# Source all the component scripts
source("foodweb_diagram.R")
source("data_preparation.R")

# Create images directory if it doesn't exist
images_dir <- "images"
if (!dir.exists(images_dir)) {
  dir.create(images_dir)
  cat("Created images directory for output files\n")
}

# Set file paths - use either real data or sample data
use_real_data <- TRUE  # Set to FALSE to use sample data instead

if (use_real_data) {
  # Check if real data files exist
  real_files_exist <- file.exists("data/HG04-key-adj.out") &&
                     file.exists("data/HG04-diets-adj.out") &&
                     file.exists("data/HG04-key-adj-out-trophic_levels.out") &&
                     file.exists("data/HG04-xpos0.txt")
  
  if (real_files_exist) {
    key_file <- "data/HG04-key-adj.out"
    diet_file <- "data/HG04-diets-adj.out"
    tl_file <- "data/HG04-key-adj-out-trophic_levels.out"
    xpos_file <- "data/HG04-xpos0.txt"
    output_prefix <- file.path(images_dir, "hauraki_gulf")
  } else {
    cat("Real data files not found. Switching to sample data.\n")
    use_real_data <- FALSE
  }
}

if (!use_real_data) {
  # Create sample data if needed
  sample_files <- create_sample_data()
  key_file <- sample_files$key_file
  diet_file <- sample_files$diet_file
  tl_file <- sample_files$tl_file
  xpos_file <- sample_files$xpos_file
  output_prefix <- file.path(images_dir, "sample")
}

# Step 1: Validate the data files
cat("\n=== Step 1: Data Validation ===\n\n")
validation_result <- validate_all_files(key_file, diet_file, tl_file, xpos_file)

if (!validation_result) {
  cat("\nWARNING: Data validation failed. The visualization may not work correctly.\n")
  cat("Press Enter to continue anyway, or Ctrl+C to abort...\n")
  invisible(readline())
}

# Step 2: Create the basic food web diagram
cat("\n=== Step 2: Creating Basic Food Web Diagram ===\n\n")
output_file <- paste0(output_prefix, "_foodweb.png")

# Ensure the images directory exists
if (!dir.exists(dirname(output_file))) {
  dir.create(dirname(output_file), recursive = TRUE)
  cat("Created directory:", dirname(output_file), "\n")
}

result <- create_foodweb_diagram(
  key_file = key_file,
  diet_file = diet_file,
  tl_file = tl_file,
  xpos_file = xpos_file,
  output_file = output_file
)

cat("Basic food web diagram created:", output_file, "\n")

# Step 3: Create alternative visualizations
cat("\n=== Step 3: Creating Alternative Visualizations ===\n\n")

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
B <- B[valid_idx]
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

# 3.1 Create a ggplot2 visualization
cat("Creating ggplot2 visualization...\n")
p1 <- ggraph(graph, layout = "manual", x = nodes_df$x, y = nodes_df$y) +
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
  labs(title = paste(ifelse(use_real_data, "Hauraki Gulf", "Sample"), "Food Web"),
       subtitle = "Node size represents biomass, edge width represents diet proportion") +
  theme_graph() +
  theme(legend.position = "right")

ggsave(paste0(output_prefix, "_foodweb_ggplot.png"), p1, width = 12, height = 8, dpi = 100)
cat("ggplot2 visualization saved to:", paste0(output_prefix, "_foodweb_ggplot.png"), "\n")

# 3.2 Create a heatmap of the diet matrix
cat("Creating diet matrix heatmap...\n")
diet_long <- as.data.frame(DD) %>%
  setNames(groups) %>%
  mutate(predator = groups) %>%
  pivot_longer(-predator, names_to = "prey", values_to = "proportion")

p2 <- ggplot(diet_long, aes(x = prey, y = predator, fill = proportion)) +
  geom_tile() +
  scale_fill_viridis() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid = element_blank()) +
  labs(title = paste(ifelse(use_real_data, "Hauraki Gulf", "Sample"), "Diet Composition Matrix"),
       subtitle = "Proportion of prey in predator diet",
       x = "Prey", y = "Predator", fill = "Proportion")

ggsave(paste0(output_prefix, "_diet_matrix_heatmap.png"), p2, width = 14, height = 12, dpi = 100)
cat("Diet matrix heatmap saved to:", paste0(output_prefix, "_diet_matrix_heatmap.png"), "\n")

# 3.3 Create a circular layout visualization
cat("Creating circular layout visualization...\n")
p3 <- ggraph(graph, layout = "circle") +
  geom_edge_arc(aes(width = weight, alpha = weight, color = flow), 
               arrow = arrow(length = unit(2, 'mm'))) +
  geom_node_point(aes(size = biomass, color = type), shape = 16) +
  geom_node_text(aes(label = label), repel = TRUE, size = 3) +
  scale_edge_width(range = c(0.1, 2)) +
  scale_edge_alpha(range = c(0.1, 0.8)) +
  scale_edge_color_viridis() +
  scale_size(range = c(2, 10)) +
  scale_color_viridis_d() +
  labs(title = paste(ifelse(use_real_data, "Hauraki Gulf", "Sample"), "Food Web"),
       subtitle = "Circular layout") +
  theme_graph() +
  theme(legend.position = "right")

ggsave(paste0(output_prefix, "_foodweb_circular.png"), p3, width = 12, height = 8, dpi = 100)
cat("Circular layout visualization saved to:", paste0(output_prefix, "_foodweb_circular.png"), "\n")

# Step 4: Create a combined visualization
cat("\n=== Step 4: Creating Combined Visualization ===\n\n")

# Create a hierarchical layout based on trophic level
p4 <- ggraph(graph, layout = "sugiyama") +
  geom_edge_link(aes(width = weight, alpha = weight), 
                arrow = arrow(length = unit(2, 'mm')),
                end_cap = circle(3, 'mm'),
                start_cap = circle(3, 'mm')) +
  geom_node_point(aes(size = biomass, color = type), shape = 16) +
  geom_node_text(aes(label = label), repel = TRUE, size = 3) +
  scale_edge_width(range = c(0.1, 2)) +
  scale_edge_alpha(range = c(0.1, 0.8)) +
  scale_size(range = c(2, 10)) +
  scale_color_viridis_d() +
  labs(title = "Hierarchical Layout",
       subtitle = "Based on trophic relationships") +
  theme_graph() +
  theme(legend.position = "none")

# Create a combined plot
combined <- (p1 + p3) / (p4 + p2) +
  plot_annotation(
    title = paste(ifelse(use_real_data, "Hauraki Gulf", "Sample"), "Food Web - Multiple Visualization Approaches"),
    subtitle = "Comparing different ways to visualize ecological food webs",
    caption = paste("Data from", ifelse(use_real_data, "Hauraki Gulf ecosystem model (HG04)", "sample ecosystem model"))
  )

ggsave(paste0(output_prefix, "_foodweb_combined.png"), combined, width = 18, height = 12, dpi = 100)
cat("Combined visualization saved to:", paste0(output_prefix, "_foodweb_combined.png"), "\n")

# Step 5: Create a chord diagram if the circlize package is available
cat("\n=== Step 5: Creating Chord Diagram ===\n\n")
if (requireNamespace("circlize", quietly = TRUE)) {
  cat("Creating chord diagram...\n")
  
  # Set up the chord diagram
  png(paste0(output_prefix, "_foodweb_chord.png"), width = 1200, height = 1200, res = 100)
  
  # Filter to only include significant relationships
  threshold <- 0.05
  DD_filtered <- DD
  DD_filtered[DD_filtered < threshold] <- 0
  
  # Create the chord diagram
  circlize::circos.clear()
  circlize::circos.par(gap.degree = 2)
  
  # Use custom colors based on trophic level
  tl_colors <- colorRampPalette(c("#2b83ba", "#abdda4", "#fdae61", "#d7191c"))(length(unique(round(TL))))
  node_colors <- tl_colors[as.numeric(cut(TL, breaks = length(tl_colors)))]
  
  # Create the chord diagram
  circlize::chordDiagram(
    DD_filtered,
    directional = TRUE,
    direction.type = "diffHeight",
    link.arr.type = "big.arrow",
    grid.col = node_colors,
    annotationTrack = "grid",
    preAllocateTracks = list(track.height = 0.1),
    annotationTrackHeight = c(0.05, 0.1),
    link.sort = TRUE,
    link.decreasing = FALSE
  )
  
  # Add labels
  circlize::circos.trackPlotRegion(
    track.index = 1,
    panel.fun = function(x, y) {
      sector.index <- circlize::get.cell.meta.data("sector.index")
      xlim <- circlize::get.cell.meta.data("xlim")
      ylim <- circlize::get.cell.meta.data("ylim")
      circlize::circos.text(
        mean(xlim), ylim[1],
        sector.index,
        facing = "clockwise",
        adj = c(0, 0.5),
        cex = 0.8
      )
    },
    bg.border = NA
  )
  
  # Add title
  title(paste(ifelse(use_real_data, "Hauraki Gulf", "Sample"), "Food Web - Chord Diagram"))
  
  # Add legend for trophic levels
  legend(
    "bottomright",
    title = "Trophic Level",
    legend = c("1 (Producers)", "2 (Primary Consumers)", "3 (Secondary Consumers)", "4+ (Top Predators)"),
    fill = tl_colors,
    bty = "n"
  )
  
  dev.off()
  cat("Chord diagram saved to:", paste0(output_prefix, "_foodweb_chord.png"), "\n")
} else {
  cat("Chord diagram creation skipped: circlize package not available.\n")
  cat("Install it with: install.packages('circlize')\n")
}

# Step 6: Summary and next steps
cat("\n=== Step 6: Summary ===\n\n")
cat("Food web diagram creation complete!\n\n")
cat("Files created:\n")
cat("1.", output_file, "(Basic food web diagram)\n")
cat("2.", paste0(output_prefix, "_foodweb_ggplot.png"), "(ggplot2 visualization)\n")
cat("3.", paste0(output_prefix, "_diet_matrix_heatmap.png"), "(Diet matrix heatmap)\n")
cat("4.", paste0(output_prefix, "_foodweb_circular.png"), "(Circular layout)\n")
cat("5.", paste0(output_prefix, "_foodweb_combined.png"), "(Combined visualization)\n")
if (requireNamespace("circlize", quietly = TRUE)) {
  cat("6.", paste0(output_prefix, "_foodweb_chord.png"), "(Chord diagram)\n")
}

cat("\nNext steps:\n")
cat("- Explore the advanced_visualization.R script for more visualization options\n")
cat("- Modify the foodweb_diagram.R script to customize the visualization parameters\n")
cat("- Use the data_preparation.R script to validate and prepare your own data\n")

cat("\nThank you for using the Hauraki Gulf Food Web Diagram tool!\n")