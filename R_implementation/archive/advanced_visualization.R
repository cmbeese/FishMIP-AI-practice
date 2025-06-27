# Advanced Food Web Visualization Options
# This script provides additional visualization options for the Hauraki Gulf food web

# Load required libraries
library(tidyverse)
library(igraph)
library(ggplot2)
library(ggraph)
library(tidygraph)
library(viridis)
library(patchwork)

# Source the main functions
source("foodweb_diagram.R")

# Load the data
key_file <- "data/HG04-key-adj.out"
diet_file <- "data/HG04-diets-adj.out"
tl_file <- "data/HG04-key-adj-out-trophic_levels.out"
xpos_file <- "data/HG04-xpos0.txt"

# Read the data files
eco_data <- read_ecosystem_file(key_file)
diet_matrix <- read_diet_file(diet_file)
tl_data <- read_trophic_levels(tl_file)
xpos_data <- read_xpos(xpos_file)

# Match the group names across files
groups <- eco_data$groups

# Calculate P and Q vectors
P <- eco_data$B * eco_data$PB  # Production
Q <- eco_data$B * eco_data$QB  # Consumption

# Calculate QQ matrix
QQ <- calculate_QQ(P, Q, diet_matrix)

# Filter out groups with invalid positions (e.g., -9999)
valid_idx <- which(xpos_data$xpos > -999)
groups <- groups[valid_idx]
xpos <- xpos_data$xpos[valid_idx]
TL <- tl_data$TL[valid_idx]
B <- eco_data$B[valid_idx]
DD <- diet_matrix[valid_idx, valid_idx]
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

# Create edges dataframe
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

# Function to create different visualizations
create_visualizations <- function() {
  # 1. Standard visualization with manual layout
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
    labs(title = "Hauraki Gulf Food Web",
         subtitle = "Manual layout based on trophic level") +
    theme_graph() +
    theme(legend.position = "right")
  
  # 2. Circular layout visualization
  p2 <- ggraph(graph, layout = "circle") +
    geom_edge_arc(aes(width = weight, alpha = weight, color = flow), 
                 arrow = arrow(length = unit(2, 'mm'))) +
    geom_node_point(aes(size = biomass, color = type), shape = 16) +
    geom_node_text(aes(label = label), repel = TRUE, size = 3) +
    scale_edge_width(range = c(0.1, 2)) +
    scale_edge_alpha(range = c(0.1, 0.8)) +
    scale_edge_color_viridis() +
    scale_size(range = c(2, 10)) +
    scale_color_viridis_d() +
    labs(title = "Hauraki Gulf Food Web",
         subtitle = "Circular layout") +
    theme_graph() +
    theme(legend.position = "right")
  
  # 3. Hierarchical layout based on trophic level
  p3 <- ggraph(graph, layout = "sugiyama") +
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
    labs(title = "Hauraki Gulf Food Web",
         subtitle = "Hierarchical layout") +
    theme_graph() +
    theme(legend.position = "right")
  
  # 4. Force-directed layout
  p4 <- ggraph(graph, layout = "fr") +
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
    labs(title = "Hauraki Gulf Food Web",
         subtitle = "Force-directed layout") +
    theme_graph() +
    theme(legend.position = "right")
  
  # Save individual plots
  ggsave("foodweb_manual_layout.png", p1, width = 12, height = 8, dpi = 100)
  ggsave("foodweb_circular_layout.png", p2, width = 12, height = 8, dpi = 100)
  ggsave("foodweb_hierarchical_layout.png", p3, width = 12, height = 8, dpi = 100)
  ggsave("foodweb_force_directed_layout.png", p4, width = 12, height = 8, dpi = 100)
  
  # Create a combined plot
  combined <- (p1 + p2) / (p3 + p4) +
    plot_annotation(
      title = "Hauraki Gulf Food Web - Different Visualization Approaches",
      subtitle = "Comparing different network layouts for ecological food web visualization",
      caption = "Data from Hauraki Gulf ecosystem model (HG04)"
    )
  
  ggsave("foodweb_all_layouts.png", combined, width = 18, height = 12, dpi = 100)
  
  cat("Visualizations saved to:\n")
  cat("- foodweb_manual_layout.png\n")
  cat("- foodweb_circular_layout.png\n")
  cat("- foodweb_hierarchical_layout.png\n")
  cat("- foodweb_force_directed_layout.png\n")
  cat("- foodweb_all_layouts.png\n")
  
  return(list(p1 = p1, p2 = p2, p3 = p3, p4 = p4, combined = combined))
}

# Create a heatmap visualization of the diet matrix
create_diet_heatmap <- function() {
  # Convert diet matrix to long format
  diet_long <- as.data.frame(DD) %>%
    setNames(groups) %>%
    mutate(predator = groups) %>%
    pivot_longer(-predator, names_to = "prey", values_to = "proportion")
  
  # Create the heatmap
  p <- ggplot(diet_long, aes(x = prey, y = predator, fill = proportion)) +
    geom_tile() +
    scale_fill_viridis() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          panel.grid = element_blank()) +
    labs(title = "Diet Composition Matrix",
         subtitle = "Proportion of prey in predator diet",
         x = "Prey", y = "Predator", fill = "Proportion")
  
  ggsave("diet_matrix_heatmap.png", p, width = 14, height = 12, dpi = 100)
  cat("Diet matrix heatmap saved to: diet_matrix_heatmap.png\n")
  
  return(p)
}

# Create a chord diagram of feeding relationships
create_chord_diagram <- function() {
  library(circlize)
  
  # Set up the chord diagram
  png("foodweb_chord_diagram.png", width = 1200, height = 1200, res = 100)
  
  # Filter to only include significant relationships
  threshold <- 0.05
  DD_filtered <- DD
  DD_filtered[DD_filtered < threshold] <- 0
  
  # Create the chord diagram
  circos.clear()
  circos.par(gap.degree = 2)
  
  # Use custom colors based on trophic level
  tl_colors <- colorRampPalette(c("#2b83ba", "#abdda4", "#fdae61", "#d7191c"))(length(unique(round(TL))))
  node_colors <- tl_colors[as.numeric(cut(TL, breaks = length(tl_colors)))]
  
  # Create the chord diagram
  chordDiagram(
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
  circos.trackPlotRegion(
    track.index = 1,
    panel.fun = function(x, y) {
      sector.index <- get.cell.meta.data("sector.index")
      xlim <- get.cell.meta.data("xlim")
      ylim <- get.cell.meta.data("ylim")
      circos.text(
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
  title("Hauraki Gulf Food Web - Chord Diagram")
  
  # Add legend for trophic levels
  legend(
    "bottomright",
    title = "Trophic Level",
    legend = c("1 (Producers)", "2 (Primary Consumers)", "3 (Secondary Consumers)", "4+ (Top Predators)"),
    fill = tl_colors,
    bty = "n"
  )
  
  dev.off()
  cat("Chord diagram saved to: foodweb_chord_diagram.png\n")
}

# Run the visualizations
visualizations <- create_visualizations()
heatmap <- create_diet_heatmap()
chord_diagram <- create_chord_diagram()

cat("\nAll visualizations completed!\n")