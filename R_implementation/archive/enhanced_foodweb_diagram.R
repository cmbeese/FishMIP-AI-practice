# Food Web Diagram in R
# This script creates a publication-ready food web diagram with:
# - Color coding based on functional groups or trophic levels
# - Legend for node sizes and arrow thicknesses
# - Improved text readability
# - Customizable parameters

# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggforce)  # For geom_circle
library(gridExtra)
library(grid)
library(scales)

# Optional color palette libraries - load if available
if (requireNamespace("RColorBrewer", quietly = TRUE)) {
  library(RColorBrewer)
  RCOLORBREWER_AVAILABLE <- TRUE
} else {
  RCOLORBREWER_AVAILABLE <- FALSE
}

if (requireNamespace("viridis", quietly = TRUE)) {
  library(viridis)
  VIRIDIS_AVAILABLE <- TRUE
} else {
  VIRIDIS_AVAILABLE <- FALSE
}

if (requireNamespace("wesanderson", quietly = TRUE)) {
  library(wesanderson)
  WESANDERSON_AVAILABLE <- TRUE
} else {
  WESANDERSON_AVAILABLE <- FALSE
}

if (requireNamespace("ggsci", quietly = TRUE)) {
  library(ggsci)
  GGSCI_AVAILABLE <- TRUE
} else {
  GGSCI_AVAILABLE <- FALSE
}

# Source the base functions from the existing implementation
source("R_implementation/foodweb_diagram.R")

# Function to determine node colors based on trophic level or functional group
get_node_colors <- function(groups, TL, type_values, color_scheme = "trophic", palette_name = "default") {
  if (color_scheme == "trophic") {
    # Create color bins based on trophic levels
    tl_bins <- c(1.0, 2.0, 3.0, 4.0, 5.0)
    tl_labels <- c("Producers (TL 1)", "Primary Consumers (TL 2)", 
                  "Secondary Consumers (TL 3)", "Tertiary Consumers (TL 4+)", 
                  "Top Predators (TL 5+)")
    
    # Choose color palette based on palette_name
    if (palette_name == "default") {
      # Viridis-inspired palette (colorblind-friendly)
      colors_palette <- c("#440154", "#3b528b", "#21908c", "#5dc963", "#fde725")
    } else if (palette_name == "nature") {
      # Nature-inspired palette
      colors_palette <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e")
    } else if (RCOLORBREWER_AVAILABLE && palette_name == "brewer") {
      # RColorBrewer palette
      colors_palette <- RColorBrewer::brewer.pal(5, "Set1")
    } else if (VIRIDIS_AVAILABLE && palette_name == "viridis") {
      # Viridis palette
      colors_palette <- viridis::viridis(5)
    } else if (WESANDERSON_AVAILABLE && palette_name == "wes") {
      # Wes Anderson palette
      colors_palette <- wesanderson::wes_palette("Zissou1", 5)
    } else if (GGSCI_AVAILABLE && palette_name == "npg") {
      # Nature Publishing Group palette
      colors_palette <- ggsci::pal_npg("nrc")(5)
    } else {
      # Default to a standard R color palette
      colors_palette <- hcl.colors(5, "Spectral")
    }
    
    # Assign colors based on trophic level
    colors <- character(length(TL))
    for (i in 1:length(TL)) {
      level <- TL[i]
      if (level < tl_bins[2]) {
        colors[i] <- colors_palette[1]
      } else if (level < tl_bins[3]) {
        colors[i] <- colors_palette[2]
      } else if (level < tl_bins[4]) {
        colors[i] <- colors_palette[3]
      } else if (level < tl_bins[5]) {
        colors[i] <- colors_palette[4]
      } else {
        colors[i] <- colors_palette[5]
      }
    }
    
    # Create color map for legend
    color_map <- setNames(colors_palette, tl_labels)
    
  } else if (color_scheme == "functional") {
    # Define functional groups based on type values and group names
    # Type 0 = consumer, Type 1 = producer, Type 2 = detritus
    functional_groups <- character(length(groups))
    
    for (i in 1:length(groups)) {
      group <- tolower(groups[i])
      type_val <- type_values[i]
      
      if (type_val == 1.0) {
        functional_groups[i] <- "Producer"
      } else if (type_val == 2.0) {
        functional_groups[i] <- "Detritus"
      } else if (grepl("fish|shark", group)) {
        functional_groups[i] <- "Fish"
      } else if (grepl("bird|cetacean", group)) {
        functional_groups[i] <- "Top Predator"
      } else if (grepl("zoo", group)) {
        functional_groups[i] <- "Zooplankton"
      } else if (grepl("bacteria", group)) {
        functional_groups[i] <- "Bacteria"
      } else if (grepl("bivalve|gastropod", group)) {
        functional_groups[i] <- "Mollusc"
      } else if (grepl("crab|crayfish", group)) {
        functional_groups[i] <- "Crustacean"
      } else {
        functional_groups[i] <- "Other Invertebrate"
      }
    }
    
    # Get unique functional groups
    present_groups <- unique(functional_groups)
    num_groups <- length(present_groups)
    
    # Choose color palette based on palette_name
    if (palette_name == "default") {
      # Viridis-inspired palette (colorblind-friendly)
      func_colors <- c(
        "Producer" = "#440154",
        "Detritus" = "#414487",
        "Fish" = "#2a788e",
        "Top Predator" = "#22a884",
        "Zooplankton" = "#7ad151",
        "Bacteria" = "#fde725",
        "Mollusc" = "#7570b3",
        "Crustacean" = "#d95f02",
        "Other Invertebrate" = "#e7298a"
      )
    } else if (palette_name == "nature") {
      # Nature-inspired palette
      func_colors <- c(
        "Producer" = "#8cc751",       # Fresh green
        "Detritus" = "#8d6e63",       # Warm brown
        "Fish" = "#5da9e9",           # Ocean blue
        "Top Predator" = "#d64161",   # Deep red
        "Zooplankton" = "#fee781",    # Soft yellow
        "Bacteria" = "#bdbdbd",       # Silver gray
        "Mollusc" = "#f48fb1",        # Soft pink
        "Crustacean" = "#90caf9",     # Light blue
        "Other Invertebrate" = "#ce93d8" # Lavender
      )
    } else if (RCOLORBREWER_AVAILABLE && palette_name == "brewer") {
      # RColorBrewer palette
      pal <- RColorBrewer::brewer.pal(min(9, num_groups), "Set1")
      func_colors <- setNames(pal[1:num_groups], present_groups)
    } else if (VIRIDIS_AVAILABLE && palette_name == "viridis") {
      # Viridis palette
      pal <- viridis::viridis(num_groups)
      func_colors <- setNames(pal, present_groups)
    } else if (WESANDERSON_AVAILABLE && palette_name == "wes") {
      # Wes Anderson palette
      pal <- wesanderson::wes_palette("Darjeeling1", num_groups, type = "continuous")
      func_colors <- setNames(pal, present_groups)
    } else if (GGSCI_AVAILABLE && palette_name == "npg") {
      # Nature Publishing Group palette
      pal <- ggsci::pal_npg("nrc")(min(10, num_groups))
      func_colors <- setNames(pal, present_groups)
    } else {
      # Default to a standard R color palette
      pal <- hcl.colors(num_groups, "Spectral")
      func_colors <- setNames(pal, present_groups)
    }
    
    # Assign colors based on functional groups
    colors <- func_colors[functional_groups]
    
    # Create color map for legend (only include groups that are present)
    color_map <- func_colors
    
  } else if (color_scheme == "habitat") {
    # Define habitat/environment groups based on group names and type values
    habitat_groups <- character(length(groups))
    
    for (i in 1:length(groups)) {
      group <- tolower(groups[i])
      type_val <- type_values[i]
      
      if (grepl("pelagic|tuna|mackerel", group)) {
        habitat_groups[i] <- "Pelagic"
      } else if (grepl("demersal|snapper|gurnard|flatfish", group)) {
        habitat_groups[i] <- "Demersal"
      } else if (grepl("reef", group)) {
        habitat_groups[i] <- "Reef"
      } else if (grepl("benthic|benthos|crab|star", group)) {
        habitat_groups[i] <- "Benthic"
      } else if (grepl("zoo|phyto|plankton", group)) {
        habitat_groups[i] <- "Planktonic"
      } else if (type_val == 1.0) {
        habitat_groups[i] <- "Primary Producer"
      } else if (type_val == 2.0) {
        habitat_groups[i] <- "Detritus"
      } else if (grepl("bird|cetacean", group)) {
        habitat_groups[i] <- "Air-breathing"
      } else {
        habitat_groups[i] <- "Other"
      }
    }
    
    # Get unique habitat groups
    present_groups <- unique(habitat_groups)
    num_groups <- length(present_groups)
    
    # Choose color palette based on palette_name
    if (palette_name == "default") {
      # Plasma-inspired palette (colorblind-friendly)
      habitat_colors <- c(
        "Pelagic" = "#0d0887",        # Deep blue
        "Demersal" = "#5402a3",       # Purple
        "Reef" = "#8b0aa5",           # Magenta
        "Benthic" = "#b83289",        # Pink
        "Planktonic" = "#db5c68",     # Salmon
        "Primary Producer" = "#f48849",# Orange
        "Detritus" = "#febd2a",       # Yellow
        "Air-breathing" = "#f0f921",  # Bright yellow
        "Other" = "#777777"           # Gray
      )
    } else if (RCOLORBREWER_AVAILABLE && palette_name == "brewer") {
      # RColorBrewer palette - blues for water habitats
      pal <- RColorBrewer::brewer.pal(min(9, num_groups), "Blues")
      habitat_colors <- setNames(pal[1:num_groups], present_groups)
    } else if (VIRIDIS_AVAILABLE && palette_name == "viridis") {
      # Viridis palette
      pal <- viridis::viridis(num_groups)
      habitat_colors <- setNames(pal, present_groups)
    } else if (VIRIDIS_AVAILABLE && palette_name == "cividis") {
      # Cividis palette
      pal <- viridis::cividis(num_groups)
      habitat_colors <- setNames(pal, present_groups)
    } else if (WESANDERSON_AVAILABLE && palette_name == "wes") {
      # Wes Anderson palette
      pal <- wesanderson::wes_palette("Zissou1", num_groups, type = "continuous")
      habitat_colors <- setNames(pal, present_groups)
    } else if (GGSCI_AVAILABLE && palette_name == "npg") {
      # Nature Publishing Group palette
      pal <- ggsci::pal_npg("nrc")(min(10, num_groups))
      habitat_colors <- setNames(pal, present_groups)
    } else {
      # Default to a blue-green palette
      pal <- hcl.colors(num_groups, "Teal")
      habitat_colors <- setNames(pal, present_groups)
    }
    
    # Assign colors based on habitat groups
    colors <- habitat_colors[habitat_groups]
    
    # Create color map for legend (only include groups that are present)
    color_map <- habitat_colors
    
  } else {
    # Default to a single color if no valid scheme is specified
    colors <- rep("white", length(groups))
    color_map <- c("All Groups" = "white")
  }
  
  return(list(colors = colors, color_map = color_map))
}

# Function to create a food web diagram
create_foodweb_diagram <- function(key_file, diet_file, tl_file, xpos_file,
                                          output_file = NULL, box_scale_factor = 0.18,
                                          min_box_size = 0.04, max_box_size = 0.4,
                                          arrow_scale = 1.0, color_scheme = "trophic",
                                          palette_name = "default", text_contrast = TRUE,
                                          font_size = 3.5, title = NULL) {
  # Read the data files using the existing functions
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
  type_values <- eco_data$type[valid_idx]
  DD <- diet_matrix[valid_idx, valid_idx]
  QQ <- QQ[valid_idx, valid_idx]
  
  # Shorten group names for better readability
  shortened_groups <- character(length(groups))
  for (i in 1:length(groups)) {
    name <- groups[i]
    # Remove common words and shorten
    name <- gsub("_", " ", name)
    name <- gsub("fish", "", name, ignore.case = TRUE)
    name <- gsub("benthos", "ben", name, ignore.case = TRUE)
    name <- gsub("plankton", "plk", name, ignore.case = TRUE)
    # Trim whitespace and capitalize first letter
    name <- trimws(name)
    if (nchar(name) > 0) {
      name <- paste0(toupper(substr(name, 1, 1)),
                    if (nchar(name) > 1) substr(name, 2, nchar(name)) else "")
    }
    shortened_groups[i] <- name
  }
  
  # Ensure vertical positions exactly match trophic levels
  # The y-coordinate will be exactly the trophic level value
  
  # Rescale and adjust x-positions to spread nodes more evenly
  # Original range is 0-1, new range is 0-4 with better spacing
  xpos <- xpos * 4.0  # Increased from 3.0 to 4.0 for more horizontal space
  
  # Add small random offsets to prevent overlaps
  set.seed(42)  # For reproducibility
  xpos <- xpos + runif(length(xpos), -0.1, 0.1)
  
  # Ensure x-positions stay within bounds
  xpos <- pmin(pmax(xpos, 0.1), 3.9)
  
  n_groups <- length(groups)
  
  # Calculate box sizes based on biomass - more closely matching IDL implementation
  # Use a stronger scaling factor to make biomass differences more apparent
  # Increase the overall size for better text readability
  box_sizes <- box_scale_factor * 1.5 * (B^0.6)  # Increased overall size by 50%
  box_sizes[box_sizes < min_box_size * 1.5] <- min_box_size * 1.5  # Increased minimum size
  box_sizes[box_sizes > max_box_size * 1.5] <- max_box_size * 1.5  # Increased maximum size
  
  # Get node colors based on the selected scheme
  if (color_scheme != "none") {
    node_colors <- get_node_colors(groups, TL, type_values, color_scheme, palette_name)
    colors <- node_colors$colors
    color_map <- node_colors$color_map
  } else {
    colors <- rep("white", n_groups)
    color_map <- c("All Groups" = "white")
  }
  
  # Create a data frame for the nodes
  # Use circles instead of rectangles for clearer biomass representation
  use_circles <- TRUE  # Set to TRUE to use circles, FALSE to use rectangles
  
  # Create a factor for the color groups
  if (color_scheme == "trophic") {
    # Create color bins based on trophic levels
    tl_bins <- c(1.0, 2.0, 3.0, 4.0, 5.0)
    tl_labels <- c("Producers (TL 1)", "Primary Consumers (TL 2)",
                  "Secondary Consumers (TL 3)", "Tertiary Consumers (TL 4+)",
                  "Top Predators (TL 5+)")
    
    # Assign color groups based on trophic level
    color_groups <- character(length(TL))
    for (i in 1:length(TL)) {
      level <- TL[i]
      if (level < tl_bins[2]) {
        color_groups[i] <- tl_labels[1]
      } else if (level < tl_bins[3]) {
        color_groups[i] <- tl_labels[2]
      } else if (level < tl_bins[4]) {
        color_groups[i] <- tl_labels[3]
      } else if (level < tl_bins[5]) {
        color_groups[i] <- tl_labels[4]
      } else {
        color_groups[i] <- tl_labels[5]
      }
    }
  } else if (color_scheme == "functional") {
    # Define functional groups based on type values and group names
    color_groups <- character(length(groups))
    
    for (i in 1:length(groups)) {
      group <- tolower(groups[i])
      type_val <- type_values[i]
      
      if (type_val == 1.0) {
        color_groups[i] <- "Producer"
      } else if (type_val == 2.0) {
        color_groups[i] <- "Detritus"
      } else if (grepl("fish|shark", group)) {
        color_groups[i] <- "Fish"
      } else if (grepl("bird|cetacean", group)) {
        color_groups[i] <- "Top Predator"
      } else if (grepl("zoo", group)) {
        color_groups[i] <- "Zooplankton"
      } else if (grepl("bacteria", group)) {
        color_groups[i] <- "Bacteria"
      } else if (grepl("bivalve|gastropod", group)) {
        color_groups[i] <- "Mollusc"
      } else if (grepl("crab|crayfish", group)) {
        color_groups[i] <- "Crustacean"
      } else {
        color_groups[i] <- "Other Invertebrate"
      }
    }
  } else if (color_scheme == "habitat") {
    # Define habitat/environment groups based on group names and type values
    color_groups <- character(length(groups))
    
    for (i in 1:length(groups)) {
      group <- tolower(groups[i])
      type_val <- type_values[i]
      
      if (grepl("pelagic|tuna|mackerel", group)) {
        color_groups[i] <- "Pelagic"
      } else if (grepl("demersal|snapper|gurnard|flatfish", group)) {
        color_groups[i] <- "Demersal"
      } else if (grepl("reef", group)) {
        color_groups[i] <- "Reef"
      } else if (grepl("benthic|benthos|crab|star", group)) {
        color_groups[i] <- "Benthic"
      } else if (grepl("zoo|phyto|plankton", group)) {
        color_groups[i] <- "Planktonic"
      } else if (type_val == 1.0) {
        color_groups[i] <- "Primary Producer"
      } else if (type_val == 2.0) {
        color_groups[i] <- "Detritus"
      } else if (grepl("bird|cetacean", group)) {
        color_groups[i] <- "Air-breathing"
      } else {
        color_groups[i] <- "Other"
      }
    }
  } else {
    # Default to a single group if no valid scheme is specified
    color_groups <- rep("All Groups", length(groups))
  }
  
  # Create the nodes data frame with color groups
  nodes <- data.frame(
    id = 1:n_groups,
    label = groups,
    x = xpos,
    y = TL,
    size = box_sizes,
    width = ifelse(use_circles, box_sizes/0.75, box_sizes * 1.2),
    height = box_sizes,
    is_circle = use_circles,
    radius = box_sizes/1.5,  # Radius for circles
    color_group = factor(color_groups)  # Use factor for color grouping
  )
  
  # Create edges dataframe using thresholds similar to IDL implementation
  # In IDL, DQlims=[2.,20.,80.]/100. is used
  DQlims <- c(0.02, 0.2, 0.8)  # Thresholds from IDL
  
  # Find significant links based on both DD and QQ matrices, as in IDL
  links <- data.frame()
  for (i in 1:nrow(DD)) {
    for (j in 1:ncol(DD)) {
      if ((DD[i, j] > DQlims[1] || QQ[i, j] > DQlims[1]) && i != j) {  # Skip self-loops
        links <- rbind(links, data.frame(
          from = i, 
          to = j, 
          dd_weight = DD[i, j],
          qq_weight = QQ[i, j],
          weight = max(DD[i, j], QQ[i, j])
        ))
      }
    }
  }
  
  # Improved overlap prevention algorithm with adaptive step size
  max_iterations <- 10  # Increased from 5 for better convergence
  convergence_threshold <- 0.001  # Stop when movements are small
  min_distance <- 0.3  # Minimum distance between circle centers
  
  for (iteration in 1:max_iterations) {
    max_movement <- 0
    
    # Create a spatial grid for faster neighbor finding
    grid_size <- 0.3
    grid <- list()
    
    for (i in 1:n_groups) {
      grid_x <- as.integer(nodes$x[i] / grid_size)
      grid_y <- as.integer(nodes$y[i] / grid_size)
      key <- paste(grid_x, grid_y, sep = "_")
      
      if (is.null(grid[[key]])) {
        grid[[key]] <- c()
      }
      grid[[key]] <- c(grid[[key]], i)
    }
    
    for (i in 1:n_groups) {
      grid_x <- as.integer(nodes$x[i] / grid_size)
      grid_y <- as.integer(nodes$y[i] / grid_size)
      
      # Check only nearby grid cells
      neighbors <- c()
      for (dx in c(-1, 0, 1)) {
        for (dy in c(-1, 0, 1)) {
          key <- paste(grid_x + dx, grid_y + dy, sep = "_")
          if (!is.null(grid[[key]])) {
            neighbors <- c(neighbors, grid[[key]])
          }
        }
      }
      
      for (j in neighbors) {
        if (i == j) {
          next
        }
        
        # Calculate distance between circles
        dx <- nodes$x[j] - nodes$x[i]
        dy <- nodes$y[j] - nodes$y[i]
        distance <- sqrt(dx^2 + dy^2)
        
        # If circles are too close, push them apart
        if (distance < min_distance) {
          # Calculate push direction
          push_x <- dx / (distance + 1e-10)  # Avoid division by zero
          
          # Push amount (half the overlap) with adaptive step size
          push_amount <- (min_distance - distance) / 2
          push_amount <- push_amount * (1.0 - iteration / max_iterations)  # Reduce step size over iterations
          
          # Only push horizontally to maintain trophic levels
          old_x_i <- nodes$x[i]
          old_x_j <- nodes$x[j]
          
          nodes$x[i] <- nodes$x[i] - push_x * push_amount
          nodes$x[j] <- nodes$x[j] + push_x * push_amount
          
          # Track maximum movement
          movement <- max(abs(nodes$x[i] - old_x_i), abs(nodes$x[j] - old_x_j))
          max_movement <- max(max_movement, movement)
        }
      }
    }
    
    # Check for convergence
    if (max_movement < convergence_threshold) {
      break
    }
  }
  
  # Ensure x-positions stay within bounds after adjustment
  nodes$x <- pmin(pmax(nodes$x, 0.1), 3.9)
  
  # Sort links by weight (descending)
  if (nrow(links) > 0) {
    links <- links[order(-links$weight), ]
    
    # Add coordinates for edge path calculation
    links$x_from <- nodes$x[links$from]
    links$y_from <- nodes$y[links$from]
    links$x_to <- nodes$x[links$to]
    links$y_to <- nodes$y[links$to]
    
    # Add box dimensions
    links$width_from <- nodes$width[links$from]
    links$height_from <- nodes$height[links$from]
    links$width_to <- nodes$width[links$to]
    links$height_to <- nodes$height[links$to]
    
    # Calculate edge start and end points on node edges
    for (i in 1:nrow(links)) {
      dx <- links$x_to[i] - links$x_from[i]
      dy <- links$y_to[i] - links$y_from[i]
      
      from_id <- links$from[i]
      to_id <- links$to[i]
      
      # For circles, calculate intersection points
      if (nodes$is_circle[from_id]) {
        # Calculate angle for start node
        angle <- atan2(dy, dx)
        links$x_start[i] <- nodes$x[from_id] + cos(angle) * nodes$radius[from_id]
        links$y_start[i] <- nodes$y[from_id] + sin(angle) * nodes$radius[from_id]
      } else {
        # For rectangles
        if (abs(dx) > abs(dy)) {
          # Horizontal movement dominates
          if (dx > 0) {
            # Moving right
            links$x_start[i] <- nodes$x[from_id] + nodes$width[from_id]/2
            links$y_start[i] <- nodes$y[from_id]
          } else {
            # Moving left
            links$x_start[i] <- nodes$x[from_id] - nodes$width[from_id]/2
            links$y_start[i] <- nodes$y[from_id]
          }
        } else {
          # Vertical movement dominates
          if (dy > 0) {
            # Moving up
            links$x_start[i] <- nodes$x[from_id]
            links$y_start[i] <- nodes$y[from_id] + nodes$height[from_id]/2
          } else {
            # Moving down
            links$x_start[i] <- nodes$x[from_id]
            links$y_start[i] <- nodes$y[from_id] - nodes$height[from_id]/2
          }
        }
      }
      
      # For end node
      if (nodes$is_circle[to_id]) {
        # Calculate angle for end node (reverse direction)
        angle <- atan2(-dy, -dx)
        links$x_end[i] <- nodes$x[to_id] + cos(angle) * nodes$radius[to_id]
        links$y_end[i] <- nodes$y[to_id] + sin(angle) * nodes$radius[to_id]
      } else {
        # For rectangles
        if (abs(dx) > abs(dy)) {
          # Horizontal movement dominates
          if (dx > 0) {
            # Moving right
            links$x_end[i] <- nodes$x[to_id] - nodes$width[to_id]/2
            links$y_end[i] <- nodes$y[to_id]
          } else {
            # Moving left
            links$x_end[i] <- nodes$x[to_id] + nodes$width[to_id]/2
            links$y_end[i] <- nodes$y[to_id]
          }
        } else {
          # Vertical movement dominates
          if (dy > 0) {
            # Moving up
            links$x_end[i] <- nodes$x[to_id]
            links$y_end[i] <- nodes$y[to_id] - nodes$height[to_id]/2
          } else {
            # Moving down
            links$x_end[i] <- nodes$x[to_id]
            links$y_end[i] <- nodes$y[to_id] + nodes$height[to_id]/2
          }
        }
      }
    }
    
    # Calculate control points for curved edges
    links$mid_x <- (links$x_start + links$x_end) / 2
    links$mid_y <- (links$y_start + links$y_end) / 2
    
    # Calculate perpendicular vectors for control points
    links$perp_dx <- -(links$y_end - links$y_start)
    links$perp_dy <- links$x_end - links$x_start
    
    # Normalize perpendicular vectors
    links$perp_len <- sqrt(links$perp_dx^2 + links$perp_dy^2)
    links$perp_dx <- links$perp_dx / links$perp_len
    links$perp_dy <- links$perp_dy / links$perp_len
    
    # Enhanced curvature algorithm based on IDL implementation
    # Base curve factor
    links$curve_factor <- 0.2
    
    # Alternate curvature direction based on index
    links$curve_factor[links$from %% 3 == 0] <- 0.1
    links$curve_factor[links$from %% 3 == 1] <- 0.3
    links$curve_factor[links$from %% 3 == 2] <- -0.2
    
    # Additional variation based on node positions
    links$curve_factor <- links$curve_factor * (1 + 0.5 * abs(links$x_end - links$x_start) /
                                             (abs(links$x_end - links$x_start) +
                                                abs(links$y_end - links$y_start) + 1e-10))
    
    # Adjust curvature for long arrows to avoid overlaps
    links$distance <- sqrt((links$x_end - links$x_start)^2 + (links$y_end - links$y_start)^2)
    links$curve_factor[links$distance > 0.5] <- links$curve_factor[links$distance > 0.5] * 1.5
    
    # Calculate control points
    links$control_x <- links$mid_x + links$curve_factor * links$perp_dx
    links$control_y <- links$mid_y + links$curve_factor * links$perp_dy
    
    # Calculate line width based on weight thresholds from IDL - enhanced for more distinction
    links$line_width <- 0.5  # Default thin line
    links$line_width[links$weight >= DQlims[1] & links$weight < DQlims[2]] <- 0.5  # Thin line for weak connections
    links$line_width[links$weight >= DQlims[2] & links$weight < DQlims[3]] <- 2.0  # Medium line (increased from 1.5)
    links$line_width[links$weight >= DQlims[3]] <- 4.0  # Thick line (increased from 3.0)
    
    # Add alpha transparency based on weight for better visual distinction
    links$alpha <- 0.7  # Default alpha
    links$alpha[links$weight < DQlims[1]] <- 0.5  # More transparent for weak connections
  }
  
  # Create the plot with wider dimensions
  p <- ggplot() +
    # Set up the plot area with even wider x-axis limits
    xlim(-0.2, 4.2) +  # Increased x-axis range to match the 4.0 scaling
    scale_y_continuous(
      limits = c(0.8, 5.2),
      breaks = c(1, 2, 3, 4, 5),
      labels = c("1", "2", "3", "4", "5")
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(size = 14),
      axis.text.y = element_text(size = 12),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 20)),
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.background = element_rect(fill = "white", color = "black"),
      legend.margin = margin(10, 10, 10, 10),
      legend.key.size = unit(1.2, "cm"),  # Larger legend keys
      legend.text = element_text(size = 12),  # Larger legend text
      legend.title = element_text(size = 14)  # Larger legend titles
    ) +
    labs(y = "Trophic level")
  
  # Add title if provided
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  
  # First add all edges (arrows) so they appear behind nodes
  if (nrow(links) > 0) {
    for (i in 1:nrow(links)) {
      # Skip self-loops for simplicity
      if (links$from[i] == links$to[i]) {
        next
      }
      
      # Create Bezier curve points
      t <- seq(0, 1, length.out = 100)
      x <- (1-t)^2 * links$x_start[i] +
           2*(1-t)*t * links$control_x[i] +
           t^2 * links$x_end[i]
      y <- (1-t)^2 * links$y_start[i] +
           2*(1-t)*t * links$control_y[i] +
           t^2 * links$y_end[i]
      
      # Create path data frame
      path_df <- data.frame(
        x = x,
        y = y,
        group = i
      )
      
      # Add path with enhanced styling based on weight
      p <- p + geom_path(
        data = path_df,
        aes(x = x, y = y, group = group),
        color = "black",
        size = links$line_width[i] * 0.3,  # Scale down for ggplot
        alpha = links$alpha[i]  # Use weight-based alpha
      )
      
      # Add arrow head
      # Calculate the distance from the end point to the target circle
      # We want to place the arrowhead just before it reaches the circle
      to_id <- links$to[i]
      end_radius <- nodes$radius[to_id]
      
      # Find a point along the curve that's outside the target circle
      # Start from the end and move backwards
      arrow_idx <- nrow(path_df) - 1
      for (j in (nrow(path_df)-2):2) {
        # Calculate distance from this point to the circle center
        dist_to_center <- sqrt((path_df$x[j] - nodes$x[to_id])^2 +
                              (path_df$y[j] - nodes$y[to_id])^2)
        
        # If we're outside the circle (with a small buffer), use this point
        if (dist_to_center > end_radius + 0.06) {  # 0.06 is 2x arrow_size
          arrow_idx <- j
          break
        }
      }
      
      arrow_x <- path_df$x[arrow_idx]
      arrow_y <- path_df$y[arrow_idx]
      arrow_dx <- path_df$x[arrow_idx+1] - path_df$x[arrow_idx]
      arrow_dy <- path_df$y[arrow_idx+1] - path_df$y[arrow_idx]
      arrow_len <- sqrt(arrow_dx^2 + arrow_dy^2)
      
      if (arrow_len > 0) {
        arrow_dx <- arrow_dx / arrow_len
        arrow_dy <- arrow_dy / arrow_len
        
        # Use consistent arrow head size regardless of line width
        arrow_size <- 0.03  # Fixed size for all arrowheads
        
        # Arrow head points
        arrow_head_df <- data.frame(
          x = c(
            arrow_x,
            arrow_x + arrow_size * (arrow_dx - 0.5 * arrow_dy),
            arrow_x + arrow_size * (arrow_dx + 0.5 * arrow_dy)
          ),
          y = c(
            arrow_y,
            arrow_y + arrow_size * (arrow_dy + 0.5 * arrow_dx),
            arrow_y + arrow_size * (arrow_dy - 0.5 * arrow_dx)
          ),
          group = paste0("arrow_", i)
        )
        
        # Add arrow head
        p <- p + geom_polygon(
          data = arrow_head_df,
          aes(x = x, y = y, group = group),
          fill = "black",
          color = "black",
          alpha = 0.9  # Increased alpha for better visibility
        )
      }
    }
  }
  
  # Now add nodes (on top of arrows)
  if (nodes$is_circle[1]) {  # Check if using circles
    # Add all circles at once with proper aesthetics for legend
    circle_data <- data.frame(
      x0 = nodes$x,
      y0 = nodes$y,
      r = nodes$radius,
      color_group = nodes$color_group
    )
    
    # Draw circles with proper fill
    p <- p + geom_circle(
      data = circle_data,
      aes(x0 = x0, y0 = y0, r = r, fill = color_group),
      color = "black", size = 0.5, alpha = 0.9
    )
    
    # Add a second layer of circles for the problematic large nodes
    large_circles <- circle_data[circle_data$r > 0.2, ]
    if (nrow(large_circles) > 0) {
      p <- p + geom_circle(
        data = large_circles,
        aes(x0 = x0, y0 = y0, r = r, fill = color_group),
        color = "black", size = 0.5, alpha = 0.9
      )
    }
  } else {
    # Add all rectangles at once with proper aesthetics for legend
    rect_data <- data.frame(
      xmin = nodes$x - nodes$width/2,
      xmax = nodes$x + nodes$width/2,
      ymin = nodes$y - nodes$height/2,
      ymax = nodes$y + nodes$height/2,
      color_group = nodes$color_group
    )
    
    p <- p + geom_rect(
      data = rect_data,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = color_group),
      color = "black", size = 0.5, alpha = 0.9
    )
  }
    
  # Function to wrap text to fit in circle
  wrap_text <- function(text, width = 10) {
    # Split text into words
    words <- unlist(strsplit(text, " "))
    lines <- character(0)
    current_line <- character(0)
    
    for (word in words) {
      if (nchar(paste(c(current_line, word), collapse = " ")) <= width) {
        current_line <- c(current_line, word)
      } else {
        if (length(current_line) > 0) {
          lines <- c(lines, paste(current_line, collapse = " "))
          current_line <- c(word)
        } else {
          # If a single word is longer than width
          lines <- c(lines, word)
          current_line <- character(0)
        }
      }
    }
    
    # Don't forget the last line
    if (length(current_line) > 0) {
      lines <- c(lines, paste(current_line, collapse = " "))
    }
    
    return(paste(lines, collapse = "\n"))
  }
  
  # Add labels for all nodes
  for (i in 1:nrow(nodes)) {
    # Add label with text wrapping
    label <- shortened_groups[i]
    
    # Handle hyphenated names and wrap text
    if (grepl("-", label)) {
      parts <- strsplit(label, "-")[[1]]
      if (length(parts) == 2) {
        # Create wrapped text for each part
        wrapped_text <- paste(parts[1], parts[2], sep = "\n")
      } else {
        # Wrap the whole text
        wrapped_text <- wrap_text(label)
      }
    } else {
      # Wrap single line label
      wrapped_text <- wrap_text(label)
    }
    
    # Add text with smaller font size for longer labels
    adjusted_font_size <- ifelse(nchar(wrapped_text) < 15, font_size, font_size - 0.5)
    
    # Add text with contrast enhancement if requested
    if (text_contrast) {
      # Add text with white outline for better readability
      # In ggplot2, we can simulate this by adding multiple text layers
      # First add white text with larger size for the "outline"
      p <- p + geom_text(
        data = data.frame(x = nodes$x[i], y = nodes$y[i], label = wrapped_text),
        aes(x = x, y = y, label = label),
        size = adjusted_font_size + 0.5,
        hjust = 0.5,
        vjust = 0.5,
        color = "white",
        fontface = "bold"
      )
      
      # Then add the actual text on top
      p <- p + geom_text(
        data = data.frame(x = nodes$x[i], y = nodes$y[i], label = wrapped_text),
        aes(x = x, y = y, label = label),
        size = adjusted_font_size,
        hjust = 0.5,
        vjust = 0.5,
        color = "black",
        fontface = "bold"
      )
    } else {
      # Standard text without outline
      p <- p + geom_text(
        data = data.frame(x = nodes$x[i], y = nodes$y[i], label = wrapped_text),
        aes(x = x, y = y, label = label),
        size = adjusted_font_size,
        hjust = 0.5,
        vjust = 0.5
      )
    }
  }
  
  # No need to add y-axis ticks and labels here as they are already defined above
  
  # Create legend elements
  legend_elements <- list()
  
  # Add color legend if using a color scheme
  if (color_scheme != "none") {
    for (name in names(color_map)) {
      legend_elements[[length(legend_elements) + 1]] <- list(
        name = name,
        color = color_map[name],
        type = "color"
      )
    }
  }
  
  # Add size legend
  size_values <- c(min_box_size, (min_box_size + max_box_size)/2, max_box_size)
  size_labels <- c("Small Biomass", "Medium Biomass", "Large Biomass")
  
  for (i in 1:length(size_values)) {
    legend_elements[[length(legend_elements) + 1]] <- list(
      name = size_labels[i],
      size = size_values[i],
      type = "size"
    )
  }
  
  # Add line width legend
  width_values <- c(0.5, 2.0, 4.0)
  width_labels <- c("Weak Interaction", "Medium Interaction", "Strong Interaction")
  
  for (i in 1:length(width_values)) {
    legend_elements[[length(legend_elements) + 1]] <- list(
      name = width_labels[i],
      width = width_values[i],
      type = "width"
    )
  }
  
  # Create custom legend grobs
  legend_grobs <- list()
  
  # Create color legend
  color_legend <- list()
  width_legend <- list()
  size_legend <- list()
  
  for (i in 1:length(legend_elements)) {
    element <- legend_elements[[i]]
    if (element$type == "color") {
      color_legend <- c(color_legend, element)
    } else if (element$type == "width") {
      width_legend <- c(width_legend, element)
    } else if (element$type == "size") {
      size_legend <- c(size_legend, element)
    }
  }
  
  # Create a legend title based on the color scheme
  if (color_scheme == "trophic") {
    legend_title <- "Trophic Levels"
  } else if (color_scheme == "functional") {
    legend_title <- "Functional Groups"
  } else if (color_scheme == "habitat") {
    legend_title <- "Habitat Groups"
  } else {
    legend_title <- "Groups"
  }
  
  # Add fill scale with the appropriate colors
  p <- p + scale_fill_manual(
    values = color_map,
    name = legend_title
  )
  
  # Set legend position to top
  p <- p + theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.key.size = unit(0.8, "cm"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  )
  
  # Save the plot if output file is specified with increased width
  if (!is.null(output_file)) {
    ggsave(output_file, p, width = 20, height = 10, dpi = 300)  # Increased height for legend
    cat("Enhanced food web diagram saved to:", output_file, "\n")
  }
  
  return(p)
}

# Run the food web diagram creation
run_foodweb_diagram <- function() {
  # Create output directory if it doesn't exist
  if (!dir.exists("images")) {
    dir.create("images")
  }
  
  # Create the version with trophic level coloring
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
    palette_name = "default",
    text_contrast = TRUE,
    font_size = 3.5,
    title = "Hauraki Gulf Food Web - Colored by Trophic Level"
  )
  
  # Create the version with functional group coloring
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
    palette_name = "default",
    text_contrast = TRUE,
    font_size = 3.5,
    title = "Hauraki Gulf Food Web - Colored by Functional Group"
  )
  
  # Create the version with habitat-based coloring
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
    palette_name = "default",
    text_contrast = TRUE,
    font_size = 3.5,
    title = "Hauraki Gulf Food Web - Colored by Habitat"
  )
  
  cat("R implementation completed successfully!\n")
}

# Run the enhanced food web diagram creation if this script is run directly
if (!interactive()) {
  run_foodweb_diagram()
}