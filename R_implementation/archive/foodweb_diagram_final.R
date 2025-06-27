# Enhanced Food Web Diagram in R
# This script improves upon the existing R implementation to match the original IDL implementation

# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(ggforce)  # For geom_circle

# Source the base functions from the existing implementation
source("R_implementation/foodweb_diagram.R")

# Function to create a more accurate food web diagram
create_accurate_foodweb_diagram <- function(key_file, diet_file, tl_file, xpos_file, 
                                          output_file = NULL) {
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
  DD <- diet_matrix[valid_idx, valid_idx]
  QQ <- QQ[valid_idx, valid_idx]
  
  # Rescale and adjust x-positions to spread nodes more evenly
  # Original range is 0-1, new range is 0-3 with better spacing
  xpos <- xpos * 3.0  # Increased from 2.0 to 3.0 for more horizontal space
  
  # Add small random offsets to prevent overlaps
  set.seed(42)  # For reproducibility
  xpos <- xpos + runif(length(xpos), -0.1, 0.1)
  
  # Ensure x-positions stay within bounds
  xpos <- pmin(pmax(xpos, 0.1), 2.9)
  
  n_groups <- length(groups)
  
  # Calculate box sizes based on biomass - more closely matching IDL implementation
  box_scale_factor <- 0.18
  min_box_size <- 0.04
  max_box_size <- 0.4  # Increased maximum box size
  box_sizes <- box_scale_factor * (B^0.6)  # Increased exponent for more pronounced scaling
  box_sizes[box_sizes < min_box_size] <- min_box_size
  box_sizes[box_sizes > max_box_size] <- max_box_size
  
  # Create a data frame for the nodes
  # Use circles instead of rectangles for clearer biomass representation
  use_circles <- TRUE  # Set to TRUE to use circles, FALSE to use rectangles
  
  nodes <- data.frame(
    id = 1:n_groups,
    label = groups,
    x = xpos,
    y = TL,
    size = box_sizes,
    width = ifelse(use_circles, box_sizes/0.75, box_sizes * 1.2),  # Reduced from 1.8 to make boxes more square
    height = box_sizes,
    is_circle = use_circles,
    radius = box_sizes/1.5  # Radius for circles
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
    xlim(-0.2, 3.2) +  # Increased x-axis range to match the 3.0 scaling
    ylim(0.8, 5.2) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_text(size = 14),
      axis.text.y = element_text(size = 12),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    labs(y = "Trophic level")
  
  # Further adjust positions to prevent circle overlaps
  min_distance <- 0.3  # Minimum distance between circle centers
  
  # Simple iterative adjustment to reduce overlaps
  for (iter in 1:5) {  # Repeat a few times for better results
    for (i in 1:n_groups) {
      for (j in (i+1):n_groups) {
        if (j <= n_groups) {  # Make sure j is valid
          # Calculate distance between circles
          dx <- nodes$x[j] - nodes$x[i]
          dy <- nodes$y[j] - nodes$y[i]
          distance <- sqrt(dx^2 + dy^2)
          
          # If circles are too close, push them apart
          if (distance < min_distance) {
            # Calculate push direction
            push_x <- dx / (distance + 1e-10)  # Avoid division by zero
            
            # Push amount (half the overlap)
            push_amount <- (min_distance - distance) / 2
            
            # Only push horizontally to maintain trophic levels
            nodes$x[i] <- nodes$x[i] - push_x * push_amount
            nodes$x[j] <- nodes$x[j] + push_x * push_amount
          }
        }
      }
    }
  }
  
  # Ensure x-positions stay within bounds after adjustment
  nodes$x <- pmin(pmax(nodes$x, 0.1), 2.9)
  
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
      arrow_idx <- nrow(path_df) - 1
      arrow_x <- path_df$x[arrow_idx]
      arrow_y <- path_df$y[arrow_idx]
      arrow_dx <- path_df$x[arrow_idx] - path_df$x[arrow_idx-1]
      arrow_dy <- path_df$y[arrow_idx] - path_df$y[arrow_idx-1]
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
  for (i in 1:nrow(nodes)) {
    if (nodes$is_circle[i]) {
      # Add circle
      p <- p + geom_circle(
        data = data.frame(
          x0 = nodes$x[i],
          y0 = nodes$y[i],
          r = nodes$radius[i]
        ),
        aes(x0 = x0, y0 = y0, r = r),
        fill = "white", color = "black", size = 0.5
      )
    } else {
      # Add rectangle
      p <- p + geom_rect(
        data = data.frame(
          xmin = nodes$x[i] - nodes$width[i]/2,
          xmax = nodes$x[i] + nodes$width[i]/2,
          ymin = nodes$y[i] - nodes$height[i]/2,
          ymax = nodes$y[i] + nodes$height[i]/2
        ),
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = "white", color = "black", size = 0.5
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
    
    # Add label with text wrapping
    label <- nodes$label[i]
    
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
    font_size <- ifelse(nchar(wrapped_text) < 15, 3.5, 3.0)
    
    p <- p + geom_text(
      data = data.frame(x = nodes$x[i], y = nodes$y[i], label = wrapped_text),
      aes(x = x, y = y, label = label),
      size = font_size, hjust = 0.5, vjust = 0.5
    )
  }
  
    # Now add text labels (on top of everything)
  
  # Add y-axis ticks and labels
  p <- p + scale_y_continuous(
    breaks = c(1, 2, 3, 4, 5),
    labels = c("1", "2", "3", "4", "5")
  )
  
  # Save the plot if output file is specified with increased width
  if (!is.null(output_file)) {
    ggsave(output_file, p, width = 20, height = 8, dpi = 300)  # Increased width from 13 to 20
    cat("Food web diagram saved to:", output_file, "\n")
  }
  
  return(p)
}

# Run the accurate food web diagram creation
result <- create_accurate_foodweb_diagram(
  key_file = "data/HG04-key-adj.out",
  diet_file = "data/HG04-diets-adj.out",
  tl_file = "data/HG04-key-adj-out-trophic_levels.out",
  xpos_file = "data/HG04-xpos0.txt",
  output_file = "images/R-layered-diagram.png"  # New filename to reflect the changes
)

cat("R implementation completed successfully!\n")