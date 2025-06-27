#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Enhanced Food Web Diagram in Python
This script creates a food web diagram similar to the original IDL implementation
"""

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.path import Path
import matplotlib.patheffects as path_effects

# Function to read the ecosystem file
def read_ecosystem_file(file_path):
    """Read the ecosystem file and return the data as a dictionary"""
    try:
        data = pd.read_csv(file_path, sep='\t')
        
        # Extract the group names and parameters
        groups = data['Group'].values
        type_values = data['type'].values
        B = data['B'].values
        PB = data['P/B'].values
        QB = data['Q/B'].values
        
        return {
            'groups': groups,
            'type': type_values,
            'B': B,
            'PB': PB,
            'QB': QB
        }
    except Exception as e:
        print(f"Error reading ecosystem file: {e}")
        return None

# Function to read the diet composition file
def read_diet_file(file_path):
    """Read the diet composition file and return the data as a matrix"""
    try:
        data = pd.read_csv(file_path, sep='\t')
        
        # Extract the group names and diet matrix
        groups = data['Group'].values
        diet_matrix = data.iloc[:, 1:].values
        
        return diet_matrix
    except Exception as e:
        print(f"Error reading diet file: {e}")
        return None

# Function to read the trophic levels file
def read_trophic_levels(file_path):
    """Read the trophic levels file and return the data as a dictionary"""
    try:
        data = pd.read_csv(file_path, sep='\t')
        
        # Extract the group names and trophic levels
        groups = data['GROUP'].values
        trophic_levels = data['Trophic_Level'].values
        
        return {
            'group': groups,
            'TL': trophic_levels
        }
    except Exception as e:
        print(f"Error reading trophic levels file: {e}")
        return None

# Function to read the x-positions file
def read_xpos(file_path):
    """Read the x-positions file and return the data as a dictionary"""
    try:
        data = pd.read_csv(file_path, sep='\t')
        
        # Extract the group names and x-positions
        groups = data['Gstr0'].values
        xpos = data['xpos'].values
        
        return {
            'group': groups,
            'xpos': xpos
        }
    except Exception as e:
        print(f"Error reading x-positions file: {e}")
        return None

# Function to calculate QQ matrix (equivalent to get_QQ.pro in IDL)
def calculate_QQ(P, Q, DD):
    """Calculate the QQ matrix from P, Q, and DD matrices"""
    Nx = len(P)
    
    # Check dimensions
    if len(Q) != Nx:
        raise ValueError("Error in calculate_QQ: bad Q dimensions")
    if DD.shape[0] != Nx or DD.shape[1] != Nx:
        raise ValueError("Error in calculate_QQ: bad DD dimensions")
    
    # Check diet totals
    row_sums = np.sum(DD, axis=1)
    if np.any((row_sums < 0) | (row_sums > 1.000001)):
        print("Warning: Bad diet totals in calculate_QQ")
    
    # Calculate QQ matrix
    out = np.zeros((Nx, Nx))
    for i in range(Nx):
        out[i, :] = DD[i, :] * Q[i]  # Multiply each row by corresponding Q value
    
    # Divide each column by corresponding P value
    for i in range(Nx):
        if P[i] > 0:
            out[:, i] = out[:, i] / P[i]  # by rows (prey)
        else:
            out[:, i] = 0
    
    # Normalize columns that sum to > 1
    col_sums = np.sum(out, axis=0)
    high_cols = np.where(col_sums > 1)[0]
    if len(high_cols) > 0:
        for i in high_cols:
            out[:, i] = out[:, i] / col_sums[i]
    
    # Set negative values to 0
    out[out < 0] = 0
    
    return out

# Function to create a quadratic Bezier curve
def quadratic_bezier(start, end, control, num_points=100):
    """Create a quadratic Bezier curve between two points with a control point"""
    t = np.linspace(0, 1, num_points)
    x = (1-t)**2 * start[0] + 2*(1-t)*t * control[0] + t**2 * end[0]
    y = (1-t)**2 * start[1] + 2*(1-t)*t * control[1] + t**2 * end[1]
    return x, y

# Function to plot the food web diagram
def plot_foodweb_diagram(groups, xpos, TL, B, DD, QQ, output_file=None,
                        box_scale_factor=0.18, min_box_size=0.04,
                        arrow_scale=1.0):
    """Plot the food web diagram using matplotlib"""
    # Filter out groups with invalid positions (e.g., -9999)
    valid_idx = np.where(xpos > -999)[0]
    groups = groups[valid_idx]
    xpos = xpos[valid_idx]
    TL = TL[valid_idx]
    B = B[valid_idx]
    DD = DD[valid_idx, :][:, valid_idx]
    QQ = QQ[valid_idx, :][:, valid_idx]
    
    # Rescale and adjust x-positions to spread nodes more evenly
    # Original range is 0-1, new range is 0-3 with better spacing
    xpos = xpos * 3.0  # Increased from 2.0 to 3.0 for more horizontal space
    
    # Add small random offsets to prevent overlaps
    np.random.seed(42)  # For reproducibility
    xpos += np.random.uniform(-0.1, 0.1, size=len(xpos))
    
    # Ensure x-positions stay within bounds
    xpos = np.clip(xpos, 0.1, 2.9)
    
    # Further adjust positions to prevent circle overlaps
    n_groups = len(groups)
    min_distance = 0.3  # Minimum distance between circle centers
    
    # Simple iterative adjustment to reduce overlaps
    for _ in range(5):  # Repeat a few times for better results
        for i in range(n_groups):
            for j in range(i+1, n_groups):
                # Calculate distance between circles
                dx = xpos[j] - xpos[i]
                dy = TL[j] - TL[i]
                distance = np.sqrt(dx**2 + dy**2)
                
                # If circles are too close, push them apart
                if distance < min_distance:
                    # Calculate push direction
                    push_x = dx / (distance + 1e-10)  # Avoid division by zero
                    push_y = dy / (distance + 1e-10)
                    
                    # Push amount (half the overlap)
                    push_amount = (min_distance - distance) / 2
                    
                    # Only push horizontally to maintain trophic levels
                    xpos[i] -= push_x * push_amount
                    xpos[j] += push_x * push_amount
    
    # Ensure x-positions stay within bounds after adjustment
    xpos = np.clip(xpos, 0.1, 2.9)
    
    n_groups = len(groups)
    
    # Calculate box sizes based on biomass - more closely matching IDL implementation
    # Use a stronger scaling factor to make biomass differences more apparent
    box_sizes = box_scale_factor * (B**0.6)  # Increased exponent for more pronounced scaling
    box_sizes[box_sizes < min_box_size] = min_box_size
    # Add upper limit for very large boxes
    max_box_size = 0.4
    box_sizes[box_sizes > max_box_size] = max_box_size
    
    # Create a much wider figure with white background
    fig, ax = plt.subplots(figsize=(20, 8), facecolor='white')  # Increased width from 13 to 20
    ax.set_facecolor('white')
    
    # Set up the plot area with even wider x-axis limits
    ax.set_xlim(-0.2, 3.2)  # Increased x-axis range
    ax.set_ylim(0.8, 5.2)
    ax.set_ylabel('Trophic level', fontsize=14)
    ax.set_yticks([1, 2, 3, 4, 5])
    ax.set_yticklabels(['1', '2', '3', '4', '5'])
    ax.set_xticks([])
    
    # Remove top, right, and bottom spines
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.spines['bottom'].set_visible(False)
    
    # Create a dictionary to store node positions and sizes
    node_info = {}
    
    # First draw all arrows (so they appear behind nodes)
    # Find significant links - using thresholds similar to IDL implementation
    # In IDL, DQlims=[2.,20.,80.]/100. is used
    # We'll use the lower threshold (0.02) to determine which links to show
    DQlims = [0.02, 0.2, 0.8]  # Thresholds from IDL
    
    # Create links based on both DD and QQ matrices, as in IDL
    links = []
    for i in range(DD.shape[0]):
        for j in range(DD.shape[1]):
            if (DD[i, j] > DQlims[0] or QQ[i, j] > DQlims[0]) and i != j:  # Skip self-loops
                links.append((i, j, DD[i, j], QQ[i, j]))
    
    # Sort links by weight (descending)
    links.sort(key=lambda x: max(x[2], x[3]), reverse=True)
    
    # Create a dictionary to store node positions and sizes
    node_info = {}
    
    # Store node information first
    for i in range(n_groups):
        # Calculate box dimensions - make boxes more square-shaped
        box_size = box_sizes[i]
        
        # Store node information
        node_info[i] = {
            'x': xpos[i],
            'y': TL[i],
            'width': box_size/0.75,  # Adjust for circle radius
            'height': box_size/0.75,
            'is_circle': True,
            'radius': box_size/1.5
        }
    
    # Draw edges (arrows) first so they appear behind nodes
    for i, j, dd_weight, qq_weight in links:
        # Get node positions
        start_node = node_info[i]
        end_node = node_info[j]
        
        # Calculate start and end points on the box edges
        dx = end_node['x'] - start_node['x']
        dy = end_node['y'] - start_node['y']
        
        # Determine which edge of the node to use based on relative positions
        if start_node.get('is_circle', False):
            # For circles, calculate intersection point with circle
            angle = np.arctan2(dy, dx)
            start_x = start_node['x'] + np.cos(angle) * start_node['radius']
            start_y = start_node['y'] + np.sin(angle) * start_node['radius']
        else:
            # For rectangles
            if abs(dx) > abs(dy):
                # Horizontal movement dominates
                if dx > 0:
                    # Moving right
                    start_x = start_node['x'] + start_node['width']/2
                    start_y = start_node['y']
                else:
                    # Moving left
                    start_x = start_node['x'] - start_node['width']/2
                    start_y = start_node['y']
            else:
                # Vertical movement dominates
                if dy > 0:
                    # Moving up
                    start_x = start_node['x']
                    start_y = start_node['y'] + start_node['height']/2
                else:
                    # Moving down
                    start_x = start_node['x']
                    start_y = start_node['y'] - start_node['height']/2
        
        if end_node.get('is_circle', False):
            # For circles, calculate intersection point with circle
            angle = np.arctan2(-dy, -dx)  # Reverse direction for end node
            end_x = end_node['x'] + np.cos(angle) * end_node['radius']
            end_y = end_node['y'] + np.sin(angle) * end_node['radius']
        else:
            # For rectangles
            if abs(dx) > abs(dy):
                # Horizontal movement dominates
                if dx > 0:
                    # Moving right
                    end_x = end_node['x'] - end_node['width']/2
                    end_y = end_node['y']
                else:
                    # Moving left
                    end_x = end_node['x'] + end_node['width']/2
                    end_y = end_node['y']
            else:
                # Vertical movement dominates
                if dy > 0:
                    # Moving up
                    end_x = end_node['x']
                    end_y = end_node['y'] - end_node['height']/2
                else:
                    # Moving down
                    end_x = end_node['x']
                    end_y = end_node['y'] + end_node['height']/2
        
        # Calculate control point for curved arrow
        # Vary curvature based on positions to avoid overlaps
        mid_x = (start_x + end_x) / 2
        mid_y = (start_y + end_y) / 2
        
        # Calculate perpendicular vector
        perp_dx = -(end_y - start_y)
        perp_dy = end_x - start_x
        perp_len = np.sqrt(perp_dx**2 + perp_dy**2)
        
        # Normalize and scale
        if perp_len > 0:
            perp_dx /= perp_len
            perp_dy /= perp_len
        
        # Enhanced curvature algorithm based on IDL implementation
        # Base curve factor
        if i % 3 == 0:
            curve_factor = 0.1
        elif i % 3 == 1:
            curve_factor = 0.3
        else:
            curve_factor = -0.2
        
        # Additional variation based on node positions
        curve_factor *= (1 + 0.5 * abs(dx) / (abs(dx) + abs(dy) + 1e-10))
        
        # Adjust curvature for long arrows to avoid overlaps
        distance = np.sqrt(dx**2 + dy**2)
        if distance > 0.5:  # For longer arrows
            curve_factor *= 1.5  # Increase curvature
        
        # Control point
        control_x = mid_x + curve_factor * perp_dx
        control_y = mid_y + curve_factor * perp_dy
        
        # Create Bezier curve
        x, y = quadratic_bezier((start_x, start_y), (end_x, end_y),
                               (control_x, control_y), num_points=100)
        
        # Calculate line width based on diet proportion and QQ value
        # In IDL, line thickness is determined by both DD and QQ
        weight = max(dd_weight, qq_weight)
        
        # Scale line width based on thresholds from IDL - enhanced for more distinction
        if weight < DQlims[1]:
            line_width = 0.5  # Thin line for weak connections
        elif weight < DQlims[2]:
            line_width = 2.0  # Medium line for moderate connections (increased from 1.5)
        else:
            line_width = 4.0  # Thick line for strong connections (increased from 3.0)
            
        # Determine line color based on weight for better visual distinction
        line_color = 'black'
        line_alpha = 0.7
        if weight < DQlims[1]:
            line_alpha = 0.5  # More transparent for weak connections
        
        # Draw the path with enhanced styling
        line = ax.plot(x, y, 'k-', linewidth=line_width, alpha=line_alpha)[0]
        
        # Use consistent arrow head size regardless of line width
        arrow_size = 0.03  # Fixed size for all arrowheads
        arrow_idx = -2  # Use second-to-last point for arrow
        dx = x[arrow_idx] - x[arrow_idx-1]
        dy = y[arrow_idx] - y[arrow_idx-1]
        arrow_len = np.sqrt(dx**2 + dy**2)
        
        if arrow_len > 0:
            dx /= arrow_len
            dy /= arrow_len
            
            # Create arrow head with consistent size
            arrow_head = patches.FancyArrow(
                x[arrow_idx], y[arrow_idx],
                dx * arrow_size, dy * arrow_size,
                width=arrow_size/3,
                head_width=arrow_size,
                head_length=arrow_size*1.5,
                shape='full',
                overhang=0,
                head_starts_at_zero=False,
                color='black',
                alpha=0.9  # Increased alpha for better visibility
            )
            ax.add_patch(arrow_head)
    
    # Now draw nodes (on top of arrows)
    for i in range(n_groups):
        # Calculate box dimensions - make boxes more square-shaped
        box_size = box_sizes[i]
        
        # Use circles instead of rectangles for clearer biomass representation
        # Create circle
        circle = patches.Circle(
            (xpos[i], TL[i]),
            radius=box_size/1.5,  # Adjust radius to make area proportional to biomass
            linewidth=1, edgecolor='black', facecolor='white'
        )
        ax.add_patch(circle)
        
        # No need to store node info again
        
        # Format group name for display with text wrapping
        display_name = groups[i]
        
        # Function to wrap text to fit in circle
        def wrap_text(text, width=10):
            """Wrap text to fit within a certain width"""
            words = text.split()
            lines = []
            current_line = []
            
            for word in words:
                if len(' '.join(current_line + [word])) <= width:
                    current_line.append(word)
                else:
                    if current_line:  # Only append if there are words
                        lines.append(' '.join(current_line))
                        current_line = [word]
                    else:  # If a single word is longer than width
                        lines.append(word)
                        current_line = []
            
            if current_line:  # Don't forget the last line
                lines.append(' '.join(current_line))
                
            return '\n'.join(lines)
        
        # Handle hyphenated names and wrap text
        if '-' in display_name:
            parts = display_name.split('-')
            if len(parts) == 2:
                # Create wrapped text for each part
                wrapped_text = parts[0] + '\n' + parts[1]
            else:
                # Wrap the whole text
                wrapped_text = wrap_text(display_name)
        else:
            # Wrap single line label
            wrapped_text = wrap_text(display_name)
        
        # Add text with smaller font size for longer labels
        font_size = 9 if len(wrapped_text) < 15 else 8
        ax.text(xpos[i], TL[i], wrapped_text,
               ha='center', va='center', fontsize=font_size)
    
    # Text labels are already added above
    
    # Save the figure if output file is specified
    if output_file:
        plt.savefig(output_file, dpi=300, bbox_inches='tight', facecolor='white')
        print(f"Food web diagram saved to: {output_file}")
    
    plt.close()
    return fig

# Main function to create the food web diagram
def create_foodweb_diagram(key_file, diet_file, tl_file, xpos_file, output_file=None,
                          box_scale_factor=0.18, min_box_size=0.04,
                          arrow_scale=1.0):
    """Create the food web diagram from the input files"""
    # Check if files exist
    for file in [key_file, diet_file, tl_file, xpos_file]:
        if not os.path.exists(file):
            print(f"Error: File not found: {file}")
            return None
    
    # Read the data files
    eco_data = read_ecosystem_file(key_file)
    diet_matrix = read_diet_file(diet_file)
    tl_data = read_trophic_levels(tl_file)
    xpos_data = read_xpos(xpos_file)
    
    if eco_data is None or diet_matrix is None or tl_data is None or xpos_data is None:
        print("Error: Failed to read one or more input files")
        return None
    
    # Match the group names across files
    groups = eco_data['groups']
    
    # Calculate P and Q vectors
    P = eco_data['B'] * eco_data['PB']  # Production
    Q = eco_data['B'] * eco_data['QB']  # Consumption
    
    # Calculate QQ matrix
    QQ = calculate_QQ(P, Q, diet_matrix)
    
    # Plot the food web diagram
    fig = plot_foodweb_diagram(
        groups=groups,
        xpos=xpos_data['xpos'],
        TL=tl_data['TL'],
        B=eco_data['B'],
        DD=diet_matrix,
        QQ=QQ,
        output_file=output_file,
        box_scale_factor=box_scale_factor,
        min_box_size=min_box_size,
        arrow_scale=arrow_scale
    )
    
    # Return results
    return {
        'groups': groups,
        'xpos': xpos_data['xpos'],
        'TL': tl_data['TL'],
        'B': eco_data['B'],
        'DD': diet_matrix,
        'QQ': QQ,
        'figure': fig
    }

# Run the food web diagram creation
if __name__ == "__main__":
    # Create output directory if it doesn't exist
    if not os.path.exists("images"):
        os.makedirs("images")
    
    # Create the matplotlib version with thresholds matching IDL
    result = create_foodweb_diagram(
        key_file="data/HG04-key-adj.out",
        diet_file="data/HG04-diets-adj.out",
        tl_file="data/HG04-key-adj-out-trophic_levels.out",
        xpos_file="data/HG04-xpos0.txt",
        output_file="images/Python-layered-diagram.png",  # New filename to reflect the changes
        box_scale_factor=0.18,
        min_box_size=0.04,
        arrow_scale=1.0
    )
    
    print("Python implementation completed successfully!")