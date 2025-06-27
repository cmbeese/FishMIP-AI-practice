#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Enhanced Food Web Diagram in Python
This script creates a publication-ready food web diagram with:
- Color coding based on functional groups or trophic levels
- Legend for node sizes and arrow thicknesses
- Improved text readability
- Customizable parameters
"""

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib.path import Path
import matplotlib.patheffects as path_effects
import matplotlib.colors as mcolors
from matplotlib.lines import Line2D
import seaborn as sns
# Optional imports for additional color palettes
try:
    import palettable
    PALETTABLE_AVAILABLE = True
except ImportError:
    PALETTABLE_AVAILABLE = False

try:
    import colorcet as cc
    COLORCET_AVAILABLE = True
except ImportError:
    COLORCET_AVAILABLE = False

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

# Function to determine node colors based on trophic level or functional group
def get_node_colors(groups, TL, type_values, color_scheme='trophic', palette_name=None):
    """
    Determine node colors based on trophic level or functional group
    
    Parameters:
    -----------
    groups : array-like
        Group names
    TL : array-like
        Trophic levels
    type_values : array-like
        Type values (for functional groups)
    color_scheme : str
        'trophic', 'functional', 'habitat', or 'none'
    palette_name : str, optional
        Name of the color palette to use (if None, uses default)
    
    Returns:
    --------
    colors : list
        List of colors for each node
    color_map : dict
        Mapping of categories to colors (for legend)
    """
    if color_scheme == 'trophic':
        # Create color bins based on trophic levels
        tl_bins = [1.0, 2.0, 3.0, 4.0, 5.0]
        tl_labels = ['Producers (TL 1)', 'Primary Consumers (TL 2)',
                    'Secondary Consumers (TL 3)', 'Tertiary Consumers (TL 4+)',
                    'Top Predators (TL 5+)']
        
        # Choose color palette based on palette_name
        if palette_name == 'default':
            # Original soft, nature-inspired colors
            colors_palette = ['#8cc751', '#5da9e9', '#fee781', '#fe9c8f', '#d64161']
        elif palette_name == 'viridis':
            # Use viridis colormap from matplotlib
            cmap = plt.cm.viridis
            colors_palette = [mcolors.rgb2hex(cmap(i/4)) for i in range(5)]
        elif palette_name == 'plasma':
            # Use plasma colormap from matplotlib
            cmap = plt.cm.plasma
            colors_palette = [mcolors.rgb2hex(cmap(i/4)) for i in range(5)]
        elif palette_name == 'seaborn':
            # Use seaborn color palette
            colors_palette = sns.color_palette("muted", 5).as_hex()
        elif PALETTABLE_AVAILABLE and palette_name == 'colorbrewer':
            # Use ColorBrewer palette from palettable
            colors_palette = palettable.colorbrewer.qualitative.Set2_5.hex_colors
        elif COLORCET_AVAILABLE and palette_name == 'colorcet':
            # Use colorcet palette
            cmap = cc.cm.rainbow
            colors_palette = [mcolors.rgb2hex(cmap(i/4)) for i in range(5)]
        else:
            # Default to a colorblind-friendly palette
            colors_palette = sns.color_palette("colorblind", 5).as_hex()
        
        # Assign colors based on trophic level
        colors = []
        for level in TL:
            if level < tl_bins[1]:
                colors.append(colors_palette[0])
            elif level < tl_bins[2]:
                colors.append(colors_palette[1])
            elif level < tl_bins[3]:
                colors.append(colors_palette[2])
            elif level < tl_bins[4]:
                colors.append(colors_palette[3])
            else:
                colors.append(colors_palette[4])
        
        # Create color map for legend
        color_map = {label: color for label, color in zip(tl_labels, colors_palette)}
        
    elif color_scheme == 'functional':
        # Define functional groups based on type values and group names
        # Type 0 = consumer, Type 1 = producer, Type 2 = detritus
        functional_groups = []
        
        for i, (group, type_val) in enumerate(zip(groups, type_values)):
            group_lower = group.lower()
            if type_val == 1.0:
                functional_groups.append('Producer')
            elif type_val == 2.0:
                functional_groups.append('Detritus')
            elif 'fish' in group_lower or 'shark' in group_lower:
                functional_groups.append('Fish')
            elif 'bird' in group_lower or 'cetacean' in group_lower:
                functional_groups.append('Top Predator')
            elif 'zoo' in group_lower:
                functional_groups.append('Zooplankton')
            elif 'bacteria' in group_lower:
                functional_groups.append('Bacteria')
            elif 'bivalve' in group_lower or 'gastropod' in group_lower:
                functional_groups.append('Mollusc')
            elif 'crab' in group_lower or 'crayfish' in group_lower:
                functional_groups.append('Crustacean')
            else:
                functional_groups.append('Other Invertebrate')
        
        # Get unique functional groups
        unique_groups = sorted(set(functional_groups))
        num_groups = len(unique_groups)
        
        # Choose color palette based on palette_name
        if palette_name == 'default':
            # Original palette
            func_colors = {
                'Producer': '#8cc751',       # Fresh green
                'Detritus': '#8d6e63',       # Warm brown
                'Fish': '#5da9e9',           # Ocean blue
                'Top Predator': '#d64161',   # Deep red
                'Zooplankton': '#fee781',    # Soft yellow
                'Bacteria': '#bdbdbd',       # Silver gray
                'Mollusc': '#f48fb1',        # Soft pink
                'Crustacean': '#90caf9',     # Light blue
                'Other Invertebrate': '#ce93d8' # Lavender
            }
        elif palette_name == 'seaborn':
            # Use seaborn color palette
            palette = sns.color_palette("husl", num_groups).as_hex()
            func_colors = {group: palette[i] for i, group in enumerate(unique_groups)}
        elif PALETTABLE_AVAILABLE and palette_name == 'tableau':
            # Use Tableau palette from palettable
            if num_groups <= 10:
                palette = palettable.tableau.Tableau_10.hex_colors
                func_colors = {group: palette[i % 10] for i, group in enumerate(unique_groups)}
            else:
                palette = palettable.tableau.Tableau_20.hex_colors
                func_colors = {group: palette[i % 20] for i, group in enumerate(unique_groups)}
        elif palette_name == 'tab10':
            # Use tab10 colormap from matplotlib
            palette = plt.cm.tab10.colors
            func_colors = {group: mcolors.rgb2hex(palette[i % 10]) for i, group in enumerate(unique_groups)}
        else:
            # Default to a colorblind-friendly palette
            palette = sns.color_palette("colorblind", num_groups).as_hex()
            func_colors = {group: palette[i] for i, group in enumerate(unique_groups)}
        
        # Assign colors based on functional groups
        colors = [func_colors[fg] for fg in functional_groups]
        
        # Create color map for legend (only include groups that are present)
        color_map = {k: v for k, v in func_colors.items() if k in functional_groups}
    
    elif color_scheme == 'habitat':
        # Define habitat/environment groups based on group names and type values
        habitat_groups = []
        
        for i, (group, type_val) in enumerate(zip(groups, type_values)):
            group_lower = group.lower()
            if 'pelagic' in group_lower or 'tuna' in group_lower or 'mackerel' in group_lower:
                habitat_groups.append('Pelagic')
            elif 'demersal' in group_lower or 'snapper' in group_lower or 'gurnard' in group_lower or 'flatfish' in group_lower:
                habitat_groups.append('Demersal')
            elif 'reef' in group_lower:
                habitat_groups.append('Reef')
            elif 'benthic' in group_lower or 'benthos' in group_lower or 'crab' in group_lower or 'star' in group_lower:
                habitat_groups.append('Benthic')
            elif 'zoo' in group_lower or 'phyto' in group_lower or 'plankton' in group_lower:
                habitat_groups.append('Planktonic')
            elif type_val == 1.0:
                habitat_groups.append('Primary Producer')
            elif type_val == 2.0:
                habitat_groups.append('Detritus')
            elif 'bird' in group_lower or 'cetacean' in group_lower:
                habitat_groups.append('Air-breathing')
            else:
                habitat_groups.append('Other')
        
        # Get unique habitat groups
        unique_groups = sorted(set(habitat_groups))
        num_groups = len(unique_groups)
        
        # Choose color palette based on palette_name
        if palette_name == 'default':
            # Original harmonious palette
            habitat_colors = {
                'Pelagic': '#0077b6',        # Deep blue
                'Demersal': '#48cae4',       # Medium blue
                'Reef': '#00b4d8',           # Turquoise
                'Benthic': '#8d6e63',        # Brown
                'Planktonic': '#90e0ef',     # Light blue
                'Primary Producer': '#70e000',# Bright green
                'Detritus': '#6c584c',       # Dark brown
                'Air-breathing': '#caf0f8',  # Sky blue
                'Other': '#adb5bd'           # Gray
            }
        elif palette_name == 'cividis':
            # Use cividis colormap from matplotlib
            cmap = plt.cm.cividis
            palette = [mcolors.rgb2hex(cmap(i/(num_groups-1) if num_groups > 1 else 0)) for i in range(num_groups)]
            habitat_colors = {group: palette[i] for i, group in enumerate(unique_groups)}
        elif palette_name == 'seaborn':
            # Use seaborn color palette
            palette = sns.color_palette("deep", num_groups).as_hex()
            habitat_colors = {group: palette[i] for i, group in enumerate(unique_groups)}
        elif COLORCET_AVAILABLE and palette_name == 'colorcet':
            # Use colorcet palette
            cmap = cc.cm.blues
            palette = [mcolors.rgb2hex(cmap(i/(num_groups-1) if num_groups > 1 else 0)) for i in range(num_groups)]
            habitat_colors = {group: palette[i] for i, group in enumerate(unique_groups)}
        elif PALETTABLE_AVAILABLE and palette_name == 'cmocean':
            # Use cmocean palette from palettable
            if num_groups <= 8:
                palette = palettable.cmocean.sequential.Deep_8.hex_colors
                habitat_colors = {group: palette[i % 8] for i, group in enumerate(unique_groups)}
            else:
                palette = palettable.cmocean.sequential.Deep_20.hex_colors
                habitat_colors = {group: palette[i % 20] for i, group in enumerate(unique_groups)}
        else:
            # Default to a blue-green palette
            palette = sns.color_palette("ocean", num_groups).as_hex()
            habitat_colors = {group: palette[i] for i, group in enumerate(unique_groups)}
        
        # Assign colors based on habitat groups
        colors = [habitat_colors[hg] for hg in habitat_groups]
        
        # Create color map for legend (only include groups that are present)
        color_map = {k: v for k, v in habitat_colors.items() if k in habitat_groups}
    
    else:
        # Default to a single color if no valid scheme is specified
        colors = ['white'] * len(groups)
        color_map = {'All Groups': 'white'}
    
    return colors, color_map

# Function to plot the food web diagram
def plot_foodweb_diagram(groups, xpos, TL, B, DD, QQ, type_values=None, output_file=None,
                         box_scale_factor=0.18, min_box_size=0.04, max_box_size=0.4,
                         arrow_scale=1.0, color_scheme='trophic', palette_name='default',
                         text_contrast=True, font_size=9, title=None):
    """
    Plot the food web diagram using matplotlib with enhanced features
    
    Parameters:
    -----------
    groups : array-like
        Group names
    xpos : array-like
        X-positions for each group
    TL : array-like
        Trophic levels for each group
    B : array-like
        Biomass values for each group
    DD : array-like
        Diet composition matrix
    QQ : array-like
        QQ matrix
    type_values : array-like, optional
        Type values for functional group coloring
    output_file : str, optional
        Path to save the output image
    box_scale_factor : float, optional
        Scaling factor for node sizes
    min_box_size : float, optional
        Minimum node size
    max_box_size : float, optional
        Maximum node size
    arrow_scale : float, optional
        Scaling factor for arrow widths
    color_scheme : str, optional
        'trophic', 'functional', 'habitat', or 'none'
    palette_name : str, optional
        Name of the color palette to use (default, seaborn, viridis, etc.)
    text_contrast : bool, optional
        Whether to add contrast to text labels
    font_size : int, optional
        Base font size for text labels
    title : str, optional
        Title for the diagram
    """
    # Filter out groups with invalid positions (e.g., -9999)
    valid_idx = np.where(xpos > -999)[0]
    groups = groups[valid_idx]
    xpos = xpos[valid_idx]
    TL = TL[valid_idx]
    B = B[valid_idx]
    if type_values is not None:
        type_values = type_values[valid_idx]
    DD = DD[valid_idx, :][:, valid_idx]
    QQ = QQ[valid_idx, :][:, valid_idx]
    
    # Shorten group names for better readability
    shortened_groups = []
    for name in groups:
        # Remove common words and shorten
        name = name.replace('_', ' ')
        name = name.replace('fish', '')
        name = name.replace('Fish', '')
        name = name.replace('benthos', 'ben')
        name = name.replace('Benthos', 'Ben')
        name = name.replace('plankton', 'plk')
        name = name.replace('Plankton', 'Plk')
        # Trim whitespace and capitalize first letter
        name = name.strip()
        if name:
            name = name[0].upper() + name[1:] if len(name) > 1 else name.upper()
        shortened_groups.append(name)
    
    # Ensure vertical positions exactly match trophic levels
    # The y-coordinate will be exactly the trophic level value
    
    # Rescale and adjust x-positions to spread nodes more evenly
    # Original range is 0-1, new range is 0-4 with better spacing
    xpos = xpos * 4.0  # Increased from 3.0 to 4.0 for more horizontal space
    
    # Add small random offsets to prevent overlaps
    np.random.seed(42)  # For reproducibility
    xpos += np.random.uniform(-0.1, 0.1, size=len(xpos))
    
    # Ensure x-positions stay within bounds
    xpos = np.clip(xpos, 0.1, 3.9)
    
    # Further adjust positions to prevent circle overlaps - improved algorithm
    n_groups = len(groups)
    min_distance = 0.3  # Minimum distance between circle centers
    
    # Improved overlap prevention algorithm with adaptive step size
    max_iterations = 10  # Increased from 5 for better convergence
    convergence_threshold = 0.001  # Stop when movements are small
    
    for iteration in range(max_iterations):
        max_movement = 0
        
        # Create a spatial grid for faster neighbor finding
        grid_size = 0.3
        grid = {}
        for i in range(n_groups):
            grid_x = int(xpos[i] / grid_size)
            grid_y = int(TL[i] / grid_size)
            if (grid_x, grid_y) not in grid:
                grid[(grid_x, grid_y)] = []
            grid[(grid_x, grid_y)].append(i)
        
        for i in range(n_groups):
            grid_x = int(xpos[i] / grid_size)
            grid_y = int(TL[i] / grid_size)
            
            # Check only nearby grid cells
            neighbors = []
            for dx in [-1, 0, 1]:
                for dy in [-1, 0, 1]:
                    key = (grid_x + dx, grid_y + dy)
                    if key in grid:
                        neighbors.extend(grid[key])
            
            for j in neighbors:
                if i == j:
                    continue
                    
                # Calculate distance between circles
                dx = xpos[j] - xpos[i]
                dy = TL[j] - TL[i]
                distance = np.sqrt(dx**2 + dy**2)
                
                # If circles are too close, push them apart
                if distance < min_distance:
                    # Calculate push direction
                    push_x = dx / (distance + 1e-10)  # Avoid division by zero
                    
                    # Push amount (half the overlap) with adaptive step size
                    push_amount = (min_distance - distance) / 2
                    push_amount *= (1.0 - iteration / max_iterations)  # Reduce step size over iterations
                    
                    # Only push horizontally to maintain trophic levels
                    old_x_i = xpos[i]
                    old_x_j = xpos[j]
                    
                    xpos[i] -= push_x * push_amount
                    xpos[j] += push_x * push_amount
                    
                    # Track maximum movement
                    movement = max(abs(xpos[i] - old_x_i), abs(xpos[j] - old_x_j))
                    max_movement = max(max_movement, movement)
        
        # Check for convergence
        if max_movement < convergence_threshold:
            break
    
    # Ensure x-positions stay within bounds after adjustment
    xpos = np.clip(xpos, 0.1, 3.9)
    
    # Calculate box sizes based on biomass - more closely matching IDL implementation
    # Use a stronger scaling factor to make biomass differences more apparent
    # Increase the overall size for better text readability
    box_sizes = box_scale_factor * 1.5 * (B**0.6)  # Increased overall size by 50%
    box_sizes[box_sizes < min_box_size * 1.5] = min_box_size * 1.5  # Increased minimum size
    # Add upper limit for very large boxes
    box_sizes[box_sizes > max_box_size * 1.5] = max_box_size * 1.5  # Increased maximum size
    
    # Get node colors based on the selected scheme
    if color_scheme != 'none':
        node_colors, color_map = get_node_colors(groups, TL, type_values, color_scheme, palette_name)
    else:
        node_colors = ['white'] * n_groups
        color_map = {'All Groups': 'white'}
    
    # Create a much wider figure with white background
    fig, ax = plt.subplots(figsize=(20, 10), facecolor='white')  # Increased height for legend
    ax.set_facecolor('white')
    
    # Set up the plot area with even wider x-axis limits
    ax.set_xlim(-0.2, 4.2)  # Increased x-axis range to match the 4.0 scaling
    ax.set_ylim(0.8, 5.2)
    ax.set_ylabel('Trophic level', fontsize=14)
    ax.set_yticks([1, 2, 3, 4, 5])
    ax.set_yticklabels(['1', '2', '3', '4', '5'])
    ax.set_xticks([])
    
    # Add title if provided
    if title:
        ax.set_title(title, fontsize=16, pad=20)
    
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
        
        # Calculate the distance from the end point to the target circle
        # We want to place the arrowhead just before it reaches the circle
        end_node = node_info[j]
        end_radius = end_node['radius']
        
        # Find a point along the curve that's outside the target circle
        # Start from the end and move backwards
        for arrow_idx in range(len(x)-2, 0, -1):
            # Calculate distance from this point to the circle center
            dist_to_center = np.sqrt((x[arrow_idx] - end_node['x'])**2 +
                                     (y[arrow_idx] - end_node['y'])**2)
            
            # If we're outside the circle (with a small buffer), use this point
            if dist_to_center > end_radius + arrow_size*2:
                break
        
        # Calculate direction for the arrow
        dx = x[arrow_idx+1] - x[arrow_idx]
        dy = y[arrow_idx+1] - y[arrow_idx]
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
        # Create circle with color
        circle = patches.Circle(
            (xpos[i], TL[i]),
            radius=box_size/1.5,  # Adjust radius to make area proportional to biomass
            linewidth=1, edgecolor='black', facecolor=node_colors[i]
        )
        ax.add_patch(circle)
        
        # Format group name for display with text wrapping
        display_name = shortened_groups[i]
        
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
        adjusted_font_size = font_size if len(wrapped_text) < 15 else (font_size - 1)
        
        # Add text with contrast enhancement if requested
        if text_contrast:
            # Add text with white outline for better readability
            text = ax.text(xpos[i], TL[i], wrapped_text,
                          ha='center', va='center', fontsize=adjusted_font_size,
                          fontweight='bold', color='black')
            
            # Add white outline/shadow effect
            text.set_path_effects([
                path_effects.Stroke(linewidth=2, foreground='white'),
                path_effects.Normal()
            ])
        else:
            # Standard text without outline
            ax.text(xpos[i], TL[i], wrapped_text,
                   ha='center', va='center', fontsize=adjusted_font_size)
    
    # Add legend for node colors
    if color_scheme != 'none':
        legend_elements = []
        for label, color in color_map.items():
            legend_elements.append(
                patches.Patch(facecolor=color, edgecolor='black', label=label)
            )
        
        # Add legend for node sizes
        # Calculate representative sizes for legend
        size_values = [min_box_size, (min_box_size + max_box_size)/2, max_box_size]
        size_labels = ['Small Biomass', 'Medium Biomass', 'Large Biomass']
        
        for size, label in zip(size_values, size_labels):
            legend_elements.append(
                patches.Circle((0, 0), radius=size/1.5, facecolor='white',
                               edgecolor='black', label=label)
            )
        
        # Add legend for arrow thicknesses
        arrow_widths = [0.5, 2.0, 4.0]
        arrow_labels = ['Weak Interaction', 'Medium Interaction', 'Strong Interaction']
        
        for width, label in zip(arrow_widths, arrow_labels):
            legend_elements.append(
                Line2D([0], [0], color='black', linewidth=width, label=label)
            )
        
        # Place legend outside the main plot area (below the plot)
        ax.legend(handles=legend_elements, loc='upper center',
                 bbox_to_anchor=(0.5, -0.15), ncol=4, fontsize=12,
                 frameon=True, fancybox=True, shadow=True)
    
    # Save the figure if output file is specified
    if output_file:
        plt.savefig(output_file, dpi=300, bbox_inches='tight', facecolor='white')
        print(f"Food web diagram saved to: {output_file}")
    
    plt.close()
    return fig

# Main function to create the food web diagram
def create_enhanced_foodweb_diagram(key_file, diet_file, tl_file, xpos_file, output_file=None,
                                   box_scale_factor=0.18, min_box_size=0.04, max_box_size=0.4,
                                   arrow_scale=1.0, color_scheme='trophic', palette_name='default',
                                   text_contrast=True, font_size=9, title=None):
    """
    Create an enhanced food web diagram from the input files
    
    Parameters:
    -----------
    key_file : str
        Path to the ecosystem file
    diet_file : str
        Path to the diet composition file
    tl_file : str
        Path to the trophic levels file
    xpos_file : str
        Path to the x-positions file
    output_file : str, optional
        Path to save the output image
    box_scale_factor : float, optional
        Scaling factor for node sizes
    min_box_size : float, optional
        Minimum node size
    max_box_size : float, optional
        Maximum node size
    arrow_scale : float, optional
        Scaling factor for arrow widths
    color_scheme : str, optional
        'trophic', 'functional', 'habitat', or 'none'
    palette_name : str, optional
        Name of the color palette to use (default, seaborn, viridis, etc.)
    text_contrast : bool, optional
        Whether to add contrast to text labels
    font_size : int, optional
        Base font size for text labels
    title : str, optional
        Title for the diagram
    """
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
        type_values=eco_data['type'],
        output_file=output_file,
        box_scale_factor=box_scale_factor,
        min_box_size=min_box_size,
        max_box_size=max_box_size,
        arrow_scale=arrow_scale,
        color_scheme=color_scheme,
        palette_name=palette_name,
        text_contrast=text_contrast,
        font_size=font_size,
        title=title
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

# Run the enhanced food web diagram creation
if __name__ == "__main__":
    # Create output directory if it doesn't exist
    if not os.path.exists("images"):
        os.makedirs("images")
    
    # Create the enhanced version with trophic level coloring
    result_trophic = create_enhanced_foodweb_diagram(
        key_file="data/HG04-key-adj.out",
        diet_file="data/HG04-diets-adj.out",
        tl_file="data/HG04-key-adj-out-trophic_levels.out",
        xpos_file="data/HG04-xpos0.txt",
        output_file="images/Python-enhanced-trophic.png",
        box_scale_factor=0.18,
        min_box_size=0.04,
        max_box_size=0.4,
        arrow_scale=1.0,
        color_scheme='trophic',
        text_contrast=True,
        font_size=9,
        title="Hauraki Gulf Food Web - Colored by Trophic Level"
    )
    
    # Create the enhanced version with functional group coloring
    result_functional = create_enhanced_foodweb_diagram(
        key_file="data/HG04-key-adj.out",
        diet_file="data/HG04-diets-adj.out",
        tl_file="data/HG04-key-adj-out-trophic_levels.out",
        xpos_file="data/HG04-xpos0.txt",
        output_file="images/Python-enhanced-functional.png",
        box_scale_factor=0.18,
        min_box_size=0.04,
        max_box_size=0.4,
        arrow_scale=1.0,
        color_scheme='functional',
        text_contrast=True,
        font_size=9,
        title="Hauraki Gulf Food Web - Colored by Functional Group"
    )
    
    # Create the enhanced version with habitat-based coloring
    result_habitat = create_enhanced_foodweb_diagram(
        key_file="data/HG04-key-adj.out",
        diet_file="data/HG04-diets-adj.out",
        tl_file="data/HG04-key-adj-out-trophic_levels.out",
        xpos_file="data/HG04-xpos0.txt",
        output_file="images/Python-enhanced-habitat.png",
        box_scale_factor=0.18,
        min_box_size=0.04,
        max_box_size=0.4,
        arrow_scale=1.0,
        color_scheme='habitat',
        text_contrast=True,
        font_size=9,
        title="Hauraki Gulf Food Web - Colored by Habitat"
    )
    
    print("Enhanced Python implementation completed successfully!")