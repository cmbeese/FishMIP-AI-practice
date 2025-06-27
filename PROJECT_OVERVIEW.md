# Food Web Diagram Project Overview

This project provides implementations of the food web diagram visualization in both R and Python, based on the original IDL implementation by M. Pinkerton. The code allows you to visualize trophic relationships between species in marine ecosystems, with a focus on the Hauraki Gulf ecosystem.

## Project Structure

The project is organized into the following directories:

- `data/`: Contains the input data files
- `images/`: Output directory for the generated diagrams
- `R_implementation/`: R implementation of the food web diagram
- `Python_implementation/`: Python implementation of the food web diagram
- `IDL/`: Original IDL implementation by M. Pinkerton (for reference)

### Core Files

#### R Implementation
1. **`R_implementation/foodweb_diagram.R`**: The main R implementation file containing functions for reading data files and creating food web diagrams.
2. **`R_implementation/run_foodweb_diagram.R`**: Script that provides a command-line interface to run the R implementation.

#### Python Implementation
1. **`Python_implementation/foodweb_diagram.py`**: The main Python implementation file containing functions for reading data files and creating food web diagrams.
2. **`Python_implementation/run_foodweb_diagram.py`**: Script that provides a command-line interface to run the Python implementation.

#### Shared Files
1. **`run_all_foodweb_diagrams.py`**: Consolidated script to run both R and Python implementations.
2. **`README.md`**: Comprehensive documentation and usage instructions.
3. **`foodweb_comparison.html`**: Interactive HTML page for comparing the different visualizations.
4. **`embed_images.py`**: Script to convert the HTML file to a self-contained version with embedded images.

## Workflow

The typical workflow for using this project is:

1. **Prepare Data**: Ensure your data files are in the correct format (see Data Files section below).
2. **Run the Implementations**: Use either the consolidated script or individual implementation scripts.
3. **View the Results**: Examine the generated diagrams in the `images/` directory.
4. **Compare Visualizations**: Use the `foodweb_comparison.html` file to compare the different implementations.

## Data Files

The code expects four main data files in the `data/` directory:

1. **Ecosystem File** (e.g., `HG04-key-adj.out`): Contains ecological parameters for each species/group.
   - Columns: Group, type, B, P/B, Q/B, EE, P/Q, etc.
   - type: 0 = consumer, 1 = producer, 2 = detritus

2. **Diet File** (e.g., `HG04-diets-adj.out`): Diet composition matrix showing feeding relationships.
   - Rows: Predator species
   - Columns: Prey species
   - Values: Proportion of prey in predator's diet (0-1)

3. **Trophic Levels File** (e.g., `HG04-key-adj-out-trophic_levels.out`): Trophic levels for each species.
   - Columns: GROUP, Trophic_Level, Omnivory_Index

4. **X-Positions File** (e.g., `HG04-xpos0.txt`): Initial horizontal positions for species in the diagram.
   - Columns: Gstr0 (group name), xpos (0-1, or -9999 for excluded groups)

## Key Functions

### R Implementation (`foodweb_diagram.R`):

- **`read_ecosystem_file(file_path)`**: Reads the ecosystem parameters file.
- **`read_diet_file(file_path)`**: Reads the diet composition file.
- **`read_trophic_levels(file_path)`**: Reads the trophic levels file.
- **`read_xpos(file_path)`**: Reads the x-positions file.
- **`calculate_QQ(P, Q, DD)`**: Calculates the QQ matrix representing biomass flow.
- **`get_node_colors(groups, TL, type_values, color_scheme)`**: Determines node colors based on the selected scheme.
- **`wrap_text(text, width)`**: Wraps text to fit within node boundaries.
- **`create_foodweb_diagram(key_file, diet_file, tl_file, xpos_file, output_file, ...)`**: Main function that orchestrates the entire process.

### Python Implementation (`foodweb_diagram.py`):

- **`read_ecosystem_file(file_path)`**: Reads the ecosystem parameters file.
- **`read_diet_file(file_path)`**: Reads the diet composition file.
- **`read_trophic_levels(file_path)`**: Reads the trophic levels file.
- **`read_xpos(file_path)`**: Reads the x-positions file.
- **`calculate_QQ(P, Q, DD)`**: Calculates the QQ matrix representing biomass flow.
- **`quadratic_bezier(start, end, control, num_points)`**: Creates a quadratic Bezier curve for smooth arrow paths.
- **`wrap_text(text, width)`**: Wraps text to fit within node boundaries.
- **`get_node_colors(groups, TL, type_values, color_scheme, palette_name)`**: Determines node colors based on the selected scheme and palette.
- **`plot_foodweb_diagram(groups, xpos, TL, B, DD, QQ, ...)`**: Creates the food web diagram.
- **`create_foodweb_diagram(key_file, diet_file, tl_file, xpos_file, output_file, ...)`**: Main function that orchestrates the entire process.

## Visualization Options

The project provides several visualization options, all saved to the `images/` directory:

1. **Trophic Level Coloring**: Colors nodes based on their trophic level in the food web.
2. **Functional Group Coloring**: Colors nodes based on their functional role in the ecosystem.
3. **Habitat-based Coloring**: Colors nodes based on their primary habitat or environment.
4. **No Coloring** (Python only): Uses white for all nodes.

All visualization files are automatically saved to the `images/` directory with clear naming conventions:
- `R-trophic.png`, `R-functional.png`, `R-habitat.png`
- `Python-diagram-trophic.png`, `Python-diagram-functional.png`, `Python-diagram-habitat.png`, `Python-diagram-none.png`

## Example Usage

### Running Both Implementations

```bash
python run_all_foodweb_diagrams.py
```

### R Implementation

```r
# From R
source("R_implementation/run_foodweb_diagram.R")

# From command line
Rscript R_implementation/run_foodweb_diagram.R --color-scheme trophic
```

### Python Implementation

```python
# From command line
python Python_implementation/run_foodweb_diagram.py --color-scheme trophic --palette viridis
```

## Dependencies

### R Dependencies
- readr
- dplyr
- ggplot2
- gridExtra
- ggforce
- grid
- scales
- viridis
- optparse

### Python Dependencies
- numpy
- pandas
- matplotlib
- seaborn
- palettable (optional)
- colorcet (optional)

## Customization

Both implementations provide several parameters for customization:

- `box_scale_factor`: Controls the size of nodes
- `min_box_size`: Minimum node size
- `max_box_size`: Maximum node size
- `arrow_scale`: Controls arrow thickness
- `color_scheme`: Color coding scheme ('trophic', 'functional', 'habitat', or 'none')
- `palette_name`: Color palette to use (Python implementation only)
- `text_contrast`: Whether to add contrast to text labels
- `font_size`: Base font size for text labels
- `title`: Custom title for the diagram

## Creating Your Own Food Web Diagrams

To create food web diagrams for your own ecosystem:

1. Prepare your data files in the required format (following the structure of the example files).
2. Place your data files in the `data/` directory or specify custom file paths.
3. Run either implementation with appropriate parameters.
4. Explore different color schemes and customization options.

## Web Visualization

The project includes a web-based visualization for comparing the different implementations:

- `foodweb_comparison.html`: Interactive HTML page for comparing the different visualizations
- `foodweb_comparison_embedded.html`: Self-contained version with embedded images
- `embed_images.py`: Script to convert the HTML file to a self-contained version with embedded images

To create the self-contained HTML file:

```bash
python embed_images.py
```

## Acknowledgments

This project is based on the original IDL implementation by M. Pinkerton, likely from NIWA (National Institute of Water and Atmospheric Research) in New Zealand. The original code was designed for visualizing the Hauraki Gulf marine ecosystem.

This project was developed as part of the FishMIP Workshop to demonstrate different approaches to visualizing food web data across programming languages.