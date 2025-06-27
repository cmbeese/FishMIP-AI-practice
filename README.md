# Hauraki Gulf Food Web Diagram

This project provides implementations of the food web diagram visualization originally created in IDL by M. Pinkerton, reimplemented in both R and Python. It visualizes the trophic relationships between species in the Hauraki Gulf marine ecosystem.

## Project Structure

The project is organized into the following directories:

- `data/`: Contains the input data files
- `images/`: Output directory for the generated diagrams
- `R_implementation/`: R implementation of the food web diagram
- `Python_implementation/`: Python implementation of the food web diagram
- `IDL/`: Original IDL implementation by M. Pinkerton (for reference)

## Data Files

The code expects the following data files in the `data/` directory:

- `HG04-key-adj.out`: Contains ecological parameters for each species/group
- `HG04-diets-adj.out`: Diet composition matrix
- `HG04-key-adj-out-trophic_levels.out`: Trophic levels for each species
- `HG04-xpos0.txt`: Initial horizontal positions for species in the diagram

## Running the Code

### Option 1: Run Both Implementations

To run both the R and Python implementations, use the consolidated script:

```bash
python run_all_foodweb_diagrams.py
```

This script will run both implementations and save the output images in the `images/` directory with clear naming conventions.

### Option 2: Run R Implementation Only

To run only the R implementation:

```bash
Rscript R_implementation/run_foodweb_diagram.R
```

For Windows users with a custom R installation path (e.g., `C:\Users\FrymanK\Documents\R-4.5.1\bin\x64\R.exe`), use the full path to Rscript:

```bash
"C:\Users\FrymanK\Documents\R-4.5.1\bin\x64\Rscript.exe" R_implementation/run_foodweb_diagram.R
```

or from within R:

```r
source("R_implementation/run_foodweb_diagram.R")
```

### Option 3: Run Python Implementation Only

To run only the Python implementation:

```bash
python Python_implementation/run_foodweb_diagram.py
```

## Output Files

All output files are saved to the `images/` directory with the following naming convention:

- `R-trophic.png`: Food web diagram created with R using trophic level coloring
- `R-functional.png`: Food web diagram created with R using functional group coloring
- `R-habitat.png`: Food web diagram created with R using habitat-based coloring
- `Python-diagram-trophic.png`: Food web diagram created with Python using trophic level coloring
- `Python-diagram-functional.png`: Food web diagram created with Python using functional group coloring
- `Python-diagram-habitat.png`: Food web diagram created with Python using habitat-based coloring
- `Python-diagram-none.png`: Food web diagram created with Python without color coding

When using custom color palettes, the naming convention remains the same as the palette selection doesn't affect the filename.

## Dependencies

### R Dependencies

The R implementation requires the following packages:
- readr
- dplyr
- ggplot2
- gridExtra
- ggforce
- grid
- scales
- viridis
- optparse

Install them with:

```r
# Run the provided installation script
source("R_implementation/install_packages.R")
```

For Windows users with a custom R installation path, you can run the installation script from the command line:

```bash
"C:\Users\FrymanK\Documents\R-4.5.1\bin\x64\R.exe" -e "source('R_implementation/install_packages.R')"
```

### Python Dependencies

The Python implementation requires the following packages:
- numpy
- pandas
- matplotlib
- seaborn
- palettable (optional)
- colorcet (optional)

Install them with:

```bash
pip install -r Python_implementation/requirements.txt
```

## Implementation Details

Both implementations follow the same general approach:

1. Read and process the data files
2. Calculate the QQ matrix representing biomass flow
3. Position nodes based on trophic level (y-axis) and predefined x-positions
4. Draw boxes for each species with size proportional to biomass
5. Draw arrows between species representing feeding relationships

The implementations include the following features:
- Curved arrow paths for better visualization of feeding relationships
- Circle nodes with size proportional to biomass
- Highlighting of strong feeding relationships with thicker, darker arrows
- Multiple color coding options (trophic level, functional group, habitat)
- Visually appealing color palettes with colorblind-friendly options
- Comprehensive legends for node colors and arrow thicknesses
- Improved text readability with contrast enhancement

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

### Command-line Options

Both implementations can be run with command-line options:

#### Python Implementation

```bash
python Python_implementation/run_foodweb_diagram.py --color-scheme habitat --palette viridis
```

Available options:
```
--key-file FILE         Path to the ecosystem file
--diet-file FILE        Path to the diet composition file
--tl-file FILE          Path to the trophic levels file
--xpos-file FILE        Path to the x-positions file
--output-file FILE      Path to save the output image
--color-scheme SCHEME   Color scheme for nodes (trophic, functional, habitat, or none)
--palette PALETTE       Color palette to use (default, seaborn, viridis, plasma, colorbrewer, tab10, cividis, colorcet, cmocean)
--box-scale SCALE       Scaling factor for node sizes
--min-box-size SIZE     Minimum node size
--max-box-size SIZE     Maximum node size
--arrow-scale SCALE     Scaling factor for arrow widths
--text-contrast         Add contrast to text labels
--font-size SIZE        Base font size for text labels
--title TITLE           Title for the diagram
--all                   Generate all color scheme variants
```

#### R Implementation

```bash
Rscript R_implementation/run_foodweb_diagram.R --color-scheme habitat
```

For Windows users with a custom R installation path:

```bash
"C:\Users\FrymanK\Documents\R-4.5.1\bin\x64\Rscript.exe" R_implementation/run_foodweb_diagram.R --color-scheme habitat
```

Available options:
```
--key-file=FILE         Path to the ecosystem file
--diet-file=FILE        Path to the diet composition file
--tl-file=FILE          Path to the trophic levels file
--xpos-file=FILE        Path to the x-positions file
--output-file=FILE      Path to save the output image
--color-scheme=SCHEME   Color scheme for nodes (trophic, functional, habitat, or none)
--box-scale=SCALE       Scaling factor for node sizes
--min-box-size=SIZE     Minimum node size
--max-box-size=SIZE     Maximum node size
--arrow-scale=SCALE     Scaling factor for arrow widths
--text-contrast=BOOL    Add contrast to text labels
--font-size=SIZE        Base font size for text labels
--title=TITLE           Title for the diagram
--all                   Generate all color scheme variants
```

## Color Schemes

Both implementations offer three color coding schemes:

1. **Trophic Level Coloring**: Colors nodes based on their trophic level in the food web
   - Producers (TL 1): Green
   - Primary Consumers (TL 2): Blue
   - Secondary Consumers (TL 3): Yellow/Orange
   - Tertiary Consumers (TL 4+): Purple
   - Top Predators (TL 5+): Red

2. **Functional Group Coloring**: Colors nodes based on their functional role in the ecosystem
   - Producer: Green
   - Detritus: Brown
   - Fish: Blue
   - Top Predator: Red
   - Zooplankton: Yellow
   - Bacteria: Gray
   - Mollusc: Pink
   - Crustacean: Light Blue
   - Other Invertebrate: Purple

3. **Habitat-based Coloring**: Colors nodes based on their primary habitat or environment
   - Pelagic: Deep Blue
   - Demersal: Medium Blue
   - Reef: Turquoise
   - Benthic: Brown
   - Planktonic: Light Blue
   - Primary Producer: Bright Green
   - Detritus: Dark Brown
   - Air-breathing: Sky Blue
   - Other: Gray

The Python implementation also offers a "none" option that uses white for all nodes.

### Available Color Palettes (Python Implementation)
- `default`: Original nature-inspired colors
- `seaborn`: Professional statistical color palettes
- `viridis`: Perceptually uniform, colorblind-friendly palette
- `plasma`: Vibrant sequential palette
- `colorbrewer`: Well-designed cartographic palettes
- `tab10`: Categorical palette for distinct groups
- `cividis`: Colorblind-friendly yellow-blue palette
- `colorcet`: Perceptually uniform colormaps
- `cmocean`: Oceanography-inspired palettes

To generate all color scheme variants at once, use the `--all` flag:

```bash
python Python_implementation/run_foodweb_diagram.py --all
Rscript R_implementation/run_foodweb_diagram.R --all
```

For Windows users with a custom R installation path:

```bash
python Python_implementation/run_foodweb_diagram.py --all
"C:\Users\FrymanK\Documents\R-4.5.1\bin\x64\Rscript.exe" R_implementation/run_foodweb_diagram.R --all
```

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