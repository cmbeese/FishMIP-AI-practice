#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Run script for the enhanced food web diagram in Python
This script provides a command-line interface to generate enhanced food web diagrams
with various customization options.
"""

import os
import sys
import argparse
from enhanced_foodweb_diagram import create_enhanced_foodweb_diagram

def main():
    """Main function to run the enhanced food web diagram with command-line arguments"""
    # Create argument parser
    parser = argparse.ArgumentParser(description='Generate enhanced food web diagrams')
    
    # Add arguments
    parser.add_argument('--key-file', type=str, default='data/HG04-key-adj.out',
                        help='Path to the ecosystem file')
    parser.add_argument('--diet-file', type=str, default='data/HG04-diets-adj.out',
                        help='Path to the diet composition file')
    parser.add_argument('--tl-file', type=str, default='data/HG04-key-adj-out-trophic_levels.out',
                        help='Path to the trophic levels file')
    parser.add_argument('--xpos-file', type=str, default='data/HG04-xpos0.txt',
                        help='Path to the x-positions file')
    parser.add_argument('--output-file', type=str, default='images/Python-enhanced-diagram.png',
                        help='Path to save the output image')
    parser.add_argument('--color-scheme', type=str, default='trophic',
                        choices=['trophic', 'functional', 'habitat', 'none'],
                        help='Color scheme for nodes (trophic, functional, habitat, or none)')
    parser.add_argument('--palette', type=str, default='default',
                        choices=['default', 'seaborn', 'viridis', 'plasma', 'colorbrewer', 'tab10', 'cividis', 'colorcet', 'cmocean'],
                        help='Color palette to use for the selected color scheme')
    parser.add_argument('--box-scale', type=float, default=0.18,
                        help='Scaling factor for node sizes')
    parser.add_argument('--min-box-size', type=float, default=0.04,
                        help='Minimum node size')
    parser.add_argument('--max-box-size', type=float, default=0.4,
                        help='Maximum node size')
    parser.add_argument('--arrow-scale', type=float, default=1.0,
                        help='Scaling factor for arrow widths')
    parser.add_argument('--text-contrast', action='store_true', default=True,
                        help='Add contrast to text labels')
    parser.add_argument('--font-size', type=int, default=9,
                        help='Base font size for text labels')
    parser.add_argument('--title', type=str, default=None,
                        help='Title for the diagram')
    parser.add_argument('--all', action='store_true',
                        help='Generate all color scheme variants')
    
    # Parse arguments
    args = parser.parse_args()
    
    # Create output directory if it doesn't exist
    output_dir = os.path.dirname(args.output_file)
    if output_dir and not os.path.exists(output_dir):
        os.makedirs(output_dir)
        print(f"Created output directory: {output_dir}")
    
    if args.all:
        # Generate all color scheme variants
        color_schemes = ['trophic', 'functional', 'habitat', 'none']
        for scheme in color_schemes:
            # Create output filename based on color scheme
            base_name, ext = os.path.splitext(args.output_file)
            output_file = f"{base_name}-{scheme}{ext}"
            
            # Create title based on color scheme
            if args.title:
                title = f"{args.title} - {scheme.capitalize()} Coloring"
            else:
                title = f"Food Web Diagram - {scheme.capitalize()} Coloring"
            
            # Run the diagram creation
            print(f"Generating diagram with {scheme} coloring...")
            result = create_enhanced_foodweb_diagram(
                key_file=args.key_file,
                diet_file=args.diet_file,
                tl_file=args.tl_file,
                xpos_file=args.xpos_file,
                output_file=output_file,
                box_scale_factor=args.box_scale,
                min_box_size=args.min_box_size,
                max_box_size=args.max_box_size,
                arrow_scale=args.arrow_scale,
                color_scheme=scheme,
                palette_name=args.palette,
                text_contrast=args.text_contrast,
                font_size=args.font_size,
                title=title
            )
            
            if result:
                print(f"Successfully created diagram: {output_file}")
            else:
                print(f"Failed to create diagram: {output_file}")
    else:
        # Run the diagram creation with specified arguments
        result = create_enhanced_foodweb_diagram(
            key_file=args.key_file,
            diet_file=args.diet_file,
            tl_file=args.tl_file,
            xpos_file=args.xpos_file,
            output_file=args.output_file,
            box_scale_factor=args.box_scale,
            min_box_size=args.min_box_size,
            max_box_size=args.max_box_size,
            arrow_scale=args.arrow_scale,
            color_scheme=args.color_scheme,
            palette_name=args.palette,
            text_contrast=args.text_contrast,
            font_size=args.font_size,
            title=args.title
        )
        
        if result:
            print(f"Successfully created diagram: {args.output_file}")
        else:
            print(f"Failed to create diagram: {args.output_file}")
    
    print("Done!")

if __name__ == "__main__":
    main()