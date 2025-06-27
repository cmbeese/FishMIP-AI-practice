#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Enhanced Image Embedding Script for Food Web Diagram Comparison

This script creates a self-contained HTML file by:
1. Finding the most recent image files in the images directory
2. Embedding all images as base64-encoded data URLs
3. Adding a timestamp to show when the comparison was last updated

Author: FishMIP Workshop Team
Last updated: June 2025
"""

import os
import base64
import re
import glob
import datetime
from pathlib import Path
from collections import defaultdict

def find_latest_images():
    """Find the most recent versions of each image type in the images directory."""
    # Define the image patterns to look for
    image_patterns = {
        'r_trophic': 'images/R-trophic*.png',
        'r_functional': 'images/R-functional*.png',
        'r_habitat': 'images/R-habitat*.png',
        'python_trophic': 'images/Python-diagram-trophic*.png',
        'python_functional': 'images/Python-diagram-functional*.png',
        'python_habitat': 'images/Python-diagram-habitat*.png',
        'python_none': 'images/Python-diagram-none*.png',
    }
    
    # Find the latest version of each image type
    latest_images = {}
    for image_type, pattern in image_patterns.items():
        matching_files = glob.glob(pattern)
        if matching_files:
            # Sort by modification time (newest first)
            latest_file = max(matching_files, key=os.path.getmtime)
            latest_images[image_type] = latest_file
    
    # Add the original IDL image
    idl_image = 'HG04_present_0p18-0p40_diagram.png'
    if os.path.exists(idl_image):
        latest_images['idl_original'] = idl_image
    
    return latest_images

def convert_to_base64(image_path):
    """Convert an image file to a base64-encoded string."""
    try:
        with open(image_path, "rb") as image_file:
            encoded_string = base64.b64encode(image_file.read()).decode('utf-8')
            
            # Determine MIME type based on file extension
            ext = os.path.splitext(image_path)[1].lower()
            mime_type = {
                '.png': 'image/png',
                '.jpg': 'image/jpeg',
                '.jpeg': 'image/jpeg',
                '.gif': 'image/gif',
                '.svg': 'image/svg+xml'
            }.get(ext, 'image/png')
            
            return f"data:{mime_type};base64,{encoded_string}"
    except Exception as e:
        print(f"Error processing {image_path}: {e}")
        return None

def update_html_with_latest_images(html_path, output_path, latest_images):
    """Update the HTML file with the latest images and embed them as base64."""
    # Read the HTML file
    with open(html_path, 'r', encoding='utf-8') as file:
        html_content = file.read()
    
    # Create a mapping of image patterns to their latest versions
    image_mapping = {
        'images/R-trophic.png': latest_images.get('r_trophic', 'images/R-trophic.png'),
        'images/R-functional.png': latest_images.get('r_functional', 'images/R-functional.png'),
        'images/R-habitat.png': latest_images.get('r_habitat', 'images/R-habitat.png'),
        'images/Python-diagram-trophic.png': latest_images.get('python_trophic', 'images/Python-diagram-trophic.png'),
        'images/Python-diagram-functional.png': latest_images.get('python_functional', 'images/Python-diagram-functional.png'),
        'images/Python-diagram-habitat.png': latest_images.get('python_habitat', 'images/Python-diagram-habitat.png'),
        'images/Python-diagram-none.png': latest_images.get('python_none', 'images/Python-diagram-none.png'),
        'HG04_present_0p18-0p40_diagram.png': latest_images.get('idl_original', 'HG04_present_0p18-0p40_diagram.png')
    }
    
    # Find all image src attributes
    img_pattern = re.compile(r'<img\s+[^>]*src="([^"]+)"[^>]*>')
    img_matches = img_pattern.findall(html_content)
    
    # Process each unique image path
    processed_paths = set()
    for img_path in img_matches:
        if img_path in processed_paths or img_path.startswith('data:'):
            continue
        
        # Check if this is one of our mapped images
        actual_path = img_path
        for pattern, latest_path in image_mapping.items():
            if img_path == pattern and latest_path != pattern:
                actual_path = latest_path
                print(f"Updating {img_path} to {latest_path}")
                break
        
        # Convert image to base64
        if os.path.exists(actual_path):
            base64_data = convert_to_base64(actual_path)
            if base64_data:
                # Replace all occurrences of this image path in the HTML
                html_content = html_content.replace(f'src="{img_path}"', f'src="{base64_data}"')
                processed_paths.add(img_path)
                print(f"Embedded: {actual_path}")
        else:
            print(f"Warning: Image file not found: {actual_path}")
    
    # Update the timestamp
    timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    html_content = html_content.replace(
        'document.getElementById("timestamp").textContent = new Date().toLocaleString();',
        f'document.getElementById("timestamp").textContent = "{timestamp}";'
    )
    
    # Write the modified HTML to the output file
    with open(output_path, 'w', encoding='utf-8') as file:
        file.write(html_content)
    
    print(f"\nCreated self-contained HTML file: {output_path}")
    print(f"Embedded {len(processed_paths)} unique images.")
    print(f"Timestamp updated to: {timestamp}")

def main():
    """Main function to find latest images and create the embedded HTML file."""
    # Input and output file paths
    input_html = "foodweb_comparison_updated.html"
    output_html = "foodweb_comparison_embedded.html"
    
    # Find the latest images
    print("Finding latest images...")
    latest_images = find_latest_images()
    
    # Print the latest images found
    print("\nLatest images found:")
    for image_type, path in latest_images.items():
        print(f"  - {image_type}: {path}")
    
    # Update the HTML with the latest images
    print("\nUpdating HTML with latest images...")
    update_html_with_latest_images(input_html, output_html, latest_images)

if __name__ == "__main__":
    main()