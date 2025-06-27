#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Consolidated script to run all food web diagram implementations (R and Python)
This script runs both the R and Python implementations of the food web diagram
and saves the output images with clear naming conventions.

Author: FishMIP Workshop Team
Last updated: June 2025
"""

import os
import sys
import subprocess
import platform
import time
import argparse
from pathlib import Path

def print_header(message):
    """Print a formatted header message."""
    print("\n" + "=" * 80)
    print(f" {message}")
    print("=" * 80)

def check_r_installation():
    """Check if R is installed and return the path to the R executable."""
    # Default R paths for different operating systems
    if platform.system() == "Windows":
        # Try to find R in common installation locations
        possible_paths = [
            r"C:\Program Files\R\R-4.5.1\bin\x64\R.exe",
            r"C:\Program Files\R\R-4.3.0\bin\x64\R.exe",
            r"C:\Program Files\R\R-4.2.0\bin\x64\R.exe",
            r"C:\Program Files\R\R-4.1.0\bin\x64\R.exe",
            r"C:\Program Files\R\R-4.0.0\bin\x64\R.exe",
            # Add user-specific path if needed
            r"C:\Users\FrymanK\Documents\R-4.5.1\bin\x64\R.exe"
        ]
        
        for path in possible_paths:
            if os.path.exists(path):
                return path
                
        print("Warning: R executable not found in common locations.")
        print("Please make sure R is installed and the path is correct.")
        return None
    elif platform.system() == "Darwin":  # macOS
        r_path = "R"  # Use R from PATH
    else:  # Linux and others
        r_path = "R"  # Use R from PATH
    
    # Test if R is working
    try:
        subprocess.run([r_path, "--version"], check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        return r_path
    except (subprocess.SubprocessError, FileNotFoundError):
        print("Warning: R is not installed or not in the system PATH.")
        print("Please make sure R is installed and accessible.")
        return None

def run_r_script(r_path, script_path, args=None):
    """Run an R script using the specified R executable."""
    if r_path is None:
        print(f"Skipping R script: {script_path} (R not available)")
        return False
    
    print(f"Running R script: {script_path}" +
          (f" with args: {' '.join(args)}" if args else ""))
    
    try:
        # Prepare command
        if platform.system() == "Windows":
            # For Windows, use Rscript.exe
            r_script_path = os.path.join(os.path.dirname(r_path), "Rscript.exe")
            if not os.path.exists(r_script_path):
                print(f"Warning: Rscript.exe not found at {r_script_path}")
                r_script_path = r_path  # Fall back to R.exe
            
            cmd = [r_script_path, script_path]
        else:
            # For Unix-like systems
            cmd = ["Rscript", script_path]
        
        # Add arguments if provided
        if args:
            cmd.extend(args)
        
        # Run the command
        process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        
        # Wait for the process to complete
        stdout, stderr = process.communicate()
        return_code = process.returncode
        
        # Print output
        if stdout:
            print(stdout)
        if stderr:
            print(f"Errors: {stderr}")
        
        if return_code == 0:
            print(f"R script completed successfully: {script_path}")
            return True
        else:
            print(f"Error running R script: {script_path} (return code: {return_code})")
            return False
    except Exception as e:
        print(f"Error running R script: {script_path}")
        print(f"Exception: {e}")
        return False

def run_python_script(script_path, args=None):
    """Run a Python script."""
    print(f"Running Python script: {script_path}" +
          (f" with args: {' '.join(args)}" if args else ""))
    
    try:
        # Prepare command
        cmd = [sys.executable, script_path]
        
        # Add arguments if provided
        if args:
            cmd.extend(args)
        
        # Run the command
        process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        
        # Wait for the process to complete
        stdout, stderr = process.communicate()
        return_code = process.returncode
        
        # Print output
        if stdout:
            print(stdout)
        if stderr:
            print(f"Errors: {stderr}")
        
        if return_code == 0:
            print(f"Python script completed successfully: {script_path}")
            return True
        else:
            print(f"Error running Python script: {script_path} (return code: {return_code})")
            return False
    except Exception as e:
        print(f"Error running Python script: {script_path}")
        print(f"Exception: {e}")
        return False

def check_dependencies():
    """Check if all required dependencies are installed."""
    print_header("Checking Dependencies")
    
    # Check Python dependencies
    python_deps = ["numpy", "pandas", "matplotlib", "seaborn"]
    python_missing = []
    
    for dep in python_deps:
        try:
            __import__(dep)
            print(f"✓ Python dependency: {dep}")
        except ImportError:
            python_missing.append(dep)
            print(f"✗ Missing Python dependency: {dep}")
    
    # Check optional Python dependencies
    optional_deps = ["palettable", "colorcet"]
    for dep in optional_deps:
        try:
            __import__(dep)
            print(f"✓ Optional Python dependency: {dep}")
        except ImportError:
            print(f"ℹ Optional Python dependency not found: {dep}")
    
    # Check R installation
    r_path = check_r_installation()
    if r_path:
        print(f"✓ R installation found: {r_path}")
    else:
        print("✗ R installation not found")
    
    # Return status
    if python_missing:
        print("\nMissing Python dependencies. Please install them with:")
        print(f"pip install {' '.join(python_missing)}")
        print("\nOr install all dependencies with:")
        print("pip install -r Python_implementation/requirements.txt")
    
    return len(python_missing) == 0, r_path

def parse_arguments():
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(description='Run all food web diagram implementations')
    
    # Input file options
    parser.add_argument('--key-file', type=str, default='data/HG04-key-adj.out',
                        help='Path to the ecosystem file')
    parser.add_argument('--diet-file', type=str, default='data/HG04-diets-adj.out',
                        help='Path to the diet composition file')
    parser.add_argument('--tl-file', type=str, default='data/HG04-key-adj-out-trophic_levels.out',
                        help='Path to the trophic levels file')
    parser.add_argument('--xpos-file', type=str, default='data/HG04-xpos0.txt',
                        help='Path to the x-positions file')
    
    # Visualization options
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
    parser.add_argument('--font-size', type=float, default=None,
                        help='Base font size for text labels (if not specified, uses implementation defaults)')
    parser.add_argument('--title', type=str, default=None,
                        help='Title for the diagrams')
    
    # Implementation options
    parser.add_argument('--python-only', action='store_true',
                        help='Run only the Python implementation')
    parser.add_argument('--r-only', action='store_true',
                        help='Run only the R implementation')
    parser.add_argument('--color-scheme', type=str, choices=['trophic', 'functional', 'habitat', 'none'],
                        help='Run a specific color scheme instead of all')
    parser.add_argument('--palette', type=str, default='default',
                        help='Color palette to use (Python implementation only)')
    
    # Output options
    parser.add_argument('--output-dir', type=str, default='images',
                        help='Directory to save output images')
    parser.add_argument('--compare', action='store_true',
                        help='Generate HTML comparison page after running')
    
    return parser.parse_args()

def main():
    """Main function to run all food web diagram implementations."""
    print_header("Hauraki Gulf Food Web Diagram - All Implementations")
    
    # Parse command-line arguments
    args = parse_arguments()
    
    # Check dependencies
    python_ok, r_path = check_dependencies()
    
    # Create output directory if it doesn't exist
    if not os.path.exists(args.output_dir):
        os.makedirs(args.output_dir)
        print(f"Created {args.output_dir} directory for output files")
    
    # Prepare file paths
    key_file = args.key_file
    diet_file = args.diet_file
    tl_file = args.tl_file
    xpos_file = args.xpos_file
    
    # Check if input files exist
    for file_path in [key_file, diet_file, tl_file, xpos_file]:
        if not os.path.exists(file_path):
            print(f"Error: Input file not found: {file_path}")
            return
    
    # Prepare script paths
    r_script = "R_implementation/run_foodweb_diagram.R"
    python_script = "Python_implementation/run_foodweb_diagram.py"
    
    # Check if scripts exist
    if not args.python_only and not os.path.exists(r_script):
        print(f"Error: R script not found: {r_script}")
        return
    
    if not args.r_only and not os.path.exists(python_script):
        print(f"Error: Python script not found: {python_script}")
        return
    
    # Prepare common arguments
    common_args = [
        f"--key-file={key_file}",
        f"--diet-file={diet_file}",
        f"--tl-file={tl_file}",
        f"--xpos-file={xpos_file}",
        f"--box-scale={args.box_scale}",
        f"--min-box-size={args.min_box_size}",
        f"--max-box-size={args.max_box_size}",
        f"--arrow-scale={args.arrow_scale}"
    ]
    
    # Add optional arguments
    if args.font_size is not None:
        common_args.append(f"--font-size={args.font_size}")
    
    if args.title is not None:
        common_args.append(f"--title={args.title}")
    
    # Add text contrast option
    if args.text_contrast:
        common_args.append("--text-contrast")
    
    # Run R implementation
    r_success = []
    if not args.python_only and r_path:
        print_header("Running R Implementation")
        
        # Prepare R-specific arguments
        r_args = common_args.copy()
        
        # Add color scheme or --all flag
        if args.color_scheme:
            r_args.append(f"--color-scheme={args.color_scheme}")
        else:
            r_args.append("--all")
        
        # Run R script
        success = run_r_script(r_path, r_script, r_args)
        r_success.append(success)
        
        if success:
            print(f"Successfully ran {r_script}")
        else:
            print(f"Failed to run {r_script}")
    
    # Run Python implementation
    py_success = []
    if not args.r_only and python_ok:
        print_header("Running Python Implementation")
        
        # Prepare Python-specific arguments
        py_args = common_args.copy()
        
        # Add color scheme or --all flag
        if args.color_scheme:
            py_args.append(f"--color-scheme={args.color_scheme}")
            py_args.append(f"--palette={args.palette}")
        else:
            py_args.append("--all")
            py_args.append(f"--palette={args.palette}")
        
        # Run Python script
        success = run_python_script(python_script, py_args)
        py_success.append(success)
        
        if success:
            print(f"Successfully ran {python_script}")
        else:
            print(f"Failed to run {python_script}")
    
    # Generate HTML comparison if requested
    if args.compare:
        print_header("Generating HTML Comparison")
        
        # Check for updated HTML comparison files
        if os.path.exists("foodweb_comparison_updated.html") and os.path.exists("embed_images_updated.py"):
            # Use the updated version
            print("Using updated HTML comparison template and embedding script")
            success = run_python_script("embed_images_updated.py")
            if success:
                print("Successfully generated updated HTML comparison")
                print("Output file: foodweb_comparison_embedded.html")
            else:
                print("Failed to generate updated HTML comparison")
        elif os.path.exists("foodweb_comparison.html") and os.path.exists("embed_images.py"):
            # Use the original version
            print("Using original HTML comparison template and embedding script")
            success = run_python_script("embed_images.py")
            if success:
                print("Successfully generated HTML comparison")
                print("Output file: foodweb_comparison_embedded.html")
            else:
                print("Failed to generate HTML comparison")
        else:
            print("Warning: HTML comparison files not found, skipping HTML comparison generation")
    
    # Print summary
    print_header("Summary")
    
    if not args.python_only and r_path:
        print("R Implementation:")
        status = "SUCCESS" if all(r_success) else "FAILED"
        print(f"  - {r_script}: {status}")
    
    if not args.r_only and python_ok:
        print("\nPython Implementation:")
        status = "SUCCESS" if all(py_success) else "FAILED"
        print(f"  - {python_script}: {status}")
    
    # Check if any images were created
    print("\nOutput Images:")
    if os.path.exists(args.output_dir):
        image_files = [f for f in os.listdir(args.output_dir) if f.endswith('.png')]
        if image_files:
            # Group images by type
            image_types = {
                'R-trophic': [],
                'R-functional': [],
                'R-habitat': [],
                'Python-diagram-trophic': [],
                'Python-diagram-functional': [],
                'Python-diagram-habitat': [],
                'Python-diagram-none': [],
                'Other': []
            }
            
            for image in sorted(image_files):
                categorized = False
                for image_type in list(image_types.keys())[:-1]:  # All except 'Other'
                    if image.startswith(image_type):
                        image_types[image_type].append(image)
                        categorized = True
                        break
                if not categorized:
                    image_types['Other'].append(image)
            
            # Print grouped images
            for image_type, images in image_types.items():
                if images:
                    print(f"\n  {image_type}:")
                    for image in images:
                        print(f"    - {os.path.join(args.output_dir, image)}")
        else:
            print("  No image files found in the output directory.")
    else:
        print("  Output directory not found.")
    
    # Print HTML comparison info if generated
    if args.compare and (os.path.exists("foodweb_comparison_embedded.html")):
        print("\nHTML Comparison:")
        print("  - foodweb_comparison_embedded.html (self-contained HTML with embedded images)")
    
    print("\nDone!")

if __name__ == "__main__":
    main()