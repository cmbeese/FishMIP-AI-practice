import os
import base64
import re
from pathlib import Path

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

def embed_images_in_html(html_path, output_path):
    """Embed all images in the HTML file as base64-encoded data URLs."""
    # Read the HTML file
    with open(html_path, 'r', encoding='utf-8') as file:
        html_content = file.read()
    
    # Get the directory of the HTML file
    html_dir = os.path.dirname(os.path.abspath(html_path))
    
    # Find all image src attributes
    img_pattern = re.compile(r'<img\s+[^>]*src="([^"]+)"[^>]*>')
    img_matches = img_pattern.findall(html_content)
    
    # Process each unique image path
    processed_paths = set()
    for img_path in img_matches:
        if img_path in processed_paths or img_path.startswith('data:'):
            continue
        
        # Convert relative path to absolute path
        abs_img_path = os.path.join(html_dir, img_path)
        
        # Check if the file exists
        if not os.path.exists(abs_img_path):
            print(f"Warning: Image file not found: {abs_img_path}")
            continue
        
        # Convert image to base64
        base64_data = convert_to_base64(abs_img_path)
        if base64_data:
            # Replace all occurrences of this image path in the HTML
            html_content = html_content.replace(f'src="{img_path}"', f'src="{base64_data}"')
            processed_paths.add(img_path)
            print(f"Embedded: {img_path}")
    
    # Write the modified HTML to the output file
    with open(output_path, 'w', encoding='utf-8') as file:
        file.write(html_content)
    
    print(f"\nCreated self-contained HTML file: {output_path}")
    print(f"Embedded {len(processed_paths)} unique images.")

if __name__ == "__main__":
    # Input and output file paths
    input_html = "foodweb_comparison.html"
    output_html = "foodweb_comparison_embedded.html"
    
    embed_images_in_html(input_html, output_html)