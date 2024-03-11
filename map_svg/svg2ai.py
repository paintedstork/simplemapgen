import os
import cairosvg

def convert_svg_to_ai(input_svg, output_ai):
    # Convert SVG to AI using cairosvg
    cairosvg.svg2pdf(url=input_svg, write_to=output_ai)

# Directory containing SVG files
svg_directory = "C:\Transfer\ShinyApps\simplemapgen\map_svg"

# Output directory for AI files
ai_output_directory = "C:\Transfer\ShinyApps\simplemapgen\map_ai"

# Ensure the output directory exists
os.makedirs(ai_output_directory, exist_ok=True)

# List all SVG files in the directory
svg_files = [f for f in os.listdir(svg_directory) if f.endswith(".svg")]

# Convert each SVG file to AI
for svg_file in svg_files:
    input_svg_path = os.path.join(svg_directory, svg_file)
    output_ai_path = os.path.join(ai_output_directory, os.path.splitext(svg_file)[0] + ".ai")
    print("Converting.")
    convert_svg_to_ai(input_svg_path, output_ai_path)

print("Conversion complete.")
