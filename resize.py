from PIL import Image

# Open the square TIFF image
input_image_path = "map_tiff/map_Sarkidiornis melanotos_100.tiff"
try:
    square_image = Image.open(input_image_path)
except IOError:
    print("Unable to open image:", input_image_path)
    exit()

# Get the dimensions of the original image
original_width, original_height = square_image.size
print("Original image size:", original_width, "x", original_height)

# Calculate the new width and height to fit into the canvas (keeping the aspect ratio)
desired_width_mm = 26.5
desired_height_mm = 28

# Calculate the aspect ratio
aspect_ratio = original_width / original_height

# Calculate the new height based on the desired width
new_width = int(desired_width_mm * 300 / 25.4)
new_height = int(new_width / aspect_ratio)

# Resize the image with the calculated new dimensions
resized_image = square_image.resize((new_width, new_height), Image.LANCZOS)

# Create a new blank canvas with the desired dimensions
canvas_width = int(desired_width_mm * 300 / 25.4)
canvas_height = int(desired_height_mm * 300 / 25.4)
new_canvas = Image.new("RGB", (canvas_width, canvas_height), color="white")

# Calculate the position to paste the resized image onto the canvas
paste_position = ((canvas_width - new_width) // 2, (canvas_height - new_height) // 2)

# Paste the resized image onto the canvas
new_canvas.paste(resized_image, paste_position)

# Save the transformed image as TIFF format
output_image_path = "map_tiff/output_transformed_image.tiff"
new_canvas.save(output_image_path, dpi=(300, 300))
print("Image saved as:", output_image_path)
