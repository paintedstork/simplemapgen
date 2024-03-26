from PIL import Image
from PIL import ImageCms
import os

def convert_to_cmyk(input_path, output_path, cmyk_profile_path):
    try:
        # Open the JPG image
        image = Image.open(input_path)
        
        # Convert from RGB to CMYK
        if image.mode != "CMYK":
            rgb_profile = ImageCms.createProfile("sRGB")
            cmyk_profile = ImageCms.ImageCmsProfile(cmyk_profile_path)
            rgb_to_cmyk_transform = ImageCms.buildTransformFromOpenProfiles(rgb_profile, cmyk_profile, "RGB", "CMYK")
            cmyk_image = ImageCms.applyTransform(image, rgb_to_cmyk_transform)
        else:
            cmyk_image = image
        
        # Save as TIFF
        cmyk_image.save(output_path, format='TIFF')
        
        print(f"Converted {input_path} to CMYK TIFF format.")
    except Exception as e:
        print(f"Failed to convert {input_path}: {e}")

def batch_convert_to_cmyk(input_folder, output_folder, cmyk_profile_path):
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)

    for filename in os.listdir(input_folder):
        if filename.endswith('.jpg'):
            input_path = os.path.join(input_folder, filename)
            output_path = os.path.join(output_folder, filename.split('.')[0] + '.tiff')
            convert_to_cmyk(input_path, output_path, cmyk_profile_path)

if __name__ == "__main__":
    input_folder = "C:\\Transfer\\ShinyApps\\simplemapgen\\map"  # Provide the path to your input folder containing JPG files
    output_folder = "C:\\Transfer\\ShinyApps\\simplemapgen\\map_tiff"  # Provide the path to your output folder
    cmyk_profile_path = "USWebCoatedSWOP.icc"  # Provide the path to your CMYK ICC profile
    
    batch_convert_to_cmyk(input_folder, output_folder, cmyk_profile_path)
