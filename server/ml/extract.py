from ultralytics import YOLO
import pandas as pd
import os

# Load YOLO model
model = YOLO("models/yolo11n-seg.pt")

# List of images to process
image_paths = os.listdir('dataset')  # Add your image paths here

# Store extracted features
features_list = []

for image_path in image_paths:
    # Predict with the model
    results = model(f'dataset/{image_path}')
    
    # Process each detection in the results
    for result in results:
        # Save image with bounding boxes
        result.save(filename=f"results/result_{image_path}")
        
        # Check if there are any detections
        if result.boxes is not None and len(result.boxes) > 0:
            # Get bounding box coordinates
            boxes = result.boxes.xyxy.cpu().numpy()  # [x1, y1, x2, y2]
            confidences = result.boxes.conf.cpu().numpy()
            masks = result.masks
            
            for i, box in enumerate(boxes):
                x1, y1, x2, y2 = box
                
                # Calculate basic features
                width = x2 - x1
                height = y2 - y1
                area = width * height
                aspect_ratio = width / height
                
                # Store features
                features = {
                    'image_path': image_path,
                    'confidence': confidences[i],
                    'bbox_width': width,
                    'bbox_height': height,
                    'bbox_area': area,
                    'mask_area': masks[i].data.numpy().sum(),
                    'aspect_ratio': aspect_ratio,
                }
                
                features_list.append(features)
                print(f"Image: {image_path}")
                print(f"BBox Area: {area:.1f}, Aspect Ratio: {aspect_ratio:.2f}")
        
        else:
            print(f"No detections in {image_path}")

# Save features to CSV
if features_list:
    df = pd.DataFrame(features_list)
    df.to_csv('extracted_features.csv', index=False)
    print(f"\nSaved features for {len(features_list)} detections to 'extracted_features.csv'")
else:
    print("No features extracted - no detections found")