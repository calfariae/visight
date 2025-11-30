import pickle
import numpy as np
import pandas as pd
import warnings
import requests
from PIL import Image
from io import BytesIO
from ultralytics import YOLO

# Suppress the version warning if you can't retrain immediately
warnings.filterwarnings('ignore', category=UserWarning)

# Load YOLO model
model_yolo = YOLO("models/yolo11n-seg.pt")
# Load regression model
with open('models/linear_regression_banana.pkl', 'rb') as file:
    model_reg = pickle.load(file)

# Read image from URL using OpenCV

# One clean step
source = Image.open(BytesIO(requests.get('url').content))
results = model_yolo(source)

weight = 0

# Process each detection in the results
for result in results:
    # Save image with bounding boxes
    result.save(filename=f"result.jpg")
    
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
            
            # Create DataFrame with column names
            features = pd.DataFrame(
                [[confidences[i], width, height, area, masks[i].data.numpy().sum(), aspect_ratio]]
            )
            
            prediction = model_reg.predict(features)
            weight += prediction[0]
            
            # print(f"BBox Area: {area:.1f}, Aspect Ratio: {aspect_ratio:.2f}")
    
    else:
        print(f"No detections")

print(weight)


