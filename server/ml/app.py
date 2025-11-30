from flask import Flask, request, jsonify
import pickle
import numpy as np
import pandas as pd
import warnings
import requests
from PIL import Image
from io import BytesIO
from ultralytics import YOLO

warnings.filterwarnings('ignore', category=UserWarning)
app = Flask(__name__)

# Load models at startup
model_yolo = YOLO("models/yolo11n-seg.pt")
with open('models/linear_regression_banana.pkl', 'rb') as file:
    model_reg = pickle.load(file)

@app.route('/predict', methods=['POST'])
def predict():
    try:
        # Get URL and imageId from request
        data = request.get_json()
        image_url = data.get('url')
        image_id = data.get('imageId')
        
        if not image_url:
            return jsonify({'error': 'No URL provided'}), 400
        if not image_id:
            return jsonify({'error': 'No imageId provided'}), 400
        
        # Load image
        source = Image.open(BytesIO(requests.get(image_url).content))
        results = model_yolo(source)
        
        weight = 0
        detections = []
        
        # Process each detection
        for result in results:
            if result.boxes is not None and len(result.boxes) > 0:
                boxes = result.boxes.xyxy.cpu().numpy()
                confidences = result.boxes.conf.cpu().numpy()
                masks = result.masks
                
                for i, box in enumerate(boxes):
                    x1, y1, x2, y2 = box
                    width = x2 - x1
                    height = y2 - y1
                    area = width * height
                    aspect_ratio = width / height
                    
                    # Create features DataFrame
                    features = pd.DataFrame(
                        [[confidences[i], width, height, area, masks[i].data.numpy().sum(), aspect_ratio]]
                    )
                    prediction = model_reg.predict(features)
                    weight += prediction[0]
                    
                    detections.append({
                        'confidence': float(confidences[i]),
                        'area': float(area),
                        'predicted_weight': float(prediction[0])
                    })
        
        # Update image in Haskell API
        update_url = f'http://api:8080/images/{image_id}' # "http://localhost:8080 if not on docker"
        update_payload = {'uiImageWeight': int(weight)}
        update_response = requests.put(update_url, json=update_payload)
        
        if update_response.status_code != 200:
            print(f"Warning: Failed to update image {image_id}: {update_response.text}")
        
        return jsonify({
            'total_weight': float(weight),
            'detections_count': len(detections),
            'detections': detections
        })
        
    except Exception as e:
        return jsonify({'error': str(e)}), 500

if __name__ == '__main__':
    app.run(host='0.0.0.0', debug=False, port=5000)