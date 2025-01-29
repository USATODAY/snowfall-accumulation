# %%
from google.cloud import storage
import os
# Function to upload file to Google Cloud Storage
def upload_to_gcs(local_file_path, bucket_name, gcs_file_path):
    # Initialize a Cloud Storage client
    storage_client = storage.Client()
    # Reference to the bucket
    bucket = storage_client.bucket(bucket_name)
    # Upload the file
    blob = bucket.blob(gcs_file_path)
    blob.upload_from_filename(local_file_path)
    print(f"File {local_file_path} uploaded to {gcs_file_path}.")

# Upload the CSV to Google Cloud Storage
#local_file_path = "outputs/24h/24h_snow_accumulation_latest.json"
bucket_name = "your-life-in-data"
#gcs_file_path = "snowfall-accumulation/24h_snow_accumulation_latest.json"

# Upload the file to GCS
#upload_to_gcs(local_file_path, bucket_name, gcs_file_path)

# Define the timeframes
timeframes = ["24h", "48h", "72h", "season"]

# Define the local file paths and GCP file paths
file_paths = [f"outputs/{timeframe}/{timeframe}_snow_accumulation_latest.json" for timeframe in timeframes]
gcs_file_paths = [f"snowfall-accumulation/{timeframe}_snow_accumulation_latest.json" for timeframe in timeframes]

# Upload the files to GCP for each timeframe
for file_path, gcs_path in zip(file_paths, gcs_file_paths):
    upload_to_gcs(file_path, bucket_name, gcs_path)
    print(f"Uploaded {file_path} to Google Cloud Storage.")

# Upload last updated JSON to GCS
local_file_path = "outputs/last_updated.json"
bucket_name = "your-life-in-data"
gcs_file_path = "snowfall-accumulation/last_updated.json"
upload_to_gcs(local_file_path, bucket_name, gcs_file_path)
print(f"Uploaded {local_file_path} to Google Cloud Storage.")

# %%

