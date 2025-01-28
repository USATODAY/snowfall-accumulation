library(googleCloudStorageR)
library(stringr)

# Authenticate with Google Cloud
# Ensure you have set up authentication JSON key beforehand
#Sys.setenv(GCS_AUTH_FILE = "/Users/AFast/Downloads/key2.json")
#gcs_auth("/Users/AFast/Downloads/storage-key.json")
#Pull credential json from GitHub secrets
gcs_auth(Sys.getenv("GCS_AUTH_FILE"))

# Function to upload file to Google Cloud Storage
upload_to_gcs <- function(local_file_path, bucket_name, gcs_file_path) {
  # Set the GCS bucket
  gcs_global_bucket(bucket_name)
  
  # Upload the file
  gcs_upload(local_file_path, name = gcs_file_path)
  
  message(sprintf("File '%s' uploaded to '%s'.", local_file_path, gcs_file_path))
}

# File paths and bucket information
#local_file_path <- "outputs/24h/24h_snow_accumulation_latest.json"
bucket_name <- "your-life-in-data"
#gcs_file_path <- "snowfall-accumulation/24h_snow_accumulation_latest.json"
#upload_to_gcs(local_file_path, bucket_name, gcs_file_path)

timeframes <- c("24h_", "48h_", "72h_", "season_")

file_paths <- paste0("outputs/", str_remove(timeframes, "_"), "/", timeframes, "snow_accumulation_latest.json")
gcs_file_paths <- paste0("snowfall-accumulation/", timeframes, "snow_accumulation_latest.json")

gcs_file_path <- "snowfall-accumulation/24h_snow_accumulation_latest.json"

# Upload the file to GCS

lapply(1:length(timeframes), function(x){ upload_to_gcs(file_paths[x], bucket_name, gcs_file_paths[x])})

message("Scraping complete. New data saved locally and uploaded to Google Cloud Storage.")
