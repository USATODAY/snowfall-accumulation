# Load required libraries
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(tesseract)
library(lubridate)

#SOURCE: https://www.nohrsc.noaa.gov/snowfall/

#Set time zone to pull files using Eastern time so GitHub Actions doesn't use UTC.
Sys.setenv(TZ="America/New_York")

# Form path to URL: First, check the time and output 12 or 00
hour <- if (as.numeric(format(Sys.time(), "%H")) >= 12 && as.numeric(format(Sys.time(), "%H")) < 24) {
  "12"
} else {
  "00"
}

path_to_image <- paste0("https://www.nohrsc.noaa.gov/snowfall/data/", format(Sys.Date(), "%Y%m"), "/sfav2_CONUS_24h_", format(Sys.Date(), "%Y%m%d"), hour, ".png")

download.file(path_to_image, paste0("/Users/AFast/Downloads/sfav2_CONUS_24h_", format(Sys.Date(), "%Y%m%d"), hour, "_", format(Sys.time(), "%H%M"), ".png"))
