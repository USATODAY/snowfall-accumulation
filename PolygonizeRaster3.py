from osgeo import gdal, ogr, osr
import requests
from datetime import datetime, timedelta
import geopandas as gpd
import numpy as np
import geopandas as gpd
import pandas as pd
import topojson
import json
import requests

def raster2vector(timeframe):
    # Get the current hour in UTC (12 after 1 p.m., otherwise 00)
    current_hour = datetime.utcnow().hour
    hour = "12" if current_hour >= 13 and current_hour < 24 else "00"
    
    # Generate the URL for the raster file
    current_date = datetime.utcnow()
    date_str = current_date.strftime("%Y%m%d")
    month_str = current_date.strftime("%Y%m")
    
    path_to_raster = f"https://www.nohrsc.noaa.gov/snowfall/data/{month_str}/sfav2_CONUS_{timeframe}{date_str}{hour}.tif"
    out_path = f'python/{timeframe}snowfall.shp'
    str_length = 30

    response = requests.head(path_to_raster)  # Send a HEAD request to check if the file exists

    if response.status_code == 404:
        # Create a path for the previous day if the file is not available
        #Season path=new_date = (datetime.strptime(season_end[:8], "%Y%m%d") - timedelta(days=1)).strftime("%Y%m%d")
        #new_path_to_raster = f"https://www.nohrsc.noaa.gov/snowfall/data/{datetime.now().strftime('%Y%m')}/sfav2_CONUS_{season_start}_to_{new_date}12.tif"
            # Create a path for the previous day if the file is not available
        if hour == "12":
            new_month = month_str
            new_date = (current_date - timedelta(days=1)).strftime("%Y%m%d")
            new_hour = "00"
        else:
            new_month = (current_date - timedelta(days=1)).strftime("%Y%m")
            new_date = (current_date - timedelta(days=1)).strftime("%Y%m%d")
            new_hour = "12"

        new_path_to_raster = f"https://www.nohrsc.noaa.gov/snowfall/data/{new_month}/sfav2_CONUS_{timeframe}{new_date}{new_hour}.tif"
        print(f"{path_to_raster[-str_length:]} is unavailable.")
        print(f"Now trying {new_path_to_raster[-str_length:]}")
    
        try:
            #r = gpd.read_file(new_path_to_raster)
            src_ds = gdal.Open(new_path_to_raster)
            print(f"Pulled {new_path_to_raster[-str_length:]}")
            #column_name = f"sfav2_CONUS_{timeframe}{new_date}{new_hour}.tif"
        except Exception as e:
            print(f"Error fetching data from {new_path_to_raster}: {e}")
    else:
        src_ds = gdal.Open(path_to_raster)
        #src_ds = gdal.Open(path_to_raster)
        print(f"Pulled {path_to_raster[-str_length:]}")
        #column_name = f"sfav2_CONUS_{timeframe}{date_str}{hour}.tif"

    #print(f"Using column: {column_name}")
    # Read the first band (assuming the data is in the first band)
    srcband = src_ds.GetRasterBand(1)
    
    # Read the data from the raster
    data = srcband.ReadAsArray()
    
    # Read the data from the raster
    #data[(data > 0) & (data < 1)] = 1  # Set values greater than 0 and <= 0.5 to 1 to preserve.

    # Add 1 to all other values
    #data[(data > 1)] += 1  # Add 1 to all values greater than 0.5. We'll need to subtract one later.
    
    # Add 1 to all other values above 0.5 WORKING CODE 2/1/25
    #data[(data > 0.5)] += 1  # Add 1 to all values greater than 0.5. We'll need to subtract one later.
    #data[(data > 0) & (data < 0.5)] = 1  # Set values greater than 0 and <= 0.5 to 1 to preserve.
    
    #Separate trace, <1 inch and all others
    data[(data > 1)] += 2  # Add 2 to all other values. We'll need to subtract two later.
    data[(data > 0.1) & (data < 1)] = 2  # Set less than 1 values to 2
    data[(data > 0) & (data <= 0.1)] = 1  # Set trace values to 1
    
    # Create a new raster to save the modified data
    driver = gdal.GetDriverByName("GTiff")
    out_ds = driver.Create('python/adjusted_raster.tif', src_ds.RasterXSize, src_ds.RasterYSize, 1, gdal.GDT_Float32)
    out_band = out_ds.GetRasterBand(1)
    out_band.WriteArray(data)
    
    # Set the geotransform and projection from the original raster
    out_ds.SetGeoTransform(src_ds.GetGeoTransform())
    out_ds.SetProjection(src_ds.GetProjection())
    
    # Now polygonize the new raster (with adjusted values)
    sp_ref = osr.SpatialReference()
    sp_ref.SetFromUserInput('EPSG:4326')
    
    drv = ogr.GetDriverByName("ESRI Shapefile")
    dst_ds = drv.CreateDataSource(out_path)
    dst_layername = 'snowfall'
    dst_layer = dst_ds.CreateLayer(dst_layername, srs=sp_ref)
    
    # Create a field for the output shapefile
    fld = ogr.FieldDefn("DN", ogr.OFTInteger)
    dst_layer.CreateField(fld)
    dst_field = dst_layer.GetLayerDefn().GetFieldIndex("DN")

    try: 
        # Polygonize the modified raster
        gdal.Polygonize(out_band, None, dst_layer, dst_field, [], callback=None) 
        print(f"Created {timeframe} shapefile successfully")
    except Exception as e:
        print(f"Error creating shapefile: {e}")
        dst_ds = None

    # Clean up
    del src_ds
    del dst_ds
    del out_ds

    #return dst_ds

# Iterate over the timeframes
timeframes = ["24h_", "48h_", "72h_"]

snow_list = [raster2vector(timeframe) for timeframe in timeframes]

# Now do seasonal map
# Get the current year
current_year = datetime.now().year

# Determine the start of the season (Oct 1 of the current or previous year)
if datetime.now().month < 10:
    season_start_year = current_year - 1  # If it's before October, use the previous year
else:
    season_start_year = current_year  # Otherwise, use the current year

# Define the start of the season (October 1st, 12 UTC)
season_start = f"{season_start_year}093012"

# Define the end of the season as the current date (with fixed hour '12')
season_end = f"{datetime.now().strftime('%Y%m%d')}12"

# Create the full URL for the seasonal raster
path_to_raster = f"https://www.nohrsc.noaa.gov/snowfall/data/{datetime.now().strftime('%Y%m')}/sfav2_CONUS_{season_start}_to_{season_end}.tif"

out_path = f'python/{timeframe}snowfall.shp'
str_length = 40

response = requests.head(path_to_raster)  # Send a HEAD request to check if the file exists

if response.status_code == 404:
    # Create a path for the previous day if the file is not available
    new_date = (datetime.strptime(season_end[:8], "%Y%m%d") - timedelta(days=1)).strftime("%Y%m%d")
    new_path_to_raster = f"https://www.nohrsc.noaa.gov/snowfall/data/{datetime.now().strftime('%Y%m')}/sfav2_CONUS_{season_start}_to_{new_date}12.tif"

    print(f"{path_to_raster[-39:]} is unavailable.")
    print(f"Now trying {new_path_to_raster[-39:]}")

    try:
        src_ds = gdal.Open(new_path_to_raster)
        print(f"Pulled {new_path_to_raster[-str_length:]}")
        #column_name = f"sfav2_CONUS_{timeframe}{new_date}{new_hour}.tif"
    except Exception as e:
        print(f"Error fetching data from {new_path_to_raster}: {e}")
else:
    src_ds = gdal.Open(path_to_raster)
    #src_ds = gdal.Open(path_to_raster)
    print(f"Pulled {path_to_raster[-str_length:]}")
    #column_name = f"sfav2_CONUS_{timeframe}{date_str}{hour}.tif"

#print(f"Using column: {column_name}")
# Read the first band (assuming the data is in the first band)
srcband = src_ds.GetRasterBand(1)

# Read the data from the raster
data = srcband.ReadAsArray()

# Read the data from the raster
#data[(data > 0) & (data < 1)] = 1  # Set values greater than 0 and <= 0.5 to 1 to preserve.

# Add 1 to all other values
#data[(data > 1)] += 1  # Add 1 to all values greater than 0.5. We'll need to subtract one later.

# Add 1 to all other values above 0.5 WORKING CODE 2/1/25
#data[(data > 0.5)] += 1  # Add 1 to all values greater than 0.5. We'll need to subtract one later.
#data[(data > 0) & (data < 0.5)] = 1  # Set values greater than 0 and <= 0.5 to 1 to preserve.

#Separate trace, <1 inch and all others
data[(data > 1)] += 2  # Add 2 to all other values. We'll need to subtract two later.
data[(data > 0.1) & (data < 1)] = 2  # Set less than 1 values to 2
data[(data > 0) & (data <= 0.1)] = 1  # Set trace values to 1

# Create a new raster to save the modified data
driver = gdal.GetDriverByName("GTiff")
out_ds = driver.Create('python/adjusted_raster.tif', src_ds.RasterXSize, src_ds.RasterYSize, 1, gdal.GDT_Float32)
out_band = out_ds.GetRasterBand(1)
out_band.WriteArray(data)

# Set the geotransform and projection from the original raster
out_ds.SetGeoTransform(src_ds.GetGeoTransform())
out_ds.SetProjection(src_ds.GetProjection())

# Now polygonize the new raster (with adjusted values)
sp_ref = osr.SpatialReference()
sp_ref.SetFromUserInput('EPSG:4326')

drv = ogr.GetDriverByName("ESRI Shapefile")
dst_ds = drv.CreateDataSource(out_path)
dst_layername = 'snowfall'
dst_layer = dst_ds.CreateLayer(dst_layername, srs=sp_ref)

# Create a field for the output shapefile
fld = ogr.FieldDefn("DN", ogr.OFTInteger)
dst_layer.CreateField(fld)
dst_field = dst_layer.GetLayerDefn().GetFieldIndex("DN")

try: 
    # Polygonize the modified raster
    gdal.Polygonize(out_band, None, dst_layer, dst_field, [], callback=None) 
    print(f"Created {timeframe} shapefile successfully")
except Exception as e:
    print(f"Error creating shapefile: {e}")
    dst_ds = None

# Clean up
del src_ds
del dst_ds
del out_ds

#return dst_ds