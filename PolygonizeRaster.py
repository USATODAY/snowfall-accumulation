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

    '''ORIG CODE
    # Try to load the raster (if fails, load the previous day's version)
    try:
        #r = gpd.read_file(path_to_raster)  # Assuming the raster is in a format geopandas can read
        src_ds = gdal.Open(path_to_raster)
        #srcband = src_ds.GetRasterBand(1)
        
    except Exception as e:
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
        print(f"{path_to_raster[-29:]} is unavailable.")
        print(f"Now trying {new_path_to_raster[-29:]}")
        
        try:
            src_ds = gdal.Open(new_path_to_raster)
            print(f"Pulled {new_path_to_raster[-29:]}")
            #column_name = f"sfav2_CONUS_{timeframe}{new_date}{new_hour}.tif"
        except Exception as e:
            print(f"Error fetching data from {new_path_to_raster}: {e}")
            return None
    else:
        print(f"Pulled {path_to_raster[-29:]}")
        #column_name = f"sfav2_CONUS_{timeframe}{date_str}{hour}.tif"
    '''   
    #print(f"Using column: {column_name}")
    # Read the first band (assuming the data is in the first band)
    srcband = src_ds.GetRasterBand(1)
    
    # Read the data from the raster
    data = srcband.ReadAsArray()
    
    # Read the data from the raster
    #data[(data > 0) & (data < 1)] = 1  # Set values greater than 0 and <= 0.5 to 1 to preserve.

    # Add 1 to all other values
    #data[(data > 1)] += 1  # Add 1 to all values greater than 0.5. We'll need to subtract one later.
    
    # Add 1 to all other values above 0.5
    data[(data > 0.5)] += 1  # Add 1 to all values greater than 0.5. We'll need to subtract one later.
    
    data[(data > 0) & (data < 0.5)] = 1  # Set values greater than 0 and <= 0.5 to 1 to preserve.
    
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

#Clean up files
def clean_vectors(timeframe):    
    # Read the shapefile
    shapefile = gpd.read_file(f'python/{timeframe}snowfall.shp')
    
    # Make sure the geometries are valid
    shapefile = shapefile[shapefile.is_valid]
    
    # Rename the column 'DN' to 'accumulation'
    shapefile = shapefile.rename(columns={"DN": "accumulation"})
    
    # Filter rows where 'accumulation' is greater than 0
    shapefile = shapefile[shapefile['accumulation'] > 0]
    
    # Reset those below 1 to 0.5 (less than 1"); subtract extra inch from all others.
    shapefile['accumulation'] = shapefile['accumulation'].apply(lambda x: 0.5 if x == 1 else x - 1)
    
    # Convert to GeoJSON (TopoJSON) format and save to a file
    # shapefile.to_file(f'outputs/test/{timeframe}accumulation.json", driver="GeoJSON")
    #topojson.Topology(shapefile).to_file(f'outputs/test/{timeframe}accumulation.json')
    # Convert to TopoJSON format
    topo = topojson.Topology(shapefile)

    # Convert the topology object to a dictionary
    topo_dict = topo.to_dict()
    # Rename to pass JS test
    topo_dict["objects"]["snowfall"] = topo_dict["objects"].pop("data")

    # Write the TopoJSON to a file
    with open(f'python/outputs/{timeframe}inches_snow_accumulation_latest.json', 'w') as f:
        json.dump(topo_dict, f)

    print (f'Cleaned {timeframe}')

snow_list2 = [clean_vectors(timeframe) for timeframe in timeframes]