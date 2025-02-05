import geopandas as gpd
from shapely.ops import unary_union
from shapely.geometry import Polygon
import multiprocessing
import time
import topojson
import geojson
import json

def process_feature(index, smoothed_polygons6):
    this_feature = smoothed_polygons6.iloc[index]
    intersections = smoothed_polygons6.intersects(this_feature.geometry)
    smoothed_polygons6a = smoothed_polygons6[intersections & (smoothed_polygons6['id'] > this_feature['id'])]

    if not smoothed_polygons6a.empty:
        smoothed_polygons6a_union = unary_union(smoothed_polygons6a.geometry)
        this_feature_clipped = this_feature.geometry.difference(smoothed_polygons6a_union)

        # Ensure geometry is a Polygon
        if this_feature_clipped.geom_type == 'GeometryCollection':
            polygons = [geom for geom in this_feature_clipped.geoms if isinstance(geom, Polygon)]
            this_feature_clipped = polygons if polygons else this_feature.geometry
    else:
        this_feature_clipped = this_feature.geometry

    #print(f"Processed feature {index + 1}/{len(smoothed_polygons6)}", end="\r", flush=True)
    return this_feature_clipped

def dissolve_polygons(smoothed_polygons8):
    # Group by 'accumulation' and perform union operation, without setting 'accumulation' as index
    smoothed_polygons9 = smoothed_polygons8.dissolve(by="accumulation", aggfunc="first", as_index=False)
    
    # Ensure valid geometries
    smoothed_polygons9['geometry'] = smoothed_polygons9['geometry'].apply(lambda geom: geom if geom.is_valid else geom.buffer(0))
    
    # Simplify geometries very slightly
    smoothed_polygons9['geometry'] = smoothed_polygons9['geometry'].simplify(tolerance=0.005)
    
    # Select only the 'accumulation' and 'geometry' columns
    smoothed_polygons9 = smoothed_polygons9[['accumulation', 'geometry']]
    
    return smoothed_polygons9

def save_topojson(geo_df, timeframe):
    output_dir = "outputs/"
    file3 = f"outputs/{timeframe}/{timeframe}_inches_snow_accumulation_latest.json"
    
    # Convert the GeoDataFrame to TopoJSON
    topojson_data = topojson.Topology(geo_df)

    # Convert TopoJSON to dictionary
    topojson_dict = topojson_data.to_dict()
    
    # Rename the default layer ("data") to "snowfall"
    if "objects" in topojson_dict and "data" in topojson_dict["objects"]:
        topojson_dict["objects"]["snowfall"] = topojson_dict["objects"].pop("data")
    
    # Save the TopoJSON data to a file
    with open(file3, 'w') as f:
        f.write(json.dumps(topojson_dict))  # Write modified dictionary to file
    
# Iterate over the timeframes
timeframes = ["24h", "48h", "72h"]

# Save the processed files (if you need to)
for timeframe in timeframes:
    start_time = time.time()
    file = f"outputs/{timeframe}/{timeframe}_inches_snow_accumulation_latest.json"
    # Load the GeoJSON file
    smoothed_polygons6 = gpd.read_file(file)

    # Add id column 
    smoothed_polygons6["id"] = range(1, len(smoothed_polygons6) + 1)

	# Fix invalid geometries
    #smoothed_polygons6["geometry"] = smoothed_polygons6["geometry"].buffer(0)

    # Remove overlaps
    smoothed_polygons6["geometry"] = smoothed_polygons6.apply(lambda row: process_feature(row.name, smoothed_polygons6), axis=1)

    # Save file    
    #smoothed_polygons6.to_file(file2, driver="GeoJSON")

    #Set CRS
    smoothed_polygons7 = smoothed_polygons6.set_crs(4326, allow_override=True)
    
    # Dissolve polygons with same accumulation
    smoothed_polygons8 = dissolve_polygons(smoothed_polygons7)
    
    # Save as TopoJSON
    #timeframe = "24h"  # Replace with your timeframe
    save_topojson(smoothed_polygons8, timeframe)
    
    end_time =time.time()
    print(f"Execution time {end_time - start_time:.4f} seconds")