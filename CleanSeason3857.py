import geopandas as gpd
import numpy as np
from shapely.geometry import Polygon
from shapely.ops import unary_union
import time
import subprocess
import json

start_time1 = time.time()
start_time = time.time()

#USE METERS in 3857
def generate_hex_grid(bounds, hex_size):
    xmin, ymin, xmax, ymax = bounds  # Already in meters
    dx = hex_size * np.sqrt(3)  # Horizontal spacing (meters)
    dy = hex_size * 1.5  # Vertical spacing (meters)

    hexagons = []
    row = 0
    y = ymin
    while y < ymax + dy:
        col = 0
        x_offset = (row % 2) * (dx / 2)  # Offset odd rows
        x = xmin - x_offset
        while x < xmax + dx:
            hexagon = Polygon([
                (x, y),
                (x + hex_size * np.sqrt(3) / 2, y + hex_size * 0.5),
                (x + hex_size * np.sqrt(3) / 2, y + hex_size * 1.5),
                (x, y + hex_size * 2),
                (x - hex_size * np.sqrt(3) / 2, y + hex_size * 1.5),
                (x - hex_size * np.sqrt(3) / 2, y + hex_size * 0.5),
                (x, y)
            ])
            hexagons.append(hexagon)
            x += dx
            col += 1
        y += dy
        row += 1

    return gpd.GeoDataFrame(geometry=hexagons, crs="EPSG:3857")  # Already in meters

# Load your raster-derived polygon shapefile
#square_polygons = gpd.read_file("/Users/AFast/Documents/GitHub/snowfall-accumulation/python/season_snowfall.shp")
square_polygons = gpd.read_file("python/season_snowfall.shp")
    
# Rename the column 'DN' to 'accumulation'
square_polygons = square_polygons.rename(columns={"DN": "accumulation"})

# Filter rows where 'accumulation' is greater than 0
square_polygons = square_polygons[square_polygons['accumulation'] > 0]

# Reset those below at one to 0.75 to indicate <1; subtract extra inch from all others.
square_polygons['accumulation'] = square_polygons['accumulation'].apply(lambda x: 0.75 if x == 1 else x - 1)

# Convert to EPSG:3857 (Web Mercator) to match hex CRS
square_polygons = square_polygons.to_crs(epsg=3857)

# Generate hexagonal grid covering the extent
#hex_size = 0.075  # 3.3MB-99s
#hex_size = 0.06 # 9.4MB- 140s
hex_size = 0.065 # 9.4MB- 140s degrees

hex_size = 8000 #for 3857 meters
hex_grid = generate_hex_grid(square_polygons.total_bounds, hex_size)

# Ensure CRS
hex_grid.set_crs(epsg=3857, inplace=True)

# Spatial join to transfer attributes from square polygons to hexagons
hex_grid = hex_grid.sjoin(square_polygons, how="left", predicate="intersects")

# Remove NULLs
hex_grid = hex_grid.dropna(subset=["accumulation"])

# Save as shapefile
#hex_grid.to_file("/Users/AFast/Documents/GitHub/snowfall-accumulation/python/outputs/hex_grid.shp")

#keep highest DN value
hex_grid_sorted = hex_grid.sort_values(by="accumulation", ascending=False)

# Drop duplicate geometries, keeping the first occurrence
hex_grid_distinct = hex_grid_sorted.drop_duplicates(subset="geometry", keep="first")

# Optionally, reset the index after removing duplicates
hex_grid_distinct = hex_grid_distinct.reset_index(drop=True)
#hex_grid_distinct.to_file("/Users/AFast/Documents/GitHub/snowfall-accumulation/python/outputs/hex_grid_dist.shp")

end_time = time.time()
print(f"Hex grid execution time {end_time - start_time:.4f} seconds")

start_time = time.time()
# Simplify geometries -- bloats file size
#hex_grid1 = hex_grid_distinct.copy()  # Copy to keep other attributes
#hex_grid1["geometry"] = hex_grid1["geometry"].apply(lambda geom: geom.simplify(0.005, preserve_topology=True))
#hex_grid1.to_file("/Users/AFast/Documents/GitHub/snowfall-accumulation/python/outputs/season_simp.json", driver="GeoJSON")

# Group by 'accumulation' and union the geometries
hex_grid3 = (
    hex_grid_distinct
    .dissolve(by="accumulation")#, aggfunc="first")  # Keeps first non-geometry column value
    .reset_index()[["accumulation", "geometry"]])

# Ensure the geometries are fully merged
#hex_grid3["geometry"] = hex_grid2.groupby("accumulation")["geometry"].apply(lambda g: unary_union(g))
hex_grid3["geometry"] = hex_grid3["geometry"].apply(lambda g: unary_union(g) if not g.is_empty else g)

# Ensure geometries are valid
#hex_grid3["geometry"] = hex_grid3["geometry"].apply(lambda geom: geom.buffer(0))
hex_grid3["geometry"] = hex_grid3["geometry"].apply(lambda g: g.buffer(0) if not g.is_valid else g)

start_time = time.time()
# Export to a dictionary
geojson_data = json.loads(hex_grid3.to_json())

# Rename the feature collection to "snowfall"
geojson_data["name"] = "snowfall"

# Save to a file
#output_path = "/Users/AFast/Documents/GitHub/snowfall-accumulation/python/outputs/season_simp2.json"
output_path = "python/outputs/season_simp2.json"

#with open(output_path, "w") as f:
#    json.dump(geojson_data, f, indent=4) #prettyprint
with open(output_path, "w") as f:
    json.dump(geojson_data, f, separators=(",", ":"))

end_time = time.time()
print(f"Convert to geojson time {end_time - start_time:.4f} seconds")

start_time = time.time()

# Define paths
#mapshaper_path = "/Users/AFast/.nvm/versions/node/v22.11.0/bin/mapshaper"
#input_geojson = "/Users/AFast/Documents/GitHub/snowfall-accumulation/python/outputs/season_simp2.json"
#output_topojson = "/Users/AFast/Documents/GitHub/snowfall-accumulation/python/outputs/season_simp4.json"

#For GH Actions
mapshaper_path = "mapshaper"
input_geojson = "python/outputs/season_simp2.json"
output_topojson = "outputs/season/season_inches_snow_accumulation_latest.json"

# Convert to TopoJSON with a named layer
subprocess.run([
    mapshaper_path, 
    input_geojson, 
    "-rename-layers", "snowfall",  # Set layer name to "snowfall"
    "-o", output_topojson, 
    "format=topojson",
    "force"#,  # Ensure output overwrites existing file
    #"preserve-properties=true"  # Retain feature properties
], check=True)
end_time = time.time()
print(f"Convert to topojson time {end_time - start_time:.4f} seconds")

final_time = time.time()
print(f"Full processing time {final_time - start_time1:.4f} seconds")