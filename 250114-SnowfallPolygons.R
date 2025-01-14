# Load required libraries
library(terra)
library(sf)
library(ggplot2)
library(tidyverse)

#SOURCE: https://www.nohrsc.noaa.gov/snowfall/

# Form path to URL: First, check the time and output 12 or 00
hour <- if (as.numeric(format(Sys.time(), "%H")) >= 12 && as.numeric(format(Sys.time(), "%H")) < 24) {
  "12"
} else {
  "00"
}

#Save path to most current raster
#6h raster <- "https://www.nohrsc.noaa.gov/snowfall/data/202501/sfav2_CONUS_6h_2025011418.tif"
path_to_raster <- paste0("https://www.nohrsc.noaa.gov/snowfall/data/202501/sfav2_CONUS_24h_", format(Sys.Date(), "%Y%m%d"), hour, ".tif")

# Load the raster
r <- rast(path_to_raster)

# Round raster values to 1 decimal place to capture those between 0-0.1, or else they'll all be rounded to 0 since as.polygons() outputs the nearest integer
r_rounded <- round(r, 1)*10

# Convert to polygons with rounded raster values
r_poly <- as.polygons(r_rounded)

# Convert raster polygons to sf
r_poly_sf <- st_as_sf(r_poly)

# Define the breaks and colors
breaks <- c(-0.01, 0, 0.1, 1, 2, 
            3, 4, 6, 8, 12, 
            18, 24, 30, 36, 48, 
            60, 72, 96, 120, 500)  # Added -0.01 and 500 to handle zero + >120 explicitly

#Multiply by 10 to match the rounding done above.
breaks = breaks*10

#used https://imagecolorpicker.com/ with the png file to set these 19 colors based on NOAA scale.
colors <- c("#ffffff", "#e4eef4", "#bdd7e7", "#6bafd6", "#2d83be", 
            "#02509d", "#022195", "#fefe96", "#ffc500", "#ff8800", 
            "#dc0c00", "#9f0000", "#690000", "#330000", "#cdcdff", 
            "#a08dd9", "#7d51a6", "#551573", "#290030")  

# Define the corresponding labels (numeric values)
labels <- c("0", "0.1", "1", "2", "3", 
            "4", "6", "8", "12", "18", 
            "24", "30", "36", "48", "60", 
            "72", "96", "120", ">120")

#Form what the column name should be. 
column_name <- paste0("sfav2_CONUS_24h_", format(Sys.Date(), "%Y%m%d"), hour)

# Use cut() to assign color categories based on the breaks
r_poly_sf$color_factor <- cut(r_poly_sf[[column_name]],
                              #r_poly_sf$sfav2_CONUS_24h_2025011412, 
                              breaks = breaks, 
                              labels = labels, 
                              include.lowest = FALSE, 
                              right = TRUE)  # Ensure right endpoint is included

# Now assign colors to the color_factor
r_poly_sf$colors <- colors[as.numeric(r_poly_sf$color_factor)]

#Spatial join all the 0.1 values between breaks together. This will reduce from hundreds of polygons to <=18 categories.
r_poly_sf2 <- r_poly_sf %>%
  group_by (color_factor, colors) %>%
  summarize (geometry = st_union (geometry))

# Plot using ggplot2
ggplot(r_poly_sf2) +
  geom_sf(aes(fill = color_factor), color = "black") +
  scale_fill_manual(values = setNames(colors, labels), 
                    breaks = labels,  # Make sure all labels are included in the legend
                    labels = labels) +  # Ensure the labels are displayed in the legend
  theme_minimal() +
  labs(fill = "Value Categories") +
  theme(legend.position = "bottom")

#Save as .gpkg (can change to GeoJSON or whatever form you need) 
#color_factor should match to the snowfall amount in the scale at https://www.nohrsc.noaa.gov/snowfall/
st_write (r_poly_sf2, paste0("outputs/", format(Sys.time(), "%y%m%d_%H%M"), "_24h_snow_accumulation.geojson"), append=FALSE)