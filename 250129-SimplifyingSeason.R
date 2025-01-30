# Load required libraries
library(terra)
library(sf)
library(dplyr)
library(stringr)
library(tesseract)
library(lubridate)
#library(tidyr) #Can remove this with drop_na gone?
library(jsonlite)
library(tibble)

#SOURCE: https://www.nohrsc.noaa.gov/snowfall/

#Set time zone to pull files using Eastern time so GitHub Actions doesn't use UTC.
#Sys.setenv(TZ="America/New_York")
Sys.setenv(TZ="UTC")

# Form path to URL: First, check the time and output 12 or 00 -- use 12 after 1 p.m. UTC. Otherwise, use 00.
hour <- if (as.numeric(format(Sys.time(), "%H")) >= 13 && as.numeric(format(Sys.time(), "%H")) < 24) {
  "12"
} else {
  "00"
}

timeframes <- c("24h_", "48h_", "72h_")

# we'll make a function to get and process the latest raster 
raster2vector <- function(timeframe){
  
  #timeframe <- "72h_"
  # make a url
  path_to_raster <- paste0("https://www.nohrsc.noaa.gov/snowfall/data/", format(Sys.Date(), "%Y%m"), "/sfav2_CONUS_", timeframe, format(Sys.Date(), "%Y%m%d"), hour, ".tif")
  
  # load the raster
  #r <- rast(path_to_raster)
  r <- try(rast(path_to_raster), silent = TRUE)
  
  # Check if path_to_raster hits 404 error because it hasn't been created yet.
  if (inherits(r, "try-error")) {
    #Create path to one version behind (if tried 12 UTC for 1/29/25, create path for 00 UTC 1/29/25, etc.)
    new_path_to_raster <- paste0("https://www.nohrsc.noaa.gov/snowfall/data/", 
                                 if_else (hour==12, format(Sys.Date(), "%Y%m"), format(Sys.Date()-days(1), "%Y%m")), 
                                 "/sfav2_CONUS_", timeframe, 
                                 if_else (hour==12, format(Sys.Date(), "%Y%m%d"), format(Sys.Date()-days(1), "%Y%m%d")), 
                                 if_else (hour==12, "00", "12"), ".tif")
    print(paste(substr(path_to_raster, nchar(path_to_raster) - 29, nchar(path_to_raster)), "is unavailable."))
    print(paste("Now trying", substr(new_path_to_raster, nchar(new_path_to_raster) - 29, nchar(new_path_to_raster))))
    r <- rast(new_path_to_raster)
    print(paste("Pulled", substr(new_path_to_raster, nchar(new_path_to_raster) - 29, nchar(new_path_to_raster))))
  } else {
    print(paste("Pulled", substr(path_to_raster, nchar(path_to_raster) - 29, nchar(path_to_raster))))
  }
  
  # round raster values to 1 decimal place to capture those between 0-0.1, or else they'll all be rounded to 0 since as.polygons() outputs the nearest integer
  r_rounded <- round(r, 1)*10
  
  # convert to polygons with rounded raster values
  r_poly <- as.polygons(r_rounded)
  
  # convert raster polygons to sf
  r_poly_sf <- st_as_sf(r_poly)
  
  # define the breaks and colors
  breaks <- c(-0.01, 0, 0.1, 1, 2, 
              3, 4, 6, 8, 12, 
              18, 24, 30, 36, 48, 
              60, 72, 96, 120, 500)  # added -0.01 and 500 to handle zero + >120 explicitly
  
  # multiply by 10 to match the rounding done above.
  breaks = breaks*10
  
  # used https://imagecolorpicker.com/ with the png file to set these 19 colors based on NOAA scale.
  colors <- c("#ffffff", "#e4eef4", "#bdd7e7", "#6bafd6", "#2d83be", 
              "#02509d", "#022195", "#fefe96", "#ffc500", "#ff8800", 
              "#dc0c00", "#9f0000", "#690000", "#330000", "#cdcdff", 
              "#a08dd9", "#7d51a6", "#551573", "#290030")  
  
  # define the corresponding labels (numeric values)
  labels <- c("0", "0.1", "1", "2", "3", 
              "4", "6", "8", "12", "18", 
              "24", "30", "36", "48", "60", 
              "72", "96", "120", ">120")
  
  # form what the column name should be 
  column_name <- paste0("sfav2_CONUS_", timeframe, format(Sys.Date(), "%Y%m%d"), hour)
  
  # assign color categories based on the breaks
  r_poly_sf$color_factor <- cut(r_poly_sf[[column_name]],
                                breaks = breaks, 
                                labels = labels, 
                                include.lowest = FALSE, 
                                right = TRUE)  # ensure right endpoint is included
  
  # now assign colors to the color_factor
  r_poly_sf$colors <- colors[as.numeric(r_poly_sf$color_factor)]
  
  # spatial join all the 0.1 values between breaks together. This will reduce from hundreds of polygons to <=18 categories.
  r_poly_sf2 <- r_poly_sf %>%
    #group_by (color_factor, colors) %>%
    #summarize (geometry = st_union (geometry)) %>%
    #drop_na() %>%
    mutate (accumulation = r_poly_sf[[column_name]]/10) %>%
    filter (accumulation > 0) %>%
    select (accumulation)
#    select (accumulation, color_factor) %>%
#    filter (color_factor != 0) 
    #ungroup() %>%
    #rename (hue = colors) %>%
    #select (color_factor)
  
  return(r_poly_sf2)
}

# iterate the function for 24hr, 48hr and 72hr accumulations
snow_list <- lapply(timeframes, raster2vector)

# also get season accumulation
# it's a slightly different format so getting that separately
raster2vector_season <- function() {
  
  ## construct the URL dynamically
  # first get current year
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # determine the start of the season (oct 1 of the current or previous year)
  season_start_year <- if (format(Sys.Date(), "%m") < "10") {
    current_year - 1  # ff it's before oct, use the previous year
  } else {
    current_year  # otherwise, use the current year
  }
  
  season_start <- paste0(season_start_year, "093012")  # october 1, 12 UTC
  
  # define the end of the season as the current date
  season_end <- paste0(format(Sys.Date(), "%Y%m%d"), "12") # hour fixed, no "00" files
  
  # connect all this for seasonal URL
  path_to_raster <- paste0(
    "https://www.nohrsc.noaa.gov/snowfall/data/",
    format(Sys.Date(), "%Y%m"), 
    "/sfav2_CONUS_", season_start, "_to_", season_end, ".tif")
  
  # load the raster
  #r <- rast(path_to_raster)
  r <- try(rast(path_to_raster), silent = TRUE)
  
  # Check if path_to_raster hits 404 error because it hasn't been created yet.
  if (inherits(r, "try-error")) {
    #Create path to one day behind the version just tried (1/28/25 instead of 1/29/25, etc.)
    new_path_to_raster <- paste0(
      "https://www.nohrsc.noaa.gov/snowfall/data/",
      format(Sys.Date()-days(1), "%Y%m"), 
      "/sfav2_CONUS_", season_start, "_to_", 
      format(ymd(substr(season_end, 1, 8))-days(1), "%Y%m%d"), "12", ".tif")
    print(paste(substr(path_to_raster, nchar(path_to_raster) - 39, nchar(path_to_raster)), "is unavailable."))
    print(paste("Now trying", substr(new_path_to_raster, nchar(new_path_to_raster) - 39, nchar(new_path_to_raster))))
    r <- rast(new_path_to_raster)
    print(paste("Pulled", substr(new_path_to_raster, nchar(new_path_to_raster) - 39, nchar(new_path_to_raster))))
    column_name <- paste0("sfav2_CONUS_", season_start, "_to_", format(ymd(substr(season_end, 1, 8))-days(1), "%Y%m%d"), "12")
  } else {
    print(paste("Pulled", substr(path_to_raster, nchar(path_to_raster) - 39, nchar(path_to_raster))))
    column_name <- paste0("sfav2_CONUS_", season_start, "_to_", season_end)
  }
  
  # round raster values to 1 decimal place to capture those between 0-0.1, or else they'll all be rounded to 0 since as.polygons() outputs the nearest integer
  r_rounded <- round(r, 1)*10
  
  # convert to polygons with rounded raster values
  r_poly <- as.polygons(r_rounded)
  
  #TRYING NEW CODE HERE
  library(stars)

  # Read raster with {stars}
  r2 <- read_stars(new_path_to_raster)
  # Convert raster to polygons
  r_poly2 <- st_as_sf(r2, merge = TRUE)  # Merge adjacent areas of same value
  r_poly3 <- r_poly2 %>%
    mutate (accumulation = case_when(
      sfav2_CONUS_2024093012_to_2025012912.tif < 0 ~ -1,
      sfav2_CONUS_2024093012_to_2025012912.tif == 0 ~ 0,
      between(sfav2_CONUS_2024093012_to_2025012912.tif, 0, 1) ~ 1,
      TRUE ~ round(sfav2_CONUS_2024093012_to_2025012912.tif)))
  r_poly4 <- r_poly3 %>%
    group_by (accumulation) %>%
    summarize (geometry=st_union(geometry)) %>%
    filter (accumulation > 0)
  check3 <- r_poly4 %>% st_drop_geometry()
  
  r_poly5 <- r_poly4%>%
    st_make_valid() %>%
    st_simplify(dTolerance = 10)
  #stars data with 1" breaks=14.2MB
  geojson_data <- geojsonio::geojson_json(r_poly5)
  
  #check <- r_poly3 %>% st_drop_geometry() %>% count (group)
  #multipolygons = 17.1MB
  geojson_data <- geojsonio::geojson_json(r_poly4)
  #polygons = 102.7MB
  geojson_data <- geojsonio::geojson_json(r_poly3)

  #don't mult by 10 for stars
  breaks3 <- c(-0.01, 0, seq(1,12), seq(18, 60, by=6), seq(72, 240, by=12), seq(300, max(r_poly_sf[[column_name]])+60, by=60))
  labels3 <- breaks3[2:length(breaks3)]

  breaks4 <- c(-0.01, 0, 1, 2, 4, 6, 8, seq(12, 60, by=6), seq(72, 180, by=12), seq(240, max(r_poly3[[paste0(column_name, ".tif")]])+60, by=60))
  labels4 <- breaks4[2:length(breaks4)]
  
  breaks5 <- c(-0.01, 0, 0.1, 1, 2, 3, 6, 12, 18,
               seq(24, 120, by=12), 
               seq(180, max(r_poly3[[paste0(column_name, ".tif")]])+60, by=60), 
               1000)
  labels5 <- breaks5[2:length(breaks5)]
  
  r_poly3$color_factor <- cut(r_poly3[[paste0(column_name, ".tif")]],
                                breaks = breaks5,
                                labels = labels5,
                                include.lowest = FALSE, 
                                right = TRUE)  # ensure right endpoint is included
  
  check <- r_poly3 %>% st_drop_geometry() %>% count (color_factor)
  
system.time(#153s
  r_poly4 <- r_poly3 %>%
    group_by (color_factor) %>%
    summarize (geometry = st_union (geometry)) %>%
    #filter (color_factor != 0) %>%
    ungroup() %>%
    mutate (accumulation = as.numeric (as.character(color_factor))) %>%
    select (accumulation) %>% 
    filter (accumulation > 0) #remove 0 and NAs
)
  check2b <- r_poly4 %>% st_drop_geometry()
  
  r_poly5 <- r_poly4 %>%
    st_make_valid() %>%
    st_simplify(dTolerance = 10)
  
  geojson_data <- geojsonio::geojson_json(r_poly5)
  geojsonio::topojson_write(geojson_data, 
                            file = paste0("outputs/test/season_inches_snow_accumulation_latest_newest_stars-breaks5.json"),
                            object_name = "snowfall",
                            #precision = 0,
                            #quantization = 1e3,
                            overwrite = TRUE)
  
  #This also works to polygonize
  # convert to polygons with rounded raster values
  r_poly <- as.polygons(r,
                        round=TRUE,
                        digits=0,
                        aggregate=TRUE) #false produces 1275000 rows
  
  
  # convert raster polygons to sf
  r_poly_sf <- st_as_sf(r_poly) %>%
    filter (sfav2_CONUS_2024093012_to_2025012912 > 0)
  
  
  #shp_projected <- st_transform(r_poly_sf, 3857)  # Web Mercator (meters)
  shp_projected <- st_transform(r_poly_sf, 5070) #Albers
  
  shp_simplified <- st_simplify(shp_projected, dTolerance = 1000)  # 100 meters
  shp_final <- st_transform(shp_simplified, st_crs(r_poly_sf))
  geojson_data <- geojsonio::geojson_json(shp_final)
  #14.2MB
  geojsonio::topojson_write(geojson_data, 
                            file = paste0("outputs/test/season6.json"),
                            object_name = "snowfall",
                            overwrite = TRUE) 
  
  shp_mapbox <- st_transform(shp_simplified, 3857)
  #Save as GeoJSON (best for web apps like Mapbox)
  geojson_data <- geojsonio::geojson_json(shp_mapbox) #32mb
  
  r_poly_sf1a <- st_buffer(r_poly_sf, 0)  # Fixes topology and removes tiny gaps
  geojson_data <- geojsonio::geojson_json(r_poly_sf1a) #18.6
  geojson_data <- geojsonio::geojson_json(shp_projected) #31.2
  
  r_poly_sf2a <- st_cast(r_poly_sf, "POLYGON")  
  geojson_data <- geojsonio::geojson_json(r_poly_sf2a) #??
  
  dissolved_ind <- st_cast(r_poly_sf, "POLYGON", group_or_split = TRUE)
  geojson_data <- geojsonio::geojson_json(dissolved_ind) #??
  
  geojsonio::topojson_write(geojson_data, 
                            file = paste0("outputs/test/season8.json"),
                            object_name = "snowfall",
                            overwrite = TRUE) 
  
  dissolved_ind <- rmapshaper::ms_explode(r_poly_sf) %>%
    st_make_valid()
  geojson_data <- geojsonio::geojson_json(dissolved_ind)
  
  
  #geom_types <- st_geometry_type(r_poly_sf)
  
  # Count the occurrences of each geometry type
  #table(geom_types)
  sf_polygons <- st_cast(r_poly_sf, "POLYGON")
  
  # Apply st_cast to each feature if it's a MULTIPOLYGON
  sf_polygons <- r_poly_sf %>%
    st_cast("POLYGON") %>%
    st_sf()  # Recreate the sf object with updated geometry
  sf_polygons <- st_collection_extract(r_poly_sf, "POLYGON")
  sf_object_split <- st_collection_extract(sf_object, "POLYGON")
  sf_object_split <- st_cast(r_poly_sf, "POLYGON")
  
  #Perform the multipart to single-part conversion
  dissolved_mp <- st_cast(r_poly_sf, "MULTIPOLYGON")
  dissolved_ind <- rmapshaper::ms_explode(dissolved_mp) 
  dissolved_ind <- st_make_valid(dissolved_ind)
  st_write (dissolved_ind, "test.gpkg", append=FALSE)
  st_write (r_poly_sf, "test2.gpkg", append=FALSE)
  st_write (shp_dissolved, "test3.gpkg", append=FALSE)
  
  
  sf_simplified <- rmapshaper::ms_simplify(r_poly_sf, keep = 0.1, method = "dp", keep_shapes = TRUE)
  # Save the output with 0.1 precision
  output_file <- "output.json"
  #15.6MB
  st_write(sf_simplified, output_file, driver = "GeoJSON", precision = 0.1)
  
  geojson_data <- geojsonio::geojson_json(r_poly_sf)
  #14.2MB
  geojsonio::topojson_write(geojson_data, 
                            file = paste0("outputs/test/season5.json"),
                            object_name = "snowfall",
                            overwrite = TRUE)  
  
  geojson_data <- geojsonio::geojson_json(dissolved_ind)
  #14.2MB
  geojsonio::topojson_write(geojson_data, 
                            file = paste0("outputs/test/season4.json"),
                            object_name = "snowfall",
                            overwrite = TRUE)
  
  st_write (dissolved_ind, "test.gpkg", append=FALSE)
  
    
  r_poly_sf2 <- r_poly_sf %>%
    st_make_valid() %>%
    st_simplify(dTolerance = 10)

  # define the breaks and colors, different from 24/48/72h scale
  # NOAA's scale
  breaks <- c(-0.01, 0, 0.1, 1, 2, 6, 
              # NOAA's scale has them in ft, multiplying by 12:
              1*12, 2*12, 3*12, 4*12, 6*12, 8*12, 10*12, 15*12, 20*12, 30*12, 40*12, 50*12,
              1000)  # added -0.01 and 1000 to handle zero + >50ft explicitly
  
  # multiply by 10 to match the rounding done above.
  breaks = breaks*10
  
  # used https://imagecolorpicker.com/ with the png file to set these 19 colors based on NOAA scale.
  colors <- c("#FFFFFF", "#BDD7E7", "#6AAFD6", "#2E83BF", "#024F9B",
              "#022195", "#FEFE96", "#FEC501", "#FD8900", "#DB0C00",
              "#9E0101", "#690100", "#CCCCFE", "#9F8CD8", "#7D51A7",
              "#541471", "#2A0030", "#3DE0FF")
  
  # define the corresponding labels (numeric values)
  labels <- c("0 in", "0.1 in", "1 in", "2 in", "6 in", 
              "1 ft", "2 ft", "3 ft", "4 ft", "6 ft",
              "8 ft", "10 ft", "15 ft", "20 ft", "30 ft",
              "40 ft", "50 ft", "> 50 ft")
  
  # USAT scale
  breaks2 <- c(-0.01, 0, 0.1, 1, 2, 3, 6,
               1*12, 2*12, 3*12, 10*12, 20*12,
               1000)
  breaks2 = breaks2*10
  # used https://imagecolorpicker.com/ with the png file to set these 19 colors based on NOAA scale.
  colors2 <- c("#FFFFFF", "#BDD7E7", "#6AAFD6", "#2E83BF", "#024F9B",
               "#022195", "#FEFE96", "#FEC501", "#FD8900", "#DB0C00",
               "#9E0101", "#690100")
  labels2 <- c("0 in", "0.1 in", "1 in", "2 in", "3 in", "6 in", 
               "1 ft", "2 ft", "3 ft", "10 ft", "20 ft", 
               "> 30 ft")
  
  # form what the column name should be 
  column_name <- paste0("sfav2_CONUS_", season_start, "_to_", season_end)
  
  accumulation = FALSE
  if (accumulation==TRUE){ 
    #RESTART CODE
    r_poly_sf <- st_as_sf(r_poly)
    #Breaks at every inch up to a foot, then quarter foot up to 5 feet, then every foot up to max value
    #breaks3 <- c(-0.01, 0, seq(1,12), seq(15, 60, by=3), seq(72, max(r_poly_sf[[column_name]])+12, by=12))
    #Breaks at every inch up to a foot, then half foot up to 5 feet, then every foot up to max value
    #breaks3 <- c(-0.01, 0, seq(1,12), seq(15, 60, by=6), seq(72, max(r_poly_sf[[column_name]])+12, by=12))
    #Breaks at every inch up to a foot, then half foot up to 5 feet, then every foot up to 20, then by 5ft up to max value
    breaks3 <- c(-0.01, 0, seq(1,12), seq(15, 60, by=6), seq(72, 240, by=12), seq(300, max(r_poly_sf[[column_name]])+60, by=60))
    breaks3 = breaks3*10
    
    labels3 <- paste(breaks3[2:length(breaks3)]/10, "in")
    labels3 <- breaks3[2:length(breaks3)]/10
    
    #without rounding
    breaks3 <- c(-0.01, 0, seq(1,12), seq(18, 60, by=6), seq(72, 240, by=12), seq(300, max(r_poly_sf[[column_name]])+60, by=60))
    labels3 <- breaks3[2:length(breaks3)]
    
    # used https://imagecolorpicker.com/ with the png file to set these 19 colors based on NOAA scale.
    #colors2 <- c("#FFFFFF", "#BDD7E7", "#6AAFD6", "#2E83BF", "#024F9B",
    #             "#022195", "#FEFE96", "#FEC501", "#FD8900", "#DB0C00",
    #             "#9E0101", "#690100")
    #labels2 <- c("0 in", "0.1 in", "1 in", "2 in", "3 in", "6 in", 
    #             "1 ft", "2 ft", "3 ft", "10 ft", "20 ft", 
    #             "> 30 ft")
    # assign color categories based on the breaks we've defined for USAT
    r_poly_sf$color_factor <- cut(r_poly_sf[[column_name]],
                                  breaks = breaks3,
                                  labels = labels3,
                                  include.lowest = FALSE, 
                                  right = TRUE)  # ensure right endpoint is included
    
    check1a <- r_poly_sf %>% st_drop_geometry()# %>% count (color_factor)
    
    r_poly_sf2 <- r_poly_sf %>%
      group_by (color_factor) %>%
      summarize (geometry = st_union (geometry)) %>%
      #filter (color_factor != 0) %>%
      ungroup() %>%
      mutate (accumulation = as.numeric (as.character(color_factor))) %>%
      select (accumulation) %>%
      filter (accumulation > 0)
    check2a <- r_poly_sf2 %>% st_drop_geometry() 
  }
  accumulation2 = FALSE #break at every inch
  if (accumulation2==TRUE){
    #RESTART CODE
    r_poly_sf <- st_as_sf(r_poly)
    breaks3 <- c(-0.01, 0, seq(1, max(r_poly_sf[[column_name]])+1))
    breaks3 = breaks3*10
    
    labels3 <- paste(breaks3[2:length(breaks3)]/10, "in")
    labels3 <- breaks3[2:length(breaks3)]/10
    # used https://imagecolorpicker.com/ with the png file to set these 19 colors based on NOAA scale.
    #colors2 <- c("#FFFFFF", "#BDD7E7", "#6AAFD6", "#2E83BF", "#024F9B",
    #             "#022195", "#FEFE96", "#FEC501", "#FD8900", "#DB0C00",
    #             "#9E0101", "#690100")
    #labels2 <- c("0 in", "0.1 in", "1 in", "2 in", "3 in", "6 in", 
    #             "1 ft", "2 ft", "3 ft", "10 ft", "20 ft", 
    #             "> 30 ft")
    # assign color categories based on the breaks we've defined for USAT
    r_poly_sf$color_factor <- cut(r_poly_sf[[column_name]],
                                  breaks = breaks3,
                                  labels = labels3,
                                  include.lowest = FALSE, 
                                  right = TRUE)  # ensure right endpoint is included
    
    #check <- r_poly_sf %>% st_drop_geometry()
    
    r_poly_sf2 <- r_poly_sf %>%
      group_by (color_factor) %>%
      summarize (geometry = st_union (geometry)) %>%
      filter (color_factor != 0) %>%
      ungroup() %>%
      mutate (accumulation = as.numeric (color_factor)) %>%
      select (accumulation)
  }
  
  
  # assign color categories based on the breaks we've defined for USAT
  r_poly_sf$color_factor <- cut(r_poly_sf[[column_name]],
                                breaks = breaks2, 
                                labels = labels2, 
                                include.lowest = FALSE, 
                                right = TRUE)  # ensure right endpoint is included
  
  # now assign colors to the color_factor
  r_poly_sf$colors <- colors[as.numeric(r_poly_sf$color_factor)]
  
  # spatial join all the 0.1 values between breaks together. This will reduce from hundreds of polygons to <=18 categories.
  r_poly_sf2 <- r_poly_sf %>%
    group_by (color_factor, colors) %>%
    summarize (geometry = st_union (geometry)) %>%
    #drop_na() %>% # the 48h and 72h file had some NA outline showing as black, removing those
    filter (color_factor != "0 in") %>%
    ungroup() %>%
    select (color_factor)
  
  #check <- r_poly_sf2 %>% st_drop_geometry()
  
  # spatial join all the 0.1 values between breaks together. This will reduce from hundreds of polygons to <=18 categories.
  #r_poly_sf2 <- r_poly_sf %>%
    #mutate (accumulation = r_poly_sf[[column_name]]/10) %>%
    #select (accumulation, color_factor) %>%
    #filter (color_factor != "0 in") 
  
  #Validate geometry and then simplify slightly to reduce file size
  r_poly_sf3 <- r_poly_sf2 %>%
    st_make_valid() %>%
    st_simplify(dTolerance = 10)
  
  return(r_poly_sf3)
}
snow_list[[4]] <- raster2vector_season()

#snow_list <- raster2vector_season()

# adding "season" now
timeframes <- c(timeframes, "season_")

# get some additional info for the chatter
ocr_text <- function(timeframe){
  #timeframe = "24h_"
  # make a url dynamically
  if (timeframe == "season_") {
    # first get current year
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    
    # determine the start of the season (oct 1 of the current or previous year)
    season_start_year <- if (format(Sys.Date(), "%m") < "10") {
      current_year - 1  # ff it's before oct, use the previous year
    } else {
      current_year  # otherwise, use the current year
    }
    
    season_start <- paste0(season_start_year, "093012")  # october 1, 12 UTC
    
    # define the end of the season as the current date
    season_end <- paste0(format(Sys.Date(), "%Y%m%d"), "12") # hour fixed, no "00" files
    #season_end <- "2025013012"
    
    # connect all this for seasonal URL
    path_to_image <- paste0(
      "https://www.nohrsc.noaa.gov/snowfall/data/",
      format(Sys.Date(), "%Y%m"), 
      "/sfav2_CONUS_", season_start, "_to_", season_end, ".png")
  } else {
    path_to_image <- paste0("https://www.nohrsc.noaa.gov/snowfall/data/", format(Sys.Date(), "%Y%m"), "/sfav2_CONUS_", timeframe, format(Sys.Date(), "%Y%m%d"), hour, ".png")
  }
  
  #print (paste("Trying", path_to_image))
  # extract text
  # make it all caps, since sometimes it reads "issued" and sometimes "Issued" - this helps subsetting later
  #text <- tesseract::ocr(path_to_image) %>% toupper()
  text <- try(tesseract::ocr(path_to_image) %>% toupper())
  
  # Check if path_to_image for season hits 404 error because it hasn't been created yet.
  if (timeframe == "season_"){
    if (inherits(text, "try-error")) {
      #Create path to one day behind the version just tried (1/28/25 instead of 1/29/25, etc.)
      new_path_to_image <- paste0(
        "https://www.nohrsc.noaa.gov/snowfall/data/",
        format(Sys.Date()-days(1), "%Y%m"), 
        "/sfav2_CONUS_", season_start, "_to_", 
        format(ymd(substr(season_end, 1, 8))-days(1), "%Y%m%d"), "12", ".png")
      print(paste(substr(path_to_image, nchar(path_to_image) - 39, nchar(path_to_image)), "is unavailable."))
      print(paste("Now trying", substr(new_path_to_image, nchar(new_path_to_image) - 39, nchar(new_path_to_image))))
      
      text <- tesseract::ocr(new_path_to_image) %>% toupper()
      print(paste("Pulled", substr(new_path_to_image, nchar(new_path_to_image) - 39, nchar(new_path_to_image))))
    } else {
      print(paste("Pulled", substr(path_to_image, nchar(path_to_image) - 39, nchar(path_to_image))))
    }
  }
  
  # Check if path_to_image for other three timeframes hits 404 error because it hasn't been created yet.
  if (timeframe != "season_"){
    if (inherits(text, "try-error")) {
      new_path_to_image <- paste0("https://www.nohrsc.noaa.gov/snowfall/data/", 
                                  if_else (hour==12, format(Sys.Date(), "%Y%m"), format(Sys.Date()-days(1), "%Y%m")), 
                                  "/sfav2_CONUS_", timeframe, 
                                  if_else (hour==12, format(Sys.Date(), "%Y%m%d"), format(Sys.Date()-days(1), "%Y%m%d")), 
                                  if_else (hour==12, "00", "12"), ".png")
      
      print(paste(substr(path_to_image, nchar(path_to_image) - 29, nchar(path_to_image)), "is unavailable."))
      print(paste("Now trying", substr(new_path_to_image, nchar(new_path_to_image) - 29, nchar(new_path_to_image))))
      
      text <- tesseract::ocr(new_path_to_image) %>% toupper()
      print(paste("Pulled", substr(new_path_to_image, nchar(new_path_to_image) - 29, nchar(new_path_to_image))))
    } else {
      print(paste("Pulled", substr(path_to_image, nchar(path_to_image) - 29, nchar(path_to_image))))
    }
  }

  # get the hour data was updated
  #data_updated <- ymd_hms(str_extract(text, "(?<=ISSUED\\s).*?UTC"))
  data_updated <-str_extract(text, "(?<=ISSUED\\s).*?UTC")
  
  return(data_updated)
}

# iterate the function for 24hr/48hr/72hr/season accumulations to get last updated data from .png
ocr_list <- lapply(timeframes, ocr_text)

#Save last updated times to JSON
ocr_times <- ocr_list %>% unlist()
last_updated <- tibble(timeframe = str_remove_all(timeframes, "_"), last_updated=ocr_times) 
write_json(last_updated, "outputs/last_updated.json")

#Convert list to dates for saving properly
ocr_list <- lapply(ocr_list, ymd_hms)

#Save as TopoJSON, overwriting "latest" file.
save_files1 <- function(x){
  #geojsonio::topojson_write(snow_list, 
  geojsonio::topojson_write(snow_list[[x]], 
                            #file = paste0("outputs/", str_remove(timeframes[x], "_"), "/", timeframes[x], "inches_snow_accumulation_latest.json"),
                            file = paste0("outputs/test/", timeframes[x], "inches_snow_accumulation_latest.json"),
                            object_name = "snowfall",
                            overwrite = TRUE)
}
lapply (1:length(timeframes), save_files1)

stop ("DONE")
library(R.utils)


# Load required library
library(gdalUtils)
library(stars)

# Define the input raster file from the URL
input_raster <- "https://www.nohrsc.noaa.gov/snowfall/data/202501/sfav2_CONUS_2024093012_to_2025012912.tif"

# Define the output shapefile path
output_shapefile <- "polygonized_output.shp"

# Run the GDAL polygonize tool
gdal_polygonize(input_raster, output_shapefile, field = "DN", band = 1)


r <- rast("https://www.nohrsc.noaa.gov/snowfall/data/202501/sfav2_CONUS_2024093012_to_2025012912.tif")

# Perform the polygonize operation
polygons <- as.polygons(r)

r_poly_sf3 <- r_poly_sf2 %>% 
  filter (sfav2_CONUS_2024093012_to_2025012912 > 0)
geojson_data <- geojsonio::geojson_json(r_poly_sf3)
#14.2MB
geojsonio::topojson_write(geojson_data, 
                          file = paste0("outputs/test/season3.json"),
                          object_name = "snowfall",
                          overwrite = TRUE)


#USING SUHAIL'S BINS
#3.4MB
geojson_data <- geojsonio::geojson_json(snow_list[[4]])
geojsonio::topojson_write(geojson_data, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_shrunk1.json"),
                          object_name = "snowfall",
                          overwrite = TRUE)
#3.3MB
geojsonio::topojson_write(snow_list[[4]], 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_shrunk2.json"),
                          object_name = "snowfall",
                          overwrite = TRUE)


#Trying with my new, new bins
#10.7MB
#Validate geometry and then simplify slightly to reduce file size
r_poly_sf3 <- r_poly_sf2 %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 10)
st_crs (r_poly_sf2)

r_poly_sf4 <- r_poly_sf3 %>%
  st_transform(2163)  # Convert to meters for spatial operations

r_poly_sf5 <- r_poly_sf4 %>%
  st_snap(r_poly_sf4, tolerance = 10) %>%  # Snap close points together
  st_buffer(0)  # Removes tiny artifacts


geojsonio::topojson_write(r_poly_sf4, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_new4.json"),
                          object_name = "snowfall",
                          overwrite = TRUE)
#no rounding: 8.7MB
geojson_data <- geojsonio::geojson_json(r_poly_sf3)
geojsonio::topojson_write(geojson_data, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_newest.json"),
                          object_name = "snowfall",
                          #precision = 0,
                          #quantization = 1e3,
                          overwrite = TRUE)
#STARS package: breaks3=11.3, but far more accurate than rast()
#STARS package: breaks4=8.7,
r_poly5 <- r_poly4 %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 10) 

st_crs(r_poly4)
r_poly6 <- r_poly4 %>%
  st_transform(2163) %>%  # Convert to meters for spatial operations
  st_buffer(0) 

r_poly7 <- r_poly6 %>%
  st_simplify(dTolerance = 1000) %>% # Round coordinates to nearest 100 meters
  st_transform(4326)
geojson_data <- geojsonio::geojson_json(r_poly7)

# Function to count vertices
count_vertices <- function(geom) {
  sum(sapply(st_geometry(geom), function(g) length(st_coordinates(g))))
}

# Count vertices before transformation
vertices_before <- count_vertices(r_poly4)

# Count vertices after transformation
vertices_after <- count_vertices(r_poly6)

geojsonio::topojson_write(geojson_data, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_newest_stars-breaks4-simpbuff.json"),
                          object_name = "snowfall",
                          #precision = 0,
                          #quantization = 1e3,
                          overwrite = TRUE)

#what is this?
geojsonio::topojson_write(r_poly_sf2, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_new2.json"),
                          object_name = "snowfall",
                          precision = 0,
                          quantization = 1e3,
                          overwrite = TRUE)



#TRYING WITH MY NEW BINS
#10.7MB
geojsonio::topojson_write(r_poly_sf3, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_su1.json"),
                          object_name = "snowfall",
                          overwrite = TRUE)

geojson_data <- geojsonio::geojson_json(r_poly_sf3)
#8.8MB after converting to GeoJSON and then topojson.
geojsonio::topojson_write(geojson_data, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_su2.json"),
                          object_name = "snowfall",
                          overwrite = TRUE)
#1.4MB
gzip("outputs/test/season_inches_snow_accumulation_latest_su2.json", 
     destname = "outputs/test/season_inches_snow_accumulation_latest_su2.json.gz", 
     overwrite = TRUE)

#6.1MB - changes boundaries - don't use
geojsonio::topojson_write(r_poly_sf3, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_su3.json"),
                          object_name = "snowfall",
                          precision = 0,
                          quantization = 1e3,
                          overwrite = TRUE)
#8.8MB - no change
geojsonio::topojson_write(geojson_data, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_su4.json"),
                          object_name = "snowfall",
                          precision = 0,
                          quantization = 1e3,
                          overwrite = TRUE)

#TRYING WITH MY BINS
#12.8
geojsonio::topojson_write(r_poly_sf3, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_bins1.json"),
                          object_name = "snowfall",
                          overwrite = TRUE)

geojson_data <- geojsonio::geojson_json(r_poly_sf3)
#10.2MB after converting to GeoJSON and then topojson.
geojsonio::topojson_write(geojson_data, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_bins2.json"),
                          object_name = "snowfall",
                          overwrite = TRUE)
#1.7MB
gzip("outputs/test/season_inches_snow_accumulation_latest_bins2.json", 
     destname = "outputs/test/season_inches_snow_accumulation_latest_bins2.json.gz", 
     overwrite = TRUE)

#11MB - changes boundaries - don't use
geojsonio::topojson_write(r_poly_sf3, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_bins3.json"),
                          object_name = "snowfall",
                          precision = 0,
                          quantization = 1e3,
                          overwrite = TRUE)
#14.4MB - no change
geojsonio::topojson_write(geojson_data, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_bins4.json"),
                          object_name = "snowfall",
                          precision = 0,
                          quantization = 1e3,
                          overwrite = TRUE)

#TRYING WITH ACCUMULATION TO 1 INCH
#18MB
geojsonio::topojson_write(r_poly_sf3, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_full1.json"),
                          object_name = "snowfall",
                          overwrite = TRUE)

geojson_data <- geojsonio::geojson_json(r_poly_sf3)
#14.4MB after converting to GeoJSON and then topojson.
geojsonio::topojson_write(geojson_data, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_full2.json"),
                          object_name = "snowfall",
                          overwrite = TRUE)

gzip("outputs/test/season_inches_snow_accumulation_latest_full2.json", 
     destname = "outputs/test/season_inches_snow_accumulation_latest_full2.json.gz", 
     overwrite = TRUE)

#11MB - changes boundaries - don't use
geojsonio::topojson_write(r_poly_sf3, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_shrunk1.json"),
                          object_name = "snowfall",
                          precision = 0,
                          quantization = 1e3,
                          overwrite = TRUE)
#14.4MB - no change
geojsonio::topojson_write(geojson_data, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_shrunk2.json"),
                          object_name = "snowfall",
                          precision = 0,
                          quantization = 1e3,
                          overwrite = TRUE)

library(rmapshaper)

# Simplify the polygons
sf_simplified <- ms_simplify(geojson_data, keep = 0.05)  # Adjust keep value (0.01-0.1 works well)

# Save the simplified file
#st_write(sf_simplified, "simplified_file.geojson", delete_dsn = TRUE)

geojsonio::topojson_write(r_poly_sf3, 
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_shrunk.json"),
                          object_name = "snowfall",
                          precision = 0,
                          quantization = 1e3,
                          overwrite = TRUE)
topojson_quantized <- ms_quantize(geojson_data, quantization = 1e4)  # Adjust quantization level

gzip("outputs/test/season_inches_snow_accumulation_latest_full.json", 
     destname = "outputs/test/season_inches_snow_accumulation_latest_full.json.gz", 
     overwrite = TRUE)

old_name <- "outputs/test/season_inches_snow_accumulation_latest_full.json.gz"
new_name <- "outputs/test/season_inches_snow_accumulation_latest_full.json"

# Rename the file
file.rename(old_name, new_name)

# Path to the gzipped TopoJSON file
gzipped_file <- "outputs/test/season_inches_snow_accumulation_latest_full.json.gz"
# Create a temporary file to store the decompressed content
temp_file <- tempfile(fileext = ".json")

# Decompress the gzipped file into the temporary file
gunzip(gzipped_file, temp_file)

# Now read the decompressed TopoJSON file
topojson_data <- geojsonio::geojson_read(temp_file, what = "sp")

# Clean up temporary file
unlink(temp_file)

library(geojsonio)
library(rmapshaper)

geojson <- geojson_json(r_poly_sf3)
topojson <- ms_convert(geojson, format = "topojson")
topojson <- ms_topojson(geojson)

# Convert an sf object to GeoJSON
geojson <- geojson_json(r_poly_sf3)

# Convert GeoJSON to TopoJSON
topojson <- geojsonio::geojson_convert(geojson, to = "topojson")

# Save to file
geojson_write(topojson, file = "outputs/test/season_inches_snow_accumulation_latest_try.json")

topojson_quantized <- geojson_quantize(r_poly_sf3, precision = 1e4)  # Adjust precision
geojson_write(topojson_quantized, file = "quantized_file.json")

geojsonio::topojson_write(r_poly_sf3, 
                          #file = paste0("outputs/", str_remove(timeframes[x], "_"), "/", timeframes[x], "inches_snow_accumulation_latest.json"),
                          file = paste0("outputs/test/season_inches_snow_accumulation_latest_try1.json"),
                          object_name = "snowfall",
                          precision = 0, #try1
                          quantization = 1e3, #try1
                          overwrite = TRUE)

#Save as TopoJSON using updated time to maintain record.
save_files2 <- function(x){
  #geojsonio::topojson_write(snow_list, 
  geojsonio::topojson_write(snow_list[[x]], 
                            file = paste0("outputs/", str_remove(timeframes[x], "_"), "/", format(Sys.Date(), "%Y%m%d"), hour, "_", format(ocr_list[[x]], "%H%M%S"), "_", timeframes[x], "snow_accumulation.json"),
                            object_name = "snowfall",
                            overwrite = TRUE)
}
lapply (1:length(timeframes), save_files2)
