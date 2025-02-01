library (sf)
library (geojsonio)
library(smoothr)
library(dplyr)
#library(tictoc)

timeframes <- c("24h", "48h", "72h")

lapply(1:3, function (x){
  #x=1
  timeframe <- timeframes[[x]]

  this_shapefile <- st_read (paste0("python/", timeframe, "_snowfall.shp")) %>%
    st_make_valid () %>%
    rename (accumulation = DN) %>%
    filter (accumulation > 0) %>%
    mutate (accumulation = case_when (
      accumulation == 1 ~ 0.1,
      TRUE ~ accumulation - 1),
      area = st_area(geometry))
  
  #for season only
  # system.time(#100s = 38MB, without 51MB
  # this_shapefile2 <- this_shapefile %>%
  #   group_by (accumulation) %>%
  #   summarize (geometry=st_union(geometry)) %>%
  #   st_make_valid()
  # )
  # smoothed_polygons <- smooth(this_shapefile2, method = "ksmooth")
  # smoothed_polygons2 <- smoothed_polygons2 %>% 
  #   mutate (area = st_area(geometry))
  # 
  #ALL OTHERS
  smoothed_polygons <- smooth(this_shapefile, method = "ksmooth")
  
  smoothed_polygons2 <- smoothed_polygons %>%
    st_make_valid() 
  smoothed_polygons3 <- st_transform(smoothed_polygons2, crs = 5070)
  
  #buffer single pixels at 1300
  singlepixels <- smoothed_polygons3 %>% 
    filter (as.numeric(area) < 20000000) %>%
    st_buffer (1200)
  
  #buffer multipixel polygons s at 1500 to fill in gaps
  multipixels <- smoothed_polygons3 %>% 
    filter (as.numeric(area) >= 20000000) %>%
    st_buffer (1800)
  
  #rejoin two types
  smoothed_polygons4 <- rbind (multipixels, singlepixels) %>%
    arrange (desc(area))
  
  #smoothed_polygons4 <- st_buffer (smoothed_polygons3, 1300)
  smoothed_polygons5 <- st_transform(smoothed_polygons4, crs = 4326) %>%
    arrange(desc(area))
  
  smoothed_polygons6 <- smoothed_polygons5 %>%
    st_simplify(dTolerance = 500) %>% 
    st_make_valid() %>%
    arrange(desc(area), desc(accumulation)) #%>%
  
  file3 <- paste0("outputs/", timeframe, "/", timeframe, "_inches_snow_accumulation_latest.json")
  
  geojsonio::topojson_write(smoothed_polygons6, 
                            file = file3,
                            object_name = "snowfall", 
                            overwrite = TRUE)
})

# Run mapshaper in R to simplify and convert to TopoJSON
topojson_file <- paste0("outputs/", timeframe, "/", timeframe, "_inches_snow_accumulation_latest2.json")

#system("/Users/AFast/.nvm/versions/node/v22.11.0/bin/mapshaper input.json -o precision=0.1 format=topojson output.json")
system(paste("/Users/AFast/.nvm/versions/node/v22.11.0/bin/mapshaper", file3, "-o precision=0.1", topojson_file))
#22mb but looks bad

smoothed_polygons6 <- smoothed_polygons5 %>%
     group_by (accumulation) %>%
     summarize (geometry=st_union(geometry)) %>%
     st_make_valid()

geojsonio::topojson_write(smoothed_polygons6, 
                          file = paste0("outputs/", timeframe, "/", timeframe, "_inches_snow_accumulation_latest-union.json"),
                          object_name = "snowfall", 
                          overwrite = TRUE)
smoothed_polygons7 <- smoothed_polygons6 %>%
  st_simplify(dTolerance = 500) %>% 
  st_make_valid()
geojsonio::topojson_write(smoothed_polygons7, 
                          file = paste0("outputs/", timeframe, "/", timeframe, "_inches_snow_accumulation_latest-union-simp.json"),
                          object_name = "snowfall", 
                          overwrite = TRUE)  

# Fix mapshaper-compressed version of seasonal map
lapply(4, function (x){
  timeframe <- timeframes[[x]]
  file <- paste0("outputs/", timeframe, "/", timeframe, "_inches_snow_accumulation_latest.json")
  
  squeezed <- topojson_read(file)
  
  #remove slivers on edges
  squeezed2 <- squeezed %>% 
    #st_make_valid() %>%
    st_simplify(dTolerance = 0.01)
  
  #save fixed file
  geojsonio::topojson_write(squeezed2, 
                            file = file,
                            object_name = "snowfall", 
                            overwrite = TRUE)
})



#STARS SEASON AND THEN SMOOTH
raster2vector_season <- function() {
  #which library to use? stars::read_stars() or terra::rast() -- read_stars() takes longer but is truer to map on NOAA site.
  version = "stars"
  
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
  
  if (version=="terra"){
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
    
    print (paste("Using column:", column_name))
    
    # convert to polygons with rounded raster values
    r_poly <- as.polygons(r,
                          round=TRUE,
                          digits=1, #round to nearest 1" (will lose 0.01-0.49" to 0)
                          aggregate=TRUE) #false produces separate features for all with same accumulation value.
  }
  
  if (version=="stars"){
    r <- try(read_stars(path_to_raster), silent = TRUE)
    
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
      r <- read_stars(new_path_to_raster)
      print(paste("Pulled", substr(new_path_to_raster, nchar(new_path_to_raster) - 39, nchar(new_path_to_raster))))
      column_name <- paste0("sfav2_CONUS_", season_start, "_to_", format(ymd(substr(season_end, 1, 8))-days(1), "%Y%m%d"), "12", ".tif")
    } else {
      print(paste("Pulled", substr(path_to_raster, nchar(path_to_raster) - 39, nchar(path_to_raster))))
      column_name <- paste0("sfav2_CONUS_", season_start, "_to_", season_end, ".tif")
    }
    
    r_poly <- st_as_sf(r, merge = TRUE)  # Merge adjacent areas of same value
  }
  
  # convert raster polygons to sf
  r_poly2 <- st_as_sf(r_poly) %>%
    filter (get(column_name) > 0) %>% #remove NAs and 0
    rename (accumulation = column_name)
  
  r_poly3 <- r_poly2 %>%
    mutate (accumulation = case_when(
      #accumulation< 0 ~ -1,
      #accumulation == 0 ~ 0,
      between(accumulation, 0, 1) ~ 1, #round 0.1-0.4 up to 1
      TRUE ~ round(accumulation)))
  
  #check <- r_poly2 %>% st_drop_geometry()
  
  r_poly4 <- r_poly3 %>%
    group_by (accumulation) %>%
    summarize (geometry=st_union(geometry)) %>%
    st_make_valid()
  
  return(r_poly4)
}
library(stars)
system.time( #202s for stars
  snow_list <- raster2vector_season()
)
