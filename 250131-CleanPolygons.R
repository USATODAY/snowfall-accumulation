library (sf)
library (geojsonio)
library(smoothr)
library(dplyr)

timeframes <- c("24h", "48h", "72h", "season")

lapply(1:3, function (x){
  #x=1
  timeframe <- timeframes[[x]]
  
  # this_shapefile <- st_read (paste0("python/", timeframe, "_snowfall.shp")) %>%
  #   st_make_valid () %>%
  #   rename (accumulation = DN) %>%
  #   filter (accumulation > 0) %>%
  #   mutate (accumulation = case_when (
  #     accumulation == 1 ~ 0.1,
  #     TRUE ~ accumulation - 1),
  #     area = st_area(geometry))

  #redone to separate trace, <1 and all others.
  this_shapefile <- st_read (paste0("python/", timeframe, "_snowfall.shp")) %>%
    st_make_valid () %>%
    rename (accumulation = DN) %>%
    filter (accumulation > 0) %>%
    mutate (accumulation = case_when (
      accumulation == 1 ~ 0.1,
      accumulation == 2 ~ 0.75,
      TRUE ~ accumulation - 2),
      area = st_area(geometry))
  
  print ("starting smoothed_polygons")
  smoothed_polygons <- smooth(this_shapefile, method = "ksmooth")
  print ("starting smoothed_polygons2")
  smoothed_polygons2 <- smoothed_polygons %>%
    st_make_valid() 
  
  smoothed_polygons3 <- smoothed_polygons %>%
    st_transform(5070) %>%   # Convert to a projected CRS to avoid Error: Snap function moved vertex (nan, nan, nan) by 3.14159265358979, which is more than the specified snap radius of 1.23413730872177e-09
    st_make_valid() 

  #print ("starting smoothed_polygons3")
  #smoothed_polygons3 <- st_transform(smoothed_polygons2, crs = 5070) combined above
  
  print ("starting singlepixels")
  #buffer single pixels at 1300
  singlepixels <- smoothed_polygons3 %>% 
    filter (as.numeric(area) < 20000000) %>%
    st_buffer (1200)
  
  print ("starting multipixels")
  #buffer multipixel polygons s at 1500 to fill in gaps
  multipixels <- smoothed_polygons3 %>% 
    filter (as.numeric(area) >= 20000000) %>%
    st_buffer (1800)
  
  print ("starting smoothed_polygons4")
  #rejoin two types
  smoothed_polygons4 <- rbind (multipixels, singlepixels) %>%
    arrange (desc(area))
  
  print ("starting smoothed_polygons5")
  #smoothed_polygons4 <- st_buffer (smoothed_polygons3, 1300)
  smoothed_polygons5 <- st_transform(smoothed_polygons4, crs = 4326) %>%
    arrange(desc(area))
  
  print ("starting smoothed_polygons6")
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
