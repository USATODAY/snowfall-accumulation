library (sf)
library (geojsonio)
library(smoothr)
library(dplyr)

timeframes <- c("24h", "48h", "72h", "season")

lapply(1:3, function (x){
  #x=3
  timeframe <- timeframes[[x]]
  
  this_shapefile <- st_read (paste0("python/", timeframe, "_snowfall.shp")) %>%
    st_make_valid () %>%
    rename (accumulation = DN) %>%
    filter (accumulation > 0) %>%
    mutate (accumulation = case_when (
      accumulation == 1 ~ 0.1,
      TRUE ~ accumulation - 1),
      area = st_area(geometry))
  
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