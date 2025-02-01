library (sf)
library (geojsonio)
library(smoothr)
library(dplyr)
library(parallel)
library(data.table)

timeframes <- c("24h", "48h", "72h", "season")

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
  
  #file3 <- paste0("outputs/", timeframe, "/", timeframe, "_inches_snow_accumulation_latest.json")
  #smoothed_polygons6 <- st_read (file3)
  # 
  # geojsonio::topojson_write(smoothed_polygons6, 
  #                           file = file3,
  #                           object_name = "snowfall", 
  #                           overwrite = TRUE)
# })

  #Remove overlapping bits for smooth appearance on Mapbox
  # smoothed_polygons7 <- parallel::mclapply(1:nrow(smoothed_polygons6), function(x){
  #   #smoothed_polygons7 <- parallel::mclapply(1:100, function(x){
  #   #x=53
  #   this_feature <- smoothed_polygons6[x,]
  #   intersections = st_intersects(x = smoothed_polygons6, y = smoothed_polygons6[x,])
  #   intersections_log = lengths(intersections) > 0
  #   smoothed_polygons6a = smoothed_polygons6[intersections_log, ] %>%
  #     #filter (id != this_feature$id)# %>% 
  #     #remove any that are larger (meaning id is lower value)
  #     filter (id > this_feature$id) 
  #   
  #   if (nrow(smoothed_polygons6a) > 0){
  #     smoothed_polygons6a <- smoothed_polygons6a %>%
  #       st_union ()
  #     this_feature_clipped <- st_difference(this_feature, smoothed_polygons6a)
  #     # Convert geometry to POLYGON
  #     this_feature_clipped <- st_cast(this_feature_clipped, "POLYGON")
  #   } else {
  #     this_feature_clipped <- this_feature
  #   }
  #   return (this_feature_clipped)
  # }, mc.cores = 6L)
  
  smoothed_polygons7 <- parallel::mclapply(1:nrow(smoothed_polygons6), function(x) {
    result <- try({
      this_feature <- smoothed_polygons6[x,]
      intersections = st_intersects(x = smoothed_polygons6, y = smoothed_polygons6[x,])
      intersections_log = lengths(intersections) > 0
      smoothed_polygons6a = smoothed_polygons6[intersections_log, ] %>%
        #filter (id != this_feature$id)# %>% 
        #remove any that are larger (meaning id is lower value)
        filter (id > this_feature$id) 
      
      if (nrow(smoothed_polygons6a) > 0){
        smoothed_polygons6a <- smoothed_polygons6a %>%
          st_union ()
        this_feature_clipped <- st_difference(this_feature, smoothed_polygons6a)
        # Convert geometry to POLYGON
        this_feature_clipped <- st_cast(this_feature_clipped, "POLYGON")
      } else {
        this_feature_clipped <- this_feature
      }
      return (this_feature_clipped)
    }, silent = TRUE)
    if (inherits(result, "try-error")) {
      return(paste("Error in iteration", x, ":", result))
    }
    return(result)
  }, mc.cores = parallel::detectCores())
  
  # Set CRS since it's missing
  smoothed_polygons7 <- lapply(smoothed_polygons7, function(x) {
    st_set_crs(x, 4326)
  })
  
  file3 <- paste0("outputs/", timeframe, "/", timeframe, "_inches_snow_accumulation_latest.json")
  
  smoothed_polygons8 <- data.table::rbindlist(smoothed_polygons7, use.names=TRUE) %>%
    st_as_sf() %>%
    group_by (accumulation) %>%
    summarize (geometry = st_union(geometry)) %>% 
    st_make_valid() %>%
    st_simplify (dTolerance = 100) #adds tiny gaps here and there
  
  geojsonio::topojson_write(smoothed_polygons8, 
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