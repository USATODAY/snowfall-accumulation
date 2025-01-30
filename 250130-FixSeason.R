library (sf)
library (geojsonio)

# file2 <- "outputs/season/season_inches_snow_accumulation_latest.json"
# 
# squeezed <- topojson_read(file2)
# 
# #remove slivers on edges
# squeezed2 <- squeezed %>% 
#   #st_make_valid() %>%
#   st_simplify(dTolerance = 0.01)
# 
# #save fixed file
# geojsonio::topojson_write(squeezed2, 
#                           file = file2,
#                           object_name = "snowfall", 
#                           overwrite = TRUE)

timeframes <- c("24h", "48h", "72h", "season")

lapply(1:length(timeframes), function (x){
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
