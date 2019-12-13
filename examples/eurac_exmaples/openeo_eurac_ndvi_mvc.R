#' ---
#' title: "First steps with R and openEO"
#' author: "peterjames.zellner@eurac.edu"
#' date: "2019/10/08"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---

#+ setup, include=FALSE
knitr::opts_chunk$set(collapse = TRUE)

#' # topics
#' - Get meteo station data through MonalisaR
#' - Get ndvi station data through MonalisaR
#' - Get raster data through openeo
#' - Compare raster data with station data

#' # libraries
#+ libraries, echo=TRUE, message=FALSE, warning=FALSE
library(devtools)
#install_github(repo="Open-EO/openeo-r-client",ref="develop", dependencies=TRUE)
#install_github("https://github.com/mattia6690/MonalisR", dependencies = TRUE)

library(openeo)
library(MonalisR)
library(vctrs)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(mapview)
library(raster)
library(sf)

#' # Get station data ==========================================================
#' ## get ndvi station data ----------------------------------------------------
#' get station locations
mona_stations = MonalisR::getMonalisaDB(subset = "geom") %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  mutate(LAT = as.numeric(LAT), 
         LON = as.numeric(LON)) %>% 
  st_as_sf(coords = c("LAT", "LON"), crs = 4326)

#' filter stations with optical sensor for ndvi data
mona_ndvi = MonalisR::getMonalisaDB(subset = "combined") %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  dplyr::filter(prop == "Normalized Difference Vegetation Index - average")

mona_stations = mona_stations %>% 
  dplyr::filter(FOI %in% unique(mona_ndvi$foi))

#' look at them on a map
mapview(mona_stations)

#' select some stations
mona_stations = mona_stations %>% 
  dplyr::filter(FOI %in% c("domef2000", "vimef2000"))

#' download the ndvi data for the selected stations
mona_ndvi <- downloadMonalisa(datestart = "2016-01-01 00:00",
                              dateend = "2016-12-31 00:00",
                              foi = mona_stations$FOI,
                              procedure = "",
                              property = "Normalized Difference Vegetation Index - average")

mona_ndvi = unnest(mona_ndvi) %>% 
  dplyr::select(foi, TimeStamp, Value) %>% 
  dplyr::mutate(day = lubridate::date(TimeStamp)) %>% 
  dplyr::group_by(foi, day) %>% 
  dplyr::summarise(ndvi = mean(Value, na.rm = TRUE))

#' plot daily ndvi at stations
ggplot(mona_ndvi, aes(x = day, y = ndvi, col = foi)) + 
  geom_line() + 
  geom_smooth() +
  facet_wrap(.~foi)

#' # Aggregate station data to weeks -------------------------------------------
mona_ndvi_week = mona_ndvi %>%
  dplyr::mutate(week = lubridate::week(day)) %>%
  dplyr::group_by(foi, week) %>%
  dplyr::summarise(ndvi = mean(ndvi, na.rm = TRUE))

ggplot(mona_ndvi_week, aes(x = week, y = ndvi, col = foi)) +
  geom_line() +
  geom_smooth() +
  facet_wrap(.~foi)

#' ## create test sites around ndvi stations -----------------------------------
test_sites = st_transform(mona_stations, 3035) %>% 
  st_buffer(dist = 50, endCapStyle = "SQUARE")

#' look at the test sites
mapview(test_sites) + mapview(mona_stations)

#' select one...
station = "domef2000" # "vimef2000"
test_sites = test_sites %>% 
  dplyr::filter(FOI == station)
test_station = mona_stations %>% 
  dplyr::filter(FOI == station)


aoi = test_sites %>% 
  st_transform(4326) %>% 
  st_bbox() %>% 
  as.list()

names(aoi) = c("west", "south", "east", "north")


#' # Get raster data via openEO ================================================
#' ## connection to openEO API --------------------------------------------------
driver_url = "https://openeo.eurac.edu"

httr::set_config(httr::config(ssl_verifypeer = 0L))
httr::set_config(httr::config(ssl_verifyhost= 0L))

user = "guest"
password = "guest_123"

#' establish the connection
conn = connect(host = driver_url, 
               user = user, 
               password = password, 
               login_type = "basic")

#' ## get meta information ------------------------------------------------------
#' list available collections
collections = conn %>% list_collections() %>% 
  as.data.frame() %>% 
  dplyr::select(title, extent.spatial)
collections

#' list available processes
conn %>% list_processes() %>% names()

#' get detailed descriptions
conn %>% describe_account()
conn %>% describe_process("normalized_difference")

#' get detailde info on the collections
#' http://10.8.244.147:8080/rasdaman/ows
conn %>% describe_collection(c("S2_L2A_T32TPS_10M"))
conn %>% describe_collection(c("SAO_S2_ST_DEM_BRDF_10m_L2A"))

#' ## create a process graph / task ---------------------------------------------
#' start an empty process graph
graph = conn %>% process_graph_builder()

#' add loading data to the graph
aoi
timespan = c("2016-07-01T00:00:00.000Z", "2016-07-31T00:00:00.000Z")
bands = c("B04", "B08")

data1 = graph$load_collection(id = graph$data$`SAO_S2_ST_DEM_BRDF_10m_L2A`,     
                              spatial_extent = aoi,
                              temporal_extent = timespan,  
                              bands = bands)

#' filter bands
b_red = graph$filter_bands(data = data1,bands = bands[1])
b_nir = graph$filter_bands(data = data1,bands = bands[2])

#' calc ndvi
ndvi = graph$normalized_difference(band1 = b_red, band2 = b_nir)

#' get maximum value in timespan
reducer = graph$reduce(data = ndvi, dimension = "temporal")
cb_graph = conn %>% callback(reducer, parameter = "reducer", choice_index = 1)
cb_graph$max(data = cb_graph$data$data) %>% cb_graph$setFinalNode()

#' set final node of the graph
graph$save_result(data = reducer, format = "GTiff") %>%  # "netcdf" "GTiff"
  graph$setFinalNode()

#' ## send process graph to openeo ------------------------------------------------
job_id = conn %>% create_job(graph = graph, 
                             title = "ndvi_ts", 
                             description = "ndvi_ts",
                             format =  "GTiff") # "netcdf" "GTiff"

conn %>% start_job(job_id)
result_obj = conn %>% list_results(job_id)
getwd()
ras_path = conn %>% download_results(job = job_id, folder = "/home/pzellner@eurac.edu")
ras_path = ras_path[[1]]

#' ## load the result -------------------------------------------------------------
#' the path has to be adjusted after every download
file.size(ras_path)/1e+6
ras = raster::stack(ras_path)
ras = raster(ras_path)
ras
dim(ras)
ncell(ras)
summary(ras)

#' assign projection information
#raster::crs(ras) <- crs("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs") # "+epsg:32632"

#' get a first glimpse
mapview(ras) + mapview(test_sites) + mapview(test_station)

#' ## extract values at stations
raster_ndvi = raster::extract(ras, test_station)
raster_ndvi = raster::extract(ras, test_sites, fun = mean)

raster_ndvi = tibble(foi = "ras", day = lubridate::date("2016-07-15"), ndvi = raster_ndvi[1])

#' ## plot results
#' station data
ggplot(mona_ndvi %>% filter(foi == station), aes(x = day, y = ndvi, col = foi)) + 
  geom_line() + 
  #geom_point() +
  geom_smooth() + 
  geom_point(data = raster_ndvi)


#' # Acknowledgements ================================================================
#' This Tutorial is based upon the work presented here: <br>
#' https://github.com/Open-EO/openeo-r-client
#' and here <br>
#' https://mattia6690.github.io/MonalisR/
#'


