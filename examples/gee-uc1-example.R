# POC: Use Case 1 RClient -> GEE back-end v0.4.2
library(openeo)
library(magrittr)
library(tibble)

user = "group4"
pwd = "test123"

# 1. Requesting the API versions available at the back-end
gee_host_url = "https://earthengine.openeo.org"
api_versions(gee_host_url)

gee %>% list_file_types()

gee = connect(host = gee_host_url, version = "0.4.2", user = user, password = pwd, login_type = "basic")
# also inserting the direct link is possible gee = connect(host = 'https://earthengine.openeo.org/v0.4',user = user,password = pwd)

# 2. Requesting the capabilities of the back-end
gee %>% capabilities()

# 3. Check which collections are available at the back-end
collections = gee %>% list_collections()
collections

# 4. Request details about a specific collection
gee %>% describe_collection("COPERNICUS/S2")

# 5. Check that needed processes are available
gee %>% list_processes()
gee %>% describe_process("reduce")

# 6. Request the supported secondary web service types
gee %>% list_service_types()

# 7. Create a WMS service (XYZ in this case)

graph = gee %>% process_graph_builder()
data1 = graph$load_collection(id = graph$data$`COPERNICUS/S2`, spatial_extent = list(west = -2.7634, south = 43.0408, east = -1.121, north = 43.8385), temporal_extent = c("2018-04-30", 
    "2018-06-26"), bands = c("B4", "B8"))
b4 = graph$filter_bands(data = data1, bands = "B4")
b8 = graph$filter_bands(data = data1, bands = "B8")

ndvi = graph$normalized_difference(band1 = b8, band2 = b4)

reducer = graph$reduce(data = ndvi, dimension = "temporal")

cb_graph = gee %>% callback(reducer, parameter = "reducer")

cb_graph$min(data = cb_graph$data$data) %>% cb_graph$setFinalNode()


apply_linear_transform = gee %>% graph$apply(data = reducer)

cb2_graph = gee %>% callback(apply_linear_transform, "process")

cb2_graph$linear_scale_range(x = cb2_graph$data$x, inputMin = -1, inputMax = 1, outputMin = 0, outputMax = 255) %>% cb2_graph$setFinalNode()

graph$save_result(data = apply_linear_transform, format = "png") %>% graph$setFinalNode()

graph

# client-sided validation
graph$validate()

# server-sided validation
gee %>% validate_process_graph(graph = graph)

service_id = gee %>% create_service(type = "xyz", graph = graph, title = "UC1 service with R", description = "Created a XYZ service from R using the graph for Use Case 1 (NDVI calculation)")
service_id

# 8. Requesting the service information
gee %>% list_services()

service = gee %>% describe_service(service_id)
url = service$url

# 8. b) visualizing a xyz service with leaflet
library(leaflet)
leaflet() %>% addTiles() %>% addTiles(url, tileOptions(tms=TRUE)) %>% setView(lng = -1.8,lat=43.4,zoom = 8)


# 9. alternative download / processing 
# 9 a) direct computation
library(sp)
library(raster)
gee %>% compute_result(graph=graph,format="png",output_file = "gee_test.png")
spplot(raster("gee_test.png"))

# 9.b) batch processing
job_id = gee %>% create_job(graph=graph,title="UC1 Rclient NDVI")
gee %>% start_job(job_id)
gee %>% download_results(job_id,folder = "./gee_test/")
gee %>% delete_job(job_id)
