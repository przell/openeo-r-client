# test single party of workflow of use case

# libraries
library(openeo)
library(dplyr)

# connection to openEO Eurac backend 
driver_url = "https://openeo.eurac.edu"
user = "aceo"
password = "aceo_123"

conn = connect(host = driver_url, 
               user = user, 
               password = password, 
               login_type = "basic")

conn %>% describe_collection("s1a_t117_epsg3035_20m_VV")
conn %>% describe_collection("openEO_S2_32632_10m_L2A")
conn %>% describe_process("max_time")

openeo::dimensions()

# 1. Reduce temporal -----------------------------------------------------------
# define timespan
timespan = c("2014-09-01T00:00:00.000Z",   
             "2015-09-01T00:00:00.000Z")
timespan = c("2018-06-04T00:00:00Z",
             "2018-06-23T00:00:00Z")

# add loading data to the graph
aoi = list("east" = 11.338266134262085,
           "south" = 46.50121343180268,
           "north" = 46.50638274103821,
           "west" = 11.32489800453186)

# start an empty process graph
graph = conn %>% process_graph_builder()

# load data cubes
s1a_vv = graph$load_collection(id = graph$data$`openEO_S2_32632_10m_L2A`, #openEO_S2_32632_10m_L2A, s1a_t117_epsg3035_20m_VV
                               temporal_extent = timespan, spatial_extent = aoi)


temporal_reduce = graph$reduce(data = s1a_vv, dimension = "temporal")
avg_graph = conn %>% callback(temporal_reduce, parameter = "reducer", choice_index = 1)
avg_graph$mean(data = avg_graph$data$data) %>% avg_graph$setFinalNode()
temporal_reduce %>% graph$save_result(format="json") %>% graph$setFinalNode()

graph

job_id = conn %>% create_job(graph=graph, 
                             title="prep_use_case_eurac", 
                             description="prep_use_case_eurac",
                             format="json")

conn %>% start_job(job_id)
result_obj = conn %>% list_results(job_id)
conn %>% download_results(job = job_id, folder = "/home/pzellner@eurac.edu")

tmp = openeo::compute_result(con = conn, graph = graph, format = "json")
head(tmp)
tmp2 = rawToChar(tmp)
tmp_json = jsonlite::fromJSON(tmp2)
tmp_json2 = jsonlite::toJSON(tmp_json,unbox=TRUE,pretty = TRUE)
dim(tmp_json) # time x y


