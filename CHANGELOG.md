# Changelog

## [0.5.0] - 2019-12-03

### Added
- example process graph building for minimum EVI with EURAC, EODC and VITO
- added a Google OIDC authentication option
- band_arithmetics function that parses a mathematical function into a process graph if the back-end supports some of the core processes
- example script shown in the open geo hub summer school
- notebooks for comparing the NDVI calculation on EURAC back-end and GEE

### Changed
- modified `connect` with new parameters `exchange_token` and `external`. With the latter we can specify 'google' as an external identity provider and with `exchange_token` we can specify if we send the default `access_token` or the `id_token` as Bearer token in the authentication header
- some endpoints allowed optional sending of a bearer token in the header, which was not yet considered. Added a function to check if a user has logged in (`OpenEOClient$isLoggedIn()`) and made it optional at the following endpoints to sent a Bearer token as Authorization-header depending on whether the user is logged in: 
```
GET /output_formats
GET /udf_runtimes
GET /service_types
GET /collections
GET /collections/{collection_id}
GET /processes
POST /validation
```
- modified gee-evi-example now that the GEE driver supports all relevant openeo processes, it now creates the graph with apply, linear_scale and saves it as PNG; added also an example for the band_arithmetics use case
- client public function `client_version` was renamed into `api_version` and returns the openEO API version which the client uses
- graphs are initialized with a connection instead of the parsed processes list
- connection is now available via a private field in the Graph object
- callback now reuses the obtained process in JSON format of the connection

### Fixed
- fixed callback function which not replaced the AnyOf parameter with the newly created Graph
- `create_job` now refers to the HTTP response header "OpenEO-Identifier" rather than parsing the "Location" header
- fixed a problem in `band_arithmetics` where the parameter in array_element were falsely assigned
- fixed callback function to recognize nested callback arguments in anyOf (#35)
- fixed the attempt to parse the response body when evaluating HTTP 202
- typo in the README

## [0.4.2] - 2019-09-09

### Changed
- modified the Use case 1 RClient -> EURAC example (mostly working) based on API version 0.4.2
- return a list of file paths on `download_results`

### Fixed
- fixed error on `list_file_types`
- fixed bad JSON graph formatting when sending a graph for immediate computation (`compute_result`)
