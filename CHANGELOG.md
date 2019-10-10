# Changelog

## [Unreleased]

## [0.5.0] - 2019-10-10

### Added
- band_arithmetics function that parses a mathematical function into a process graph if the back-end supports some of the core processes
- example script shown in the open geo hub summer school
- notebooks for comparing the NDVI calculation on EURAC back-end and GEE

### Changed
- client public function `client_version` was renamed into `api_version` and returns the openEO API version which the client uses
- graphs are initialized with a connection instead of the parsed processes list
- connection is now available via a private field in the Graph object
- callback now reuses the obtained process in JSON format of the connection

### Fixed
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