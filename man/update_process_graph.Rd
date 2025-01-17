% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_graph.R
\name{update_process_graph}
\alias{update_process_graph}
\title{Modify the current graph with a given}
\usage{
update_process_graph(con, id, graph = NULL, title = NULL,
  description = NULL)
}
\arguments{
\item{con}{connected and authorized openeo client object}

\item{id}{process graph id}

\item{graph}{a process graph definition created by chaining 'process()', 'collection()' or using a ProcessGraphBuilder}

\item{title}{title of the process graph (optional)}

\item{description}{description of the process graph (optional)}
}
\description{
Upload a process graph to the back-end under a given (existing) process graph.
}
