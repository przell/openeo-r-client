% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_graph_building.R
\name{Graph}
\alias{Graph}
\title{Graph object}
\value{
Object of \code{\link{R6Class}} with methods for building an openeo process graph
}
\description{
This class represents an openeo process graph. It consists of \code{\link{ProcessNode}}s and optional \code{\link{Variable}}s. The 
class as such offers also an environment where the offered processes of a back-end are made available on
runtime. This means besides the functions mentioned here, there are also the processes of the back-end
mapped dynamically after creation.
}
\section{Fields}{

\describe{
\item{\code{data}}{a named list of collection ids or callback parameters depending on the context}
}}

\section{Methods}{

\describe{
   \item{\code{$new(processes, data = list())}}{The object creator created from processes and available data. 
   If \code{data} was omitted then it is fetched from \code{\link{list_collections}}. }
   \item{$getNodes()}{a function to return a list of created \code{\link{ProcessNode}}s for this graph}
   \item{$clean()}{function to clean the graph from unused process nodes that are not connected with the graph}
   \item{$serialize()}{creates a list representation of the graph by recursively calling \code{$serialize} or 
   \code{$serializeAsReference} on all graph elements that are connected to the graph}
   \item{$validate()}{runs through the nodes and checks the validity of its argument values}
   \item{$getNode(node_id)}{searches and returns a node from within the graph referenced by its node id}
   \item{$removeNode(node_id)}{removes a process node from the graph}
   \item{$getFinalNode()}{gets the result process node of a process graph}
   \item{$setFinalNode(node)}{sets the result process node by node id or a ProcessNode}
   \item{$setArgumentValue(node_id, parameter, value)}{sets or replaces a value on a specific ProcessNodes parameter with the given value}
   \item{$getVariables()}{creates a named list of the defined variables of a process graph}
   \item{$createVariable(id,description=NULL,type="string",default=NULL)}{creates a variable and registers 
   it internally as defined variable}
   \item{$removeVariable(variable_id)}{removes a variable}
}
}

\section{Arguments}{

\describe{
   \item{processes}{a list of Process objects}
   \item{data}{optional a named list of available data}
   \item{node_id}{the id of a process node}
   \item{node}{process node or  its node id}
   \item{parameter}{the name of a parameter in a process}
   \item{value}{the value to be set for a parameter of a paricular process}
   \item{id or variable_id}{the variable id}
   \item{description}{a description field for a variable}
   \item{type}{the type of variable, default 'string'}
   \item{default}{optional default value to be set for a variable}
}
}

