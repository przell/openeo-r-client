
#' Creates a callback
#' 
#' The callback function creates a callback graph for a parameter of a process if the parameter needs
#' a \code{\link{CallbackArgument}} as a value. If the parameter is omitted it shows possible parameter names which require
#' a callback.
#' 
#' @param con a connected \code{\link{OpenEOClient}}
#' @param process a \code{\link{Process}} or \code{\link{ProcessNode}} object of a back-end process
#' @param parameter optional name of a parameter of the process which requires a callback as value. If omitted then it returns only the names of parameter that require a callback
#' @param choice_index optional integer denoting the callback parameter selected in an \code{\link{anyOf}}
#'  
#' @return a \code{\link{Graph}} object with the callback parameters as 'data'
#' 
#' @export
callback = function(con, process, parameter = NULL, choice_index=NULL) {
    if (!"Process" %in% class(process)) 
        stop("Parameter process is no process for a openeo graph")
    
    # iterate over parameters and state callback possibilities
    callbacksParameterNames = unname(unlist(sapply(process$parameters, function(param) {
        if ("callback" %in% class(param)) 
            return(param$getName())
        
        if ("anyOf" %in% class(param)) {
            if (any(sapply(param$getChoice(), function(p) {
                return("callback" %in% class(p))
            }))) return(param$getName())
        }
    })))
    
    if (!is.null(callbacksParameterNames)) {
        # if parameter is not null check if it exists and is callback
        if (!is.null(parameter) && is.character(parameter) && parameter %in% callbacksParameterNames) {
            callback_arg = process$getParameter(name = parameter)
            
            if ("anyOf" %in% class(callback_arg)) {
                
                if (is.na(choice_index) || length(choice_index) == 0) {
                    message("Callback parameter offers multiple data injection options. Please state the 'choice_index' parameter.")
                    return(invisible(NULL))
                }
                
                callback_arg = callback_arg$getChoice()[[choice_index]]
                # also replace the AnyOf choice
                process$setParameter(name=parameter, value=callback_arg)
            }
            
            cb_parameters = callback_arg$getCallbackParameters()  # all the possible data exports offered by the argument
            
            cb_graph = Graph$new(con, cb_parameters)
            
            callback_arg$setValue(cb_graph)
            return(cb_graph)
        } else {
            message(paste0("Parameter that expect a callback: ",
                       paste(callbacksParameterNames, collapse = ", ")))
        }
        
        # get the callback values that are available for this callback (data that will be used in the graph like load_collection in the main graph)
    } else {
        message("No callbacks found in the parameters of the stated process")
    }
    invisible(callbacksParameterNames)
    
}
