##' Lists the available single attribute utility functions
##'
##' By default, this function returns a vector with all of the available single attribute
##' utility functions that are available in the \code{maua} package. It can also be used to
##' verify that a single attribute utility function is available in the package.
##'
##' @details \code{saUtilFunctions()} facilitates the addition of new single attribute utility functions to the package.
##' 
##' @export 
##' @param method If supplied, a character string with the name of the single attribute utility function.
##' 
##' @return By default, a vector with all available single attribute utility functions, or if a single attribute utility function
##' is provided to \code{saUtilFun}, \code{TRUE} or \code{FALSE} is returned depending on whether the function exists.
##'
##' @examples
##' saUtilFunctions()
##'
##' # The 'saUtil_exp' function exists
##' saUtilFunctions("saUtil_exp")
##'
##' # But the 'noUtil'function does not
##' saUtilFunctions("notlikely")

saUtilFunctions <- function(saUtilFun = NULL) {

  Smisc::stopifnotMsg(if (!is.null(saUtilFun)) {
                        is.character(saUtilFun) & (length(saUtilFun) == 1)
                      } else TRUE,
                      "'saUtilFun' must be NULL or a single character string indicating the single attribute utility function")
    
  saUtilFuns <- c("saUtil_exp", "saUtil_log")
    
  if (is.null(saUtilFun)) {
    return(saUtilFuns)
  } else {
    return(saUtilFun %in% saUtilFuns)
  }

} # saUtilFunctions
