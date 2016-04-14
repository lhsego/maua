##' Generates or evaluates a single attribute utility function 
##'
##' Creates an object of class \code{saUtilFun}, a single attribute utility function that can be called later, or an
##' object of class \code{saUtilCall}, which contains the evaluation of the single attribute utility function for
##' attribute data, \code{z}.
##' 
##' @export
##' @rdname saUtil
##'
##' @param saUtilFunction A character string (or unique abbreviation) indicating the single attribute
##' utility function. Must be one (and only one) of the method functions returned by \code{\link{saUtilFunctions}}.
##' 
##' @param z A numeric vector of attribute values, or NULL.
##' 
##' @param dots Optional named arguments to the \code{\link{saUtilFunctions}} function that will set the parameters
##' for the single attribute utility function.
##'
##' @param x a \code{saUtilCall} or \code{saUtilFun} object returned by \code{saUtil}
##' 
##' @param dots additional arguments passed to \code{\link{print.default}} or \code{\link{plot.default}}
##'
##' @return
##' \item{1}{If \code{z = NULL}, the single attribute utility function with fixed parameters is returned
##' and can be called later. This function has a single argument, \code{z}, which are the measured
##' values of the attribute of interest. The returned object is of class \code{saUtilFun}.}
##' \item{2}{If a numeric vector is provided for the argument \code{z},
##' the utility method is evaluated using \code{z} for the given parameters \code{...} and the
##' calculated utility values are returned. The returned object is of class \code{saUtilCall}.}
##' 
##' @examples
##' # Create a decreasing, exponential single attribture utility function
##' uf <- saUtil("saUtil_exp", zrange = c(2, 14), urange = c(1, 0), certEquiv = 5)
##' is.function(uf)
##' 
##' # Info about 'uf' is contained in the attributes
##' attributes(uf)
##' 
##' # Plotting without specific data values
##' plot(uf)
##' 
##' # Now calculate the utility for a sequence of points
##' u <- uf(seq(2, 14, length = 10))
##' print(u)
##' plot(u)
##' 
##' # If we send actual data (attribute values) into saUtil(), the utility function
##' # is calculated for us
##' calculatedUtil <- saUtil("saUtil_log", z = runif(10), shift = -0.01, zrange = c(1/10, 1))
##' print(calculatedUtil)
##' plot(calculatedUtil)
##' 
##' # An example where the range of attribute values is defined by the existing data
##' calculatedUtil <- saUtil("saUtil_log", z = runif(10), shift = -0.01)
##' plot(calculatedUtil, col = "Blue", pch = 2, cex = 2)

saUtil <- function(saUtilFunction = "saUtil_exp", z = NULL, ...) {

  # Check the saUtilFunction argument
  saUtilFunction <- match.arg(saUtilFunction, saUtilFunctions())

  # Check the z argument
  Smisc::stopifnotMsg(is.vector(z) & is.numeric(z),
                      "'z' must be a numeric vector")
  
  inputParms <- list(...)

  ## # If certainty equivalent is provided, if theta was included, set to NA
  ## if (saUtilFunction == "saUtil_exp") {
  ##   if ("certEquiv" %in% names(inputParms)) {
  ##     if (!is.null(inputParms$certEquiv))
  ##       inputParms$theta <- as.numeric(NA)
  ##   }
  ## }
  
  # Check that arguments provided in parms match the utility method
  validArgs <- formals(saUtilFunction)
  
  if (!all(names(inputParms) %in% names(validArgs)))
    stop("'", paste(bad <- setdiff(names(inputParms), names(validArgs)), collapse = "', '"), "' ",
         ifelse(length(bad) == 1, "is not a valid argument", "are not valid arguments"),
         " to '", saUtilFunction, "'")

  # Create the list of parms. 
  # Get set of parms that were not provided in the ...
  parms <- validArgs[setdiff(names(validArgs), names(inputParms))]
  
  # Add in the parms that were provided in the ...
  parms <- c(parms, inputParms)
  
  # Now remove the data vector z
  parms <- parms[-which(names(parms) == "z")]

  # Pepare to calculate the utility function
  if (is.null(z))
    z1 <- mean(parms$zrange)
  else
    z1 <- z
  
  # Calculate the utility function for the z's that are given--or calculate it to obtain
  # theta and certEquiv, depending on the value of z1
  utilFun <- do.call(saUtilFunction, c(list(z = z1), parms))
  parms <- attributes(utilFun)$parms
  
  # If z is null, return the utility function (instead of the output of the function)
  if (is.null(z)) {

    # Remove z from parms if it wasn't there before...
    # Note this 'parms' will have both theta and certEquiv, since the last call to 'utilFun'
    # would have produced it
    parms <- parms[-which(names(parms) == "z")]

    # Define the single attribute utility function
    utilFun <- function(z) {
  
      do.call(saUtilFunction, c(list(z = z), parms))
  
    } # utilFun

    class(utilFun) <- unique(c("saUtilFun", class(utilFun)))

  }
  # Otherwise, if there are data z, assign it the class of saUtilCall
  else {
    class(utilFun) <- unique(c("saUtilCall", class(utilFun)))
  }
  
  
  # Add in attributes that make it clear what the parameters are set to
  attributes(utilFun) <- c(attributes(utilFun),
                           list(saUtilFunction = saUtilFunction),
                           list(parms = parms))

  return(utilFun)

} # saUtil

################################################################################
### Printing and plotting methods
################################################################################

##' @method print saUtilCall
##'
## @describeIn saUtil some text

# A method for printing that doesn't show the attributes
print.saUtilCall <- function(x, ...) {

  attributes(x) <- list(names = names(x))
  print(x, ...)
  
} # print.saUtilCall

##' @method print saUtilFun

print.saUtilFun <- function(x, ...) {
  
  y <- x
  attributes(y) <- list(names = names(x))
  print(y, ...)
  
  saUtilFunction <- attributes(x)$saUtilFunction
  parms <- names(attributes(x)$parms)
  cat("\n")
  Smisc::pvar(saUtilFunction, parms)
  
  obj <- deparse(substitute(x))
  cat("\nUse 'str(", ifelse(length(obj) > 1, "", obj), ")' for more details\n", sep = "")
 
} # print.saUtilFun



# A general plotting function for the classes 'saUtilCall' and 'saUtilFun'
plot_saUtil <- function(saUtilFunObject, ...) {

  # Get the function that will be plotted
  fun <- attributes(saUtilFunObject)$saUtilFunction
  
  # Get the parameters for the function call
  parms <- attributes(saUtilFunObject)$parms

  # Drop out the z values if they're present
  if (length(to.rm <- which(names(parms) == "z")))
    parms <- parms[-to.rm]

  # Determine whether saUtilFunObject is a function or an evaluated call to a utility method
  isFun <- is.function(saUtilFunObject)

  # If zrange is 'range(z)' and we don't have data...
  if ((all(as.character(parms$zrange) == c("range", "z"))) & isFun) {
    parms$zrange <- c(0, 1)
    warning("Default setting 'zrange = range(z)' does not provide a viable plotting range.\n",
            "Setting 'zrange = c(0, 1)' instead")
  }

  # Get the datapoints z if we need them
  if (!isFun)
    zR <- range(attributes(saUtilFunObject)$parms$z)
  else
    zR <- c(NA, NA)
  
  # Create sequences for the plot, where the zSeq is the union of parms$zrange and the data
  zLo <- min(parms$zrange[1], zR[1], na.rm = TRUE)
  zHi <- max(parms$zrange[2], zR[2], na.rm = TRUE)
  zSeq <- seq(zLo, zHi, length = 500)
  u <- do.call(fun, c(list(z = zSeq), parms))

  # Insert the acheived value of theta if necessary
  if (fun == "saUtil_exp")
    parms$theta <- attributes(u)$parms$theta
  
  # Remove elements from parms that are NULL
  parms <- parms[names(parms)[unlist(lapply(parms, function(x) !is.null(x)))]]
  
  # Set graphing parameters  
  inputPlotParms <- list(...)

  # Default plotting parameters which can be overriden by arguments to ...
  defaultPlotParms <- list(main = c(Smisc::pvar(fun, verbose = FALSE),
                                    Smisc::pvar(lapply(parms, eval), verbose = FALSE, digits = 4)),
                           xlab = "Value of Attribute",
                           ylab = "Utility", type = "l")
  
  plotParms <- defaultPlotParms[setdiff(names(defaultPlotParms), names(inputPlotParms))]
  plotParms <- c(plotParms, inputPlotParms)

  # Make the plot
  do.call("plot", c(list(x = zSeq, y = u), plotParms))

  # Add in the points of the the actual data if they are present
  if (!isFun)
    points(attributes(saUtilFunObject)$parms$z, saUtilFunObject, ...)

} # plot_saUtil

##' @method plot saUtilFun

# Plot methods
plot.saUtilFun <- function(x, ...) {

  plot_saUtil(x, ...)

} # plot.saUtilFun

##' @method plot saUtilCall

plot.saUtilCall <- function(x, ...) {

  plot_saUtil(x, ...)

} # plot.saUtilFun
