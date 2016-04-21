##' Calculate the Brier sharpness for each observation
##'
##' @details
##' The entropy is \emph{sum_k (P_k * (1 - P_k)} where \emph{P_k} is the predicted probability that the true class is \emph{k}.
##'
##' @export
##' @param X a data frame containing observations (i.e. instances, examples) on the rows, along with probability predictions for each
##' class.  May contain other columns as well.
##'
##' @param probs A character vector, logical vector, or vector of numeric indexes that identify the columns in \code{X} that contain
##' the predicted probabilities of the classes.  This is passed to
##' \href{http://pnnl.github.io/docs-Smisc/rd.html#selectelements}{selectElements()} from the
##' \href{http://pnnl.github.io/docs-Smisc}{Smisc} package.
##'
##' @param outCol A character string that indicates the name of the column that will contain the Brier sharpness
##'
##' @param scale A logical indicating whether the entropy should be linearly scaled so thatand uniform classifications
##' result in a value of \emph{0}, and probabilities with maximum information (minimum entropy) have a value of \code{1}. See Details.
##'
##' @return The data frame \code{X} is returned, with an appended column that contains the Brier sharpness.
##'
##' @examples
##' p1 <- runif(9)
##' p2 <- runif(9, 0, 1 - p1)
##' p3 <- 1 - p1 - p2
##'
##' X <- data.frame(prob_7 = p1,
##'                 prob_9 = p2,
##'                 prob_2 = p3,
##'                 extra = rnorm(9))
##' 
##' fidel_brierShaprness(X, 1:3)

fidel_brierSharpness <- function(X, probs, outCol = "brierSharpness", scale = FALSE) {

  # After checking all the args, this leaves 'Xprobs' in the environment of this function
  Smisc::sepList(check_entropy_brierSharpness_args(X, truth, probs, outCol, scale = FALSE))

  # Calculate the Brier sharpness
  brierSharpness <- apply(Xprobs * (Xprobs - 1), 1, sum)

  # Scale if asked
  if (scale) {
    brierSharpness <- Smisc::linearMap(brierSharpness, D = c(), R = c())
  }
  
  # Add the brierSharpness to the output
  X[,outCol] <- brierSharpness

  # Return the data frame
  return(X)

} # fidel_brierSharpness

