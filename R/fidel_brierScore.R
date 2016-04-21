##' Calculate the Brier (Quadratic) score for each observation
##' 
##' @details The column indicated by \code{truth} must be a factor whose levels indicate all possible classes.  The columns indicated by
##' \code{prob} must have names which contain the level names in the truth column.
##'
##' The Brier score is \emph{sum_k ( (I_k - P_k)^2 ) )}, where \emph{I_k} is \emph{1} if the class \emph{k} is
##' the true class, \emph{0} otherwise; and \emph{P_k} is the predicted probability that \emph{k} is the true class.
##'
##' Note that for \code{scale = FALSE}, smaller is better, whereas for \code{scale = TRUE}, larger is better.
##'
##' The linear transformation
##' that takes place when \code{scale = TRUE} maps the Brier score, which ranges in \emph{[0, k]},
##' (\emph{k} being the number of classes) to \emph{[1  + k^2 / (1 - k), 1]}.
##' 
##' @export
##' @param X a data frame containing observations (i.e. instances, examples) on the rows, along with a truth column and
##' probability predictions for each class.  May contain other columns as well.
##'
##' @param truth A character string, logical vector, or numeric index that identifies the column in \code{X} that contains the
##' truth classes. This is passed to \href{http://pnnl.github.io/docs-Smisc/rd.html#selectelements}{selectElements()}
##' from the \href{http://pnnl.github.io/docs-Smisc}{Smisc} package.
##'
##' @param probs A character vector, logical vector, or vector of numeric indexes that identify the columns in \code{X} that contain
##' the predicted probabilities of the classes.  This is passed to
##' \href{http://pnnl.github.io/docs-Smisc/rd.html#selectelements}{selectElements()}.
##'
##' @param outCol A character string that indicates the name of the column that will contain the log scores.
##'
##' @param scale A logical indicating whether the Brier scores should be linearly scaled so that larger is better and
##' a negative score indicates the classifier has done worse than guessing (assigning uniform probabilities), and if the score is
##' \emph{1}, the classifier has chosen perfectly by assigning probability \emph{1} to the correct class.
##'
##' @return The data frame \code{X} is returned, with an appended column that contains the Brier scores
##'
##' @examples
##' p1 <- runif(9)
##' p2 <- runif(9, 0, 1 - p1)
##' p3 <- 1 - p1 - p2
##'
##' # Notice how the names of the probability columns "prob_7", "prob_9", and "prob_2" have
##' # "2", "7", and "9" in them, which are the values of truth column, "verdad"
##' X <- data.frame(verdad = factor(rep(c(2, 7, 9), 3)),
##'                 prob_7 = p1,
##'                 prob_9 = p2,
##'                 prob_2 = p3,
##'                 extra = rnorm(9))
##' 
##' fidel_brierScore(X, "verdad", 2:4)

fidel_brierScore <- function(X, truth, probs, outCol = "brierScore", scale = FALSE) {

  # After checking all the args, this leaves character values of 'truth' and 'probs',
  # as well as 'levelNames', 'matches', and the matrix 'Xprobs' in
  # the environment of this function
  Smisc::sepList(check_logScore_brierScore_args(X, truth, probs, outCol, scale))

  # Construct the truth indicator matrix
  indMatrix <- truthIndicator(X, truth, probs, levelNames, matches)

  # Calculate Brier score
  brierScore <- apply((indMatrix - Xprobs)^2, 1, sum))

  # Scale it if requested.  Mapping uniform guesses to 0 (k - 1) / k, mapping a perfect score, 0, to 1.
  if (scale) {
    k <- length(probs)
    brierScore <- Smisc::linearMap(brierScore, D = c(0, (k - 1) / k), R = c(1, 0))
  }
  
  # Add the brierScore to the output
  X[,outCol] <- brierScore

  # Return the data frame
  return(X)

} # fidel_brierScore

