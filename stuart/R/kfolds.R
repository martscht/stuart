### Roxygen-izable Documentation ----
#' Data selection for k-folds crossvalidation.
#' 
#' Split a \code{data.frame} into two subsets for k-folds crossvalidation.
#' 
#' @author Martin Schultze
#' 
#' @seealso \code{\link{mmas}}, \code{\link{crossvalidate}}
#' 
### Inputs ----
#' @param data A \code{data.frame}.
#' @param k The k-folding parameter. Defaults to 2, for an even split.
#' @param seed A random seed. See \code{\link{Random}} for more details.
#' 
### Outputs ----
#' @return Returns a list containing two \code{data.frame}s, called calibrate and validate. The first corresponds to the calibration sample, the second to the validation sample.
#' 
#' @concept ACO subtests
#' 
#' @export


kfolds <- function(data, k = 2, seed = NULL) {
  
  # set random seed
  if (!is.null(seed)) {
    old.seed <- .Random.seed
    old.kind <- RNGkind()[1]
    set.seed(seed)
  }
  
  n_cali <- ceiling(nrow(data)/k * (k - 1))
  filter <- sort(sample(nrow(data), n_cali))
  
  output <- list(calibrate = data[filter, ], validate = data[-filter, ])
  class(output) <- 'stuartKfolds'
  
  # return to previous random seeds
  if (!is.null(seed)) {
    RNGkind(old.kind)
    .Random.seed <<- old.seed
  }
  
  return(output)
  
} 