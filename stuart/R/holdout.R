### Roxygen-izable Documentation ----
#' Data selection for holdout validation.
#' 
#' Split a \code{data.frame} into two subsets for holdout validation.
#' 
#' @author Martin Schultze
#' 
#' @seealso \code{\link{mmas}}, \code{\link{crossvalidate}}
#' 
### Inputs ----
#' @param data A \code{data.frame}.
#' @param prop Proportion of data in calibration sample. Default to .5, for an even split.
#' @param seed A random seed. See \code{\link{Random}} for more details.
#' 
### Outputs ----
#' @return Returns a list containing two \code{data.frame}s, called calibrate and validate. The first corresponds to the calibration sample, the second to the validation sample.
#' 
#' @concept ACO subtests
#' 
#' @export


holdout <- function(data, prop = .5, seed = NULL) {
  
  # set random seed
  if (!is.null(seed)) {
    old.seed <- .Random.seed
    old.kind <- RNGkind()[1]
    set.seed(seed)
  }
  
  n_cali <- ceiling(nrow(data)*prop)
  filter <- sort(sample(nrow(data), n_cali))
  
  output <- list(calibrate = data[filter, ], validate = data[-filter, ])
  class(output) <- 'stuartHoldout'
  
  # return to previous random seeds
  if (!is.null(seed)) {
    RNGkind(old.kind)
    .Random.seed <<- old.seed
  }
  
  return(output)
  
} 