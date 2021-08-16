#' @export

print.stuartEmpiricalObjective <- function(x, ...) {
  cat('Empirical STUART objectitve function with:\n\n')
  cat(x$string)
  cat('\n\nUse ...$func() to apply function to data.')
}

#' @export

print.stuartDefaultObjective <- function(x, ...) {
  cat('Default STUART objectitve function with:\n\n')
  cat(x$string)
  cat('\n\nUse ...$func() to apply function to data.')
}

#' @export

print.stuartManualObjective <- function(x, ...) {
  cat('Manual STUART objectitve function with:\n\n')
  cat(x$string)
  cat('\n\nUse ...$func() to apply function to data.')
}