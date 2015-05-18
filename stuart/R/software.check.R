software.check <-
function(software,cores,...) { #function begin

  #check for Mplus if requested
  if (software=='Mplus') {
    if (!(nzchar(Sys.which('mplus'))|nzchar(Sys.which('/Applications/Mplus/mplus')))) {
      stop('Mplus is not installed. You could try using lavaan or OpenMx instead.\n',call.=FALSE) }
  }
  
  #check for Mplus Demo if requested
  if (software=='Mplus Demo') {
    if (!(nzchar(Sys.which('mpdemo'))|nzchar(Sys.which('/Applications/Mplus/mpdemo')))) {
      stop('Mplus Demo is not installed. You could try using lavaan or OpenMx instead.\n',call.=FALSE) }
  }

  #check for OpenMx if requested
  if (software=='OpenMx') {
    if (!require(OpenMx)) {
      stop('OpenMx is not installed. You could try using lavaan or Mplus instead.\n',call.=FALSE) }
  }

  #check for lavaan if requested
  if (software=='lavaan') {
    if (!require(lavaan)) {
      stop('lavaan is not installed. You could try using OpenMx or Mplus instead.\n',call.=FALSE) }
  }

  if (!require(parallel)) {
    cat('parallel could not be loaded. STUART will continue without it.')
    cores <- 1
  }
  else {
    if (is.null(cores)) {
      cores <- detectCores()
    }
    if (cores > detectCores()) {
      cores <- detectCores()
    }    
  }  

  return(cores)

}
