print.summary.stuartOutput <-
function(x,...) {
  message('Warning: This is an alpha-build, so there may be (a lot of) bugs.\n')
  cat('SUMMARY OF ANALYSIS:\n\n')
  cat('Analysis Type:',x$Type,'\n')
  cat('Estimation Software:',x$Software,'\n')
  cat('Models estimated:',x$Models,'\n')
  cat('Replications of final solution:',x$Replications,'\n')
  cat('Time Required:',x$Time,'seconds\n')
  cat('\nOptimization History:\n')
  print(x$Results)
  cat('\nConstructed Subtests:\n')
  for (i in 1:length(x$Subtests)) {
    for (j in 1:length(x$Subtests[[i]])) {
      cat(paste(names(x$Subtests[[i]])[j],': ',paste(x$Subtests[[i]][[j]],collapse=' '),'\n',collapse=' ',sep=''))
    }
  }

}
