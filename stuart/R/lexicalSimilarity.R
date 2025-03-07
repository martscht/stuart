lexicalSimilarity <- function(
  data = NULL, factor.structure, capacity = NULL,
  item.phrasing,
  model = 'bert-base-multilingual-cased', method = 'cosine',
  ...) {
  
  if (is.null(names(item.phrasing))) {
    warning('No names were provided for the item phrasing. Assuming same order of items as in factor.structrue.')
    names(item.phrasing) <- unlist(factor.structure)
  }
  
  message(paste0('\nInitializing text package...'))
  suppressMessages(text::textrpp_initialize())
  
  # set defaults (if nothing provided)
  args <- c(model = list(model), method = list(method), list(...))
  if (!'remove_non_ascii' %in% names(args)) args$remove_non_ascii <- FALSE
  
  embeds <- vector('list', length(item.phrasing))

  message(paste0('\nRunning text embedding through ', args$model, '...'))
  pb <- txtProgressBar(min = 0, max = length(item.phrasing), style = 3)
  embed_args <- args[names(args) %in% names(formals(text::textEmbed))]
  for (i in 1:length(item.phrasing)) {
    embeds[[i]] <- suppressMessages(try(do.call(text::textEmbed, c(list(texts = item.phrasing[i]), embed_args))))
    setTxtProgressBar(pb, i)
  }
  
  simi <- matrix(0, nrow = length(item.phrasing), ncol = length(item.phrasing))
  
  message(paste0('\nComputing pairwise ', args$method, ' item-similarities...'))
  
  pb <- txtProgressBar(min = 0, max = length(item.phrasing), style = 3)
  
  for (i in 1:length(item.phrasing)) {
    for (j in 1:length(item.phrasing)) {
      if (i == j) {
        simi[i, j] <- 1
      } else {
        simi[i, j] <- text::textSimilarity(embeds[[i]]$texts$texts, embeds[[j]]$texts$texts, method = args$method)
      }
    }
    setTxtProgressBar(pb, i)
  }
  
  colnames(simi) <- rownames(simi) <- names(item.phrasing)
  
  out <- list(
    factor.structure = factor.structure,
    similarity.matrix = simi
  )
  
  return(out)
}
