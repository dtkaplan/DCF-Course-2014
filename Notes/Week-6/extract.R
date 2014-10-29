#' Extract substrings
#' 
extract <- function( data, pattern, var, ... ) {
  var <- substitute( var )
  resNames <- list( ... )
  dataStrings <- as.character( 
    eval( var, envir=data, enclos=parent.frame() ) 
  )
  results <- data.frame(stringr::str_match( dataStrings, pattern )[,-1])
  # default names: match1, match2, ...
  newNames <- paste0( "match", 1:ncol(results) )
  # pull out any names given in ...
  if (length(resNames) > 0){
    if (! all(unlist(resNames) %in% 1:ncol(results)) )
      stop( "New names should be assigned to values in 1:npatterns")
    newNames[unlist(resNames)] <- names(resNames)
  }
  names(results) <- newNames
  return( cbind(data, results ) )
}