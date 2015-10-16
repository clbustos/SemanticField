#' Generate matrix with people as rows and words as columns
#' 
#' @param x a corpus object
#' @return x a matrix
#' @export

bagOfWords<-function(x) {
  corpus<-x$corpus
  out<-xtabs(~corpus$id+corpus$word)
  
}
