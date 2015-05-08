#' Returns a data.frame with nodes LAI and degree
#' @param x a data.frame that conforms to a corpus of lexical availability
#' @param trim level for graph
#' @return a data.fram with columns (word,lai,degree)  
#' @export
#' @import igraph
compareLAIDegree<-function(x,trim=1) {
  sf<-generateCorpus(x)
  lai<-data.frame(word=names(sf$idl),lai=as.numeric(sf$idl))
  
  gr<-graphFromCorpus(x)
  gr<-trimByWeight(as.undirected(gr),trim)
  deg<-igraph::degree(gr,mode="all")
  deg<-data.frame(word=names(deg),degree=as.numeric(deg))
  merge(lai,deg)
}
