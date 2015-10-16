#' Returns a data.frame with statistics for each node
#' @param corpus
#' @param trim level for graph
#' @return a data.frame with columns (word,lai,degree, weight.sum, weight.mean)  
#' @export
#' @import igraph
nodeStatistics<-function(corpus,trim=1) {
  wf<-corpus$words.frequency
  
  lai<-LAI(corpus)
  lai<-data.frame(word=names(lai),lai=as.numeric(lai))
  gr<-graphFromCorpus(corpus)
  gr<-trimByWeight(as.undirected(gr),trim)
  #deg<-igraph::degree(gr,mode="all")
  nw<-nodeWeight(gr)
  #print(wf)
  out<-merge(wf,lai,by="word")
  out<-merge(out,nw,by="word")
  out
}
