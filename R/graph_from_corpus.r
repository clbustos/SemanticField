#' Generate a Graph from igraph, using a data.frame with corpus structure or a corpus object
#' @param x a corpus object
#' @return a directed graph
#' @export
#' @import reshape
#' @import igraph
graphFromCorpus<-function(corpus) {
  x<-corpus$corpus[,c("id","order","word","coi")]
  x2<-x
  base<-x2[x2$order==1,]
  base$order<-0
  base$word<-base$coi
  x2<-rbind(base,x2)
  #x2$coi<-NULL;
  ss<-reshape::cast(x2,id~word,value="order")[,-1]
  
  words<-colnames(ss)
  lw<-length(words)
  mm<-matrix(0,lw,lw,dimnames=list(words,words))
  for(i in 1:(lw-1)) {
    for(j in (i+1):lw) {
      res<-as.numeric(ss[,i]<ss[,j])
      
      
      antes<-sum(res,na.rm=T)
      
      despues<-sum(!is.na(res))-antes
      
      mm[i,j]<-antes
      mm[j,i]<-despues
    }
  }
  igraph::graph.adjacency(mm,mode="directed",weighted=T,diag=F)
}
