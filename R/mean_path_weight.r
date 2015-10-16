#' Calculates mean path weight for every person
#' In raw 
#' @param x Corpus 
#' @return data.frame with raw MWP for every person
#' @export
#' @import igraph
meanPathWeight<-function(x) {
  gfc<-igraph::as.undirected(graphFromCorpus(x))
  
  c.cor<-x$corpus[order(x$corpus$id,x$corpus$order),]
  mpw<-numeric(x$n.subjects)
  ids<-unique(c.cor$id)
  for(i in 1:length(ids)) {
    c.id<-ids[i]
    w<-0
#    print(c.cor[c.cor$id==i,"coi"][1])
    palabras<-c(as.character(c.cor[c.cor$id==c.id,"coi"][1]),as.character(c.cor[c.cor$id==c.id,"word"]))
  #  print(palabras)
    if(length(palabras>1)) {
      for(j in 1:(length(palabras)-1)) {
        ee<-E(gfc)[get.edge.ids(gfc,palabras[c(j,j+1)])]
        w<-w+get.edge.attribute(gfc,"weight",ee)
      }
    }
    mpw[i]<-w / (length(palabras)-1)
  }
  data.frame(id=ids,mpw=mpw)
}
