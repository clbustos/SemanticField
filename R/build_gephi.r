#' Build a Gephi file based on a corpus 
#' @param corpus Corpus
#' @param trim level to trim the graph paths. 
#' @param undirected if graph should be directed or undirected
#' @param filename
#' @return boolean
#' @export
#' @import igraph
buildGephiFromCorpus<-function(corpus,trim=1, FUN_direction=as.directed, filename) {
  lai<-LAI(corpus)
  gr<-trimByWeight(FUN_direction(graphFromCorpus(corpus)),trim)
  buildGephiFromGraph(gr,filename,lai=data.frame(word=names(lai),lai=as.numeric(lai)))
}


#' Build a Gephi file based on graph 
#' @param gr Graph
#' @param filename
#' @param lai Optional data.frame, with "word" and "lai"
#' @return boolean
#' @export
#' @import igraph
#' @import rgexf
buildGephiFromGraph<-function(gr,filename,lai=NULL) {
  vertex=data.frame(ID = c(1:(igraph::vcount(gr))), NAME = igraph::V(gr)$name)
  nodesAtt=data.frame(degree_=degree(gr))
  #print(idl)
  if(!is.null(lai)) {
    idl.name=merge(vertex, lai, by.x="NAME",by.y="word",all.x=T)
    idl.name<-idl.name[order(idl.name$ID),]  
    nodesAtt=data.frame(idl=idl.name$lai)
  }

  edges=as.data.frame(igraph::get.edges(gr, c(1:igraph::ecount(gr))))
  edgesWeight=igraph::edge.attributes(gr)$weight
  
  rgexf::write.gexf(vertex,edges,edgesLabel=edgesWeight,edgesWeight=edgesWeight,nodesAtt=nodesAtt,output=filename)
}
