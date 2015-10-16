#' Clean raw database, before generate a corpus 
#' @export
cleanRawCorpus<-function(x) {
  x<-x[order(x$id,x$order),]
  ids=unique(x$id)
  out<-x[0,]
  for(id in ids) {
    x.a<-x[x$id==id,]
    if(ad<-anyDuplicated(x.a$word)) { 
      
      x.a<-x.a[!duplicated(x.a$word),-which(colnames(x.a) %in% "order")]
      
    }
    x.a$order<-1:nrow(x.a)
    out<-rbind(out,x.a)
  }
  out
}
