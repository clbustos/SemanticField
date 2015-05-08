#' Transform wide representation (each line a set of responses)
#' on a csv file to corpus object representation
#' @export
#' @param filename CSV filename
#' @param center of interest for the corpus
#' @param other.vars set other variables, as a list or data.frame
#' @return a corpus object
#' @import reshape
sfWideCsvToCorpus<-function(filename,center_of_interest="_ci_",other.vars=data.frame()) {
   
   cc<-paste0("V",1:200)
   x<-read.csv(filename,header=F,col.names=cc)
   is.na(x)<-x==""
   x$id<-seq(1,nrow(x))
   rr<-reshape::melt(x,id.vars="id",measure.vars=cc,na.rm=T,variable_name="order")
   rr$order<-as.numeric(gsub("V","",rr$order))
   out<-data.frame(coi=center_of_interest,other.vars,rr)
   class(out)<-c("sfCorpusWide",class(out))
   out
}
