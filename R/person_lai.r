#' Calculates mean LAI for every person
#' In raw and normalized version
#' @param x Corpus 
#' @return data.frame with raw and normalized mean LAI for every person
personalLAI<-function(x,...) {
  l <-LAI(x,...)
  
  lai.s<-sort(l)
  lai.f<-length(lai.s)
  n.p<-sort(unique(x$answers.per.subject$n))
  
  lai.min.max<-data.frame(n=n.p,t(sapply(n.p,function(x) {
    c(
      min=mean(lai.s[1:x]),
      max=mean(lai.s[(lai.f-x):lai.f])
    )
  })))
  
  l2<-data.frame(word=names(l),lai=as.numeric(l))
  res<-merge(x$corpus,l2,by="word")
  raw.pl<-aggregate(res$lai,list(person=res$id),mean)
  # Unimos la cantidad de respuesta con el min-max
  
  min.max.pl<-merge(x$answers.per.subject, lai.min.max , by.x="n" , by.y="n")
  min.max.pl.id<-merge(min.max.pl,raw.pl,by.x="id",by.y="person")
  norm.idl<-(min.max.pl.id$x-min.max.pl.id$min)/(min.max.pl.id$max-min.max.pl.id$min)
  data.frame(min.max.pl.id[,c("id","n","min","max")],personal.lai=min.max.pl.id$x, normalized.personal.lai=norm.idl)
}
