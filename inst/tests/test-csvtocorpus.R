corpus.from.csv<-sfWideCsvToCorpus("../extdata/test_csv.csv")
corpus.from.csv<-corpus.from.csv[order(corpus.from.csv$order, corpus.from.csv$id),]
expect_that(nrow(corpus.from.csv),equals(11))
expect_that(as.character(corpus.from.csv$coi),equals(rep("_ci_",11)))
expect_that(corpus.from.csv$id,equals(c(1:4,1:4,1:3)))
expect_that(corpus.from.csv$order,equals(c(1,1,1,1,2,2,2,2,3,3,3)))
expect_that(as.character(corpus.from.csv$word),equals(c("a","b","e","f","b","c","f","g","c","d","g")))
