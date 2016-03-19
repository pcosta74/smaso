camel.case<-function(x=as.character()) {
  gsub('(\\w)(\\w*)', '\\U\\1\\L\\2', as.character(x), perl=TRUE)
}

dim.setup<-function(x) {
  as.integer(unlist(strsplit(x,"x")))
}