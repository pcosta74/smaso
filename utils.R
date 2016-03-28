camel.case<-function(x=as.character()) {
  gsub('(\\w)(\\w*)', '\\U\\1\\L\\2', as.character(x), perl=TRUE)
}

enum <- function(x=c()) {
  c<-match.call()
  tryCatch({
    e <- ordered(1:length(as.vector(x)), labels=as.vector(x))
    class(e)<-c(class(e),'enum')
    return(e)
  },
  error = function(e) {
    e$call<-c
    e$message<-sub('labels','x',e$message)
    stop(e)
  })  
}

as.enum <- function(x) {
  enum(x)
}

is.enum <- function(x) {
  'enum' %in% class(x)
}

match.enum <- function(x , enum) {
  c<-match.call()
  tryCatch({
    if(!is.null(x)) x<- as.character(x)
    match.arg(x,enum)
  },
  error = function(e) {
    e$call<-c
    e$message<-sub('arg','x',e$message)
    stop(e)
  })
}
