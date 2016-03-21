source('./utils.R')

SETUPS <- enum(c('4x4','12x5'))
INDICATORS <- enum(c('prices','wealth','utility'))

dim.setup<-function(x) {
  s<-as.character(match.enum(x,SETUPS))
  as.integer(unlist(strsplit(s,"x")))
}