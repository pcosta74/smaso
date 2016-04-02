source(file.path('.','utils.R'))

SETUPS <- enum('4x4','12x5')
VARIABLES <- enum('QNTT','PRIC','BETA','HIST','FCON','VCON','PROD','UCST','ASEC')
INDICATORS <- enum('quantities','production','prices','wealth','utility')

dim.setup<-function(x) {
  s<-as.character(match.enum(x,SETUPS))
  as.integer(unlist(strsplit(s,"x")))
}