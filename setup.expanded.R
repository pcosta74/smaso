source('./properties.R')
source('./enums.R')
source('./utils.R')
source('./setup.R')

# Sector indexes
AGRC <- 1
CLTH <- 2
TRNS <- 3
FUEL <- HLTH <- 4
MONY <- 5

# Variable indexes
for(s in seq_along(VARIABLES))
  assign(as.character(VARIABLES[s]), as.integer(VARIABLES[s]), pos = '.GlobalEnv')

#
# Redefine setup
#
setup.new <- function(x = NULL,
                      quant.delta = 1, pref.delta = 1, 
                      prod.delta = 1, vcons.delta = 1) {
    ##
    # Parse parameters
    
    # Validade setup
    x <- match.enum(x,SETUPS)
    dims <- dim.setup(x)
    
    # Handle ratios
    params <- as.list(match.call())[-1]
    params$x <- NULL
    
    # Transform parameters
    for(param.name in names(params)) {
      param.val  <- get(param.name)
      l.par.val  <- length(param.val)
      
      if(is.vector(param.val) && l.par.val>1) {
        if(l.par.val != dims[2])
          stop(paste(param.name, 
                     'invalid vector size', sQuote(l.par.val),
                     ', expected', sQuote(dims[2])))
        assign(param.name, rep(param.val, each = dims[1]))
      }
    }
    

    if(!exists('base')) {
      # Call original setup
      setup()
      
      # Base setup
      base <<- switch(
        as.character(x),
        '4x4' = dados1,
        '12x5' = dados2
      )
      base[[UCST]] <<- base[[VCON]]/apply(base[[PROD]], 1, max)
      names(base)  <<- levels(VARIABLES)

      # clean up
      rm(dados1, pos = '.GlobalEnv')
      rm(dados2, pos = '.GlobalEnv')
    }

    # Alternative setup
    alter <<- base
    alter[[QNTT]] <<- base[[QNTT]] * quant.delta 
    alter[[BETA]] <<- base[[BETA]] * pref.delta
    alter[[VCON]] <<- base[[VCON]] * vcons.delta
    alter[[PROD]] <<- base[[PROD]] * prod.delta
}

sector.agents<-function(setup, sector, exclude.=F) {
  dims <- dim.setup(setup)
  n.sec.ag <- ceiling(dims[1]/dims[2])
  lst.ag <- c((sector-1)*n.sec.ag + c(1:n.sec.ag))
  
  if(exclude.) return(setdiff(1:dims[1],lst.ag))
  return(lst.ag)
}

# `agents<-` <- function(m,value,...) {
#   args<-list(...)
#   
#   if(args$sector && is.numeric(args$sector)) {
#     n.agents<-ceiling(nrow(m)/ncol(m))
#     sector<-as.integer(args$sector)
#     m[sector*1:n.agents,sector]<-value
#   } else {
#     m[,]<-value
#   }
#   return(m)
# }

