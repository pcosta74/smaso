source('./properties.R')
source('./enums.R')
source('./utils.R')
source('./setup.R')

# Sector constants
AGRC <- 1
CLTH <- 2
TRNS <- 3
FUEL <- 4
HLTH <- 4
MONY <- 5

# Pages
QUANT  <- 1
PRICE  <- 2
BETA   <- 3
HIST   <- 4
FCONS  <- 5
VCONS  <- 6
PROD   <- 7

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
      names(base)<<-levels(PAGES)

      # clean up
      rm(dados1, pos = ".GlobalEnv")
      rm(dados2, pos = ".GlobalEnv")
    }

    # Alternative setup
    alter <<- base
    alter[[QUANT]] <<- base[[QUANT]] * quant.delta 
    alter[[PRICE]] <<- base[[PRICE]] * pref.delta
    alter[[VCONS]] <<- base[[VCONS]] * vcons.delta
    alter[[PROD]]  <<- base[[PROD]] * prod.delta
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

