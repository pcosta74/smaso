source('./properties.R')
source('./utils.R')

#
# Redefine setup
#
setup.new <- function(x = NULL, 
                      quant.delta = 1, pref.delta = 1, 
                      prod.delta = 1, vcons.delta = 1) {
    
    ##
    # Parse parameters
    
    # Validade setup
    x <- tryCatch(
      as.character(match.arg(x, levels(SETUPS))),
      error = function(e) stop('Invalid setup ', sQuote(x), call.=F)
    )
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
    
    # Call original setup function
    source('./setup.R', local = T)
    setup()
    
    # Base setup
    base<<- switch(x,
      '4x4' = dados1,
      '12x5' = dados2
    )
    names(base)<<-c('QUANT','PRICES','BETA','HIST',
                   'F.CONS','V.CONS','PROD')
    
    # Alternative setup
    alter <<- base
    alter[[1]] <<- base[[1]] * quant.delta 
    alter[[3]] <<- base[[3]] * pref.delta
    alter[[6]] <<- base[[6]] * vcons.delta
    alter[[7]] <<- base[[7]] * prod.delta
    
    # clean up
    rm(dados1, pos = ".GlobalEnv")
    rm(dados2, pos = ".GlobalEnv")
  }

