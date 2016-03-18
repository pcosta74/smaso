source('./properties.R')

#
# Redefine setup
#
setup.new <-
  function(scenario = NULL, ratio.delta = 1, prod.delta = 1, quant.delta = 1, pref.delta = 1) {
    
    ##
    # Parse parameters
    
    # Validade scenario
    scenario <- tryCatch(
      as.character(match.arg(scenario, levels(SCENARIOS))),
      error = function(e) stop('Invalid scenario ', sQuote(scenario), call.=F)
    )
    
    # Enforce exclusive ratios
    param <- as.list(match.call())[-1]
    param$scenario <- NULL
    
    switch(
      as.character(length(param)),
      '0' = {
        # NOP
      }, 
      '1' = {
         # Transform parameter
         param.name <- names(param)
         param.val  <- get(param.name)
         l.par.val  <- length(param.val)
         
         if(is.vector(param.val) && l.par.val>1) {
           dims <- as.integer(unlist(strsplit(scenario,"x")))
           if(l.par.val != dims[2])
            stop(paste(param.name, 
                       'invalid vector size', sQuote(l.par.val),
                       ', expected', sQuote(dims[2])))
           assign(param.name, rep(param.val, each = dims[1]))
         }
      },
      stop('Several parameters: ',
           paste(names(param), param, sep = '=', collapse = ','))
    )

    
    # Call original setup function
    source('./setup.R', local = T)
    setup()
    
    # Base scenario
    base<<- switch(
      scenario,
      '4x4' = dados1,
      '12x5' = dados2
    )
    
    # Alternative scenario
    alter <<- base
    alter[[1]] <<- base[[1]] * quant.delta 
    alter[[3]] <<- base[[3]] * pref.delta
    alter[[6]] <<- base[[6]] * ratio.delta
    alter[[7]] <<- base[[7]] * ratio.delta * prod.delta
    
    # clean up
    rm(dados1, pos = ".GlobalEnv")
    rm(dados2, pos = ".GlobalEnv")
  }

