source(file.path('.','properties.R'))
source(file.path('.','enums.R'))
source(file.path('.','utils.R'))
source(file.path('.','setup.R'))

#
# Choose setup
#
create.base.setup <- function(x) {
  # Validade setup
  x <- match.enum(x,SETUPS)
  
  # Call original setup
  setup()

  # Choose base setup
  base <<- switch(
    as.character(x),
    '4x4' = dados1,
    '12x5' = dados2
  )
  
  # Sector indexes
  sectors <- toupper(abbreviate(names(base[[7]])))
  for(s in seq_along(sectors))
    assign(sectors[s], s, pos = '.GlobalEnv')
  
  # create enum for sectors
  assign('SECTORS', enum(sectors), pos = '.GlobalEnv')


  # Variable indexes
  for(s in seq_along(VARIABLES))
    assign(as.character(VARIABLES[s]), as.integer(VARIABLES[s]), pos = '.GlobalEnv')
  
  # Extend base scenario
  base[[UCST]] <<- base[[VCON]]/apply(base[[PROD]], 1, max)
  base[[ASEC]] <<- base[[PROD]] > 0
  
  # Change names for labels
  names(base)  <<- levels(VARIABLES)
  
  # clone base into alter
  alter <<- base
  
  # clean up
  rm(dados1, pos = '.GlobalEnv')
  rm(dados2, pos = '.GlobalEnv')
}

#
# Redefine setup
#
create.alter.setup <- function(quant.delta = 1, pref.delta = 1, prod.delta = 1, vcons.delta = 1) {

    if(!exists('base'))
      stop('No base setup, please run',sQuote('create.base.setup(x)'),'first!')
    dims <- dim(base[[PROD]])
  
    ##
    # Parse parameters
    # Handle ratios
    params <- as.list(match.call())[-1]

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

    # Alternative setup
    alter <<- base
    alter[[QNTT]] <<- base[[QNTT]] * quant.delta 
    alter[[BETA]] <<- base[[BETA]] * pref.delta
    alter[[VCON]] <<- base[[VCON]] * vcons.delta
    alter[[PROD]] <<- base[[PROD]] * prod.delta
}
