setup <- function() {

# Set-up with 4 agents and 4 goods

# Initial quatities
  quant <- data.frame(
    Agriculture=c(420,210,210,210),
    Clothing=c(1.154,2.308,1.154,1.154),
    Transportation=c(0.03846,0.03846,0.07692,0.03846),
    Fuel=c(2.308,2.308,2.308,4.616)
  )

# Vector of initial prices for all goods
  prices <- c(3,50,10000,50)

# Preferences for goods 
  beta <- data.frame(
    Agriculture=rep(0.5,4),
    Clothing=rep(0.05,4),
    Transportation=rep(0.3,4),
    Fuel=rep(0.15,4)
  )

# Initial history (empty data frame to be filled in)
  hist <- data.frame(
    Agriculture=c(0),
    Clothing=c(0),
    Transportation=c(0),
    Fuel=c(0)
  )

# Fixed consumption
  cons.fixed <- data.frame(
    Agriculture=rep(10.5,4),
    Clothing=rep(0.0577,4),
    Transportation=rep(0.001923,4),
    Fuel=rep(0.1154,4)
  )

# Variable consumption
  cons.var <- data.frame(
    Agriculture=rep(10.5,4),
    Clothing=rep(0.0577,4),
    Transportation=rep(0.001923,4),
    Fuel=rep(0.1154,4)
  )

# Production
  prod <- data.frame(
    Agriculture=c(84,0,0,0),
    Clothing=c(0,0.4616,0,0),
    Transportation=c(0,0, 0.015384, 0),
      Fuel=c(0,0,0,0.9232)
  )

  dados1 <<- list()
  dados1[[1]] <<- quant
  dados1[[2]] <<- prices
  dados1[[3]] <<- beta
  dados1[[4]] <<- hist
  dados1[[5]] <<- cons.fixed
  dados1[[6]] <<- cons.var 
  dados1[[7]] <<- prod

# Set-up with 12 agents and 5 goods

# Initial quatities
  quant <- data.frame(
    Agriculture=c(rep(420,3),rep(168,9)),
    Clothing=c(rep(0.9232,3),rep(2.308,3),rep(0.9232,6)),
    Transportation=c(rep(0.030768,6),rep(0.7692,3),rep(0.030768,3)),
    Health=c(rep(11.04,9),rep(27.6,3)),
    Money=rep(200,12)
  )

# Vector of initial prices for all goods
  prices <- c(3,50,10000,10,1)

# Preferences for goods 
  beta <- data.frame(
    Agriculture=rep(0.32,12),
    Clothing=rep(0.03,12),
    Transportation=rep(0.2,12),
    Health=rep(0.2,12), 
    Money=rep(0.13,12)
  )

# Initial history (empty data frame to be filled in)
  hist <- data.frame(
    Agriculture=c(0),
    Clothing=c(0),
    Transportation=c(0),
    Health=c(0),
    Money=c(0)
  )

# Preferences for goods 
  cons.fixed <- data.frame(
    Agriculture=rep(10.5,12),
    Clothing=rep(0.0577,12),
    Transportation=rep(0.001923),
    Health=rep(0.69,12),
    Money=rep(0,12)
  )

  cons.var <- data.frame(
    Agriculture=rep(10.5,12),
    Clothing=rep(0.0577,12),
    Transportation=rep(0.001923),
    Health=rep(0.69,12),
    Money=rep(0,12)
  )

# Production
  prod <- data.frame(
    Agriculture=c(rep(84,3),rep(0,9)),
    Clothing=c(rep(0,3),rep(0.4616,3),rep(0,6)),
    Transportation=c(rep(0,6),rep(0.015384,3),rep(0,3)),
    Health=c(rep(0,9),rep(5.52,3)),
    Money=rep(0,12)
  )

  dados2 <<- list()
  dados2[[1]] <<- quant
  dados2[[2]] <<- prices
  dados2[[3]] <<- beta
  dados2[[4]] <<- hist
  dados2[[5]] <<- cons.fixed
  dados2[[6]] <<- cons.var
  dados2[[7]] <<- prod
}

