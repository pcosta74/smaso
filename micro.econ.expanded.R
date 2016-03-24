Agent.micro.econ <- function(data, weeks, verbose=TRUE, PROD.FUN=`const.prod`, ...) {
  
  # *************************************************************
  # Read-in data
  
  quant      <- data[[QNTT]]
  prices     <- data[[PRIC]]
  beta       <- data[[BETA]]
  hist       <- data[[HIST]] 
  cons.fixed <- data[[FCON]] 
  cons.var   <- data[[VCON]] 
  prod       <- data[[PROD]]
  unit.cost  <- data[[UCST]]
  
  # Number of agents and goods
  nagents <- nrow(quant)
  ngoods <- ncol(quant)
  offset <- 0
  if(verbose) {
    cat("number of agents",nagents,"\n")
    cat("number of goods",ngoods,"\n")
  }
  
  # Maximum number of iterations
  it <- 75
  
  # historic per agent (TEMPLATE)
  t.hist.per.agent <- matrix(rep(0, nagents), 1, nagents,
                             dimnames = list(NULL, c(paste("Agent", 1:nagents, sep=' '))))
  t.hist.per.agent <- as.data.frame(t.hist.per.agent)

  # Initial quantities
  hist.quant <- t.hist.per.agent
  hist.quant[1,1:nagents] <- values.per.agent(quant)
  
  # Initial production
  hist.prod <- t.hist.per.agent
  hist.prod[1,1:nagents] <- values.per.agent(prod)
  
  # Initial prices
  hist.prices <- hist
  hist.prices[1,1:ngoods] <- prices
  
  hist.iter.prices <- hist
  hist.iter.ex.demand <- hist
  hist.iter.prices[1,1:ngoods] <- prices
  
  # Initial wealth
  wealth <- wealth(nagents,ngoods,offset,quant,prices)
  
  hist.wealth <- t.hist.per.agent
  hist.wealth[1,1:nagents] <- wealth

  # Initial utility values
  hist.utility <- t.hist.per.agent
  hist.utility[1,1:nagents] <- utility(quant, beta)
  
  ### Iterate for weeks
  for (week in 1:weeks) {
    if(verbose) {
      cat("Week no.: ",week,"\n")
    }
    
    # Call market
    new.values <- market(nagents,ngoods,offset,quant,prices,beta,hist.iter.ex.demand,hist.iter.prices,it,week)
    #  print(new.values)

    prices <- new.values[[1]]
    hist.prices <- rbind(hist.prices,prices)
    
    quant <- new.values[[2]]
    util <- utility(quant,beta)
    hist.utility <- rbind(hist.utility,util)
    
    # Initial wealth
    #  cat("Initial wealth","\n")
    #  print(round(wealth,1))
    
    # Wealth after exchange on the market
    wealth <- wealth(nagents,ngoods,offset,quant,prices)
    #  cat("Wealth after exchange on the market","\n")
    #  print(round(wealth,1))
    hist.wealth <- rbind(hist.wealth, wealth)
    
    prod <- PROD.FUN(prod, offset, quant, prices, beta, cons.fixed, cons.var, unit.cost, it, week, ...)
    cons.var <- apply(prod, 1, max) * unit.cost
    hist.prod <- rbind(hist.prod, values.per.agent(prod))
    
    quant <- quant - (cons.fixed + cons.var) + prod
    hist.quant <- rbind(hist.quant, values.per.agent(quant))

    #  cat ("Quantities after production / consumption ", "\n")
    #  print(quant)
    
    # Wealth after production / consumption
    # wealth <- wealth(nagents,ngoods,offset,quant,prices)
    #  cat("Wealth after production / consumption","\n")
    #  print(round(wealth,1))
    
  } # end iterate weeks
  
  if(verbose) {
    cat("Evolution of quantities","\n")
    print(hist.quant)
    
    cat("Evolution of production","\n")
    print(hist.prod)

    cat("Evolution of prices","\n")
    print(hist.prices)
    
    cat("Evolution of wealth of agents","\n")
    print(hist.wealth)
    
    cat("Evolution of utility of agents","\n")
    print(hist.utility)
  }

  return(list(hist.quant, hist.prod, hist.prices, hist.wealth, hist.utility))
} # end function Agent.micro.econ


# *****************************************************************
# Function market establishes new price and quantities (= desired)

market <- function(nagents, ngoods, offset, quant, prices, beta,
                   hist.iter.ex.demand, hist.iter.prices, it,
                   week, verbose=F) { 

  # Initialization of variables   			
  
  # Create a table of desired quantities filled in with 0's
  desired<- quant - quant
  
  # Write out prices that are used by the agents to optimize quantities
  # cat("Initial prices", round(prices,2),"\n") 
  
  # Initial values of excess demand (use values different from 0)
  tot.ex.demand <- rep(1,ngoods)
  
  # Iterative process  
  for (iteration in 1:it) {
    # cat("*** Iteration:", iteration, "\n") 
    
    # If total excess demand has reached 0 in previous round, terminate iterations
    if ( sum(abs(tot.ex.demand) ) < 0.2) {
      #  cat("Reached excess demand 0", "\n") & 
      break}
    
    #### AGENTES
    budget.restr <- rep(0,nagents)
    
    for (agent in 1:nagents) { 	# for each agent
      for (good in offset+1:(offset+ngoods)) { 	# for each good
        
        # Calculate budget.restr that need to be respected by each agent
        budget.restr[agent]<- sum(quant[agent,]*prices)    
        
        # Use the formula to obtain the solution that maximizes utility
        desired[agent,good]<- beta[agent,good]*budget.restr[agent]/(sum(beta[agent,])*prices[good])    
      } # agent
    } # good
    # Generate a table nagents x ngoods with ideal quantity for each good and each agent
    ex.demand<- (desired-quant) 
    
    #### LEILOEIRO
    
    # For each good
    for (good in 1:ngoods) { 
      
      # Calculate the excess demand
      tot.ex.demand[good]<-sum(ex.demand[,good])  
      
      # Adjust the prices 
      prices[good]<-prices[good]*(1+tot.ex.demand[good]/(2* sum(quant[,good]) )) 
      
    } # For each good
    
    # Communicate the values of excess demand
    # cat("Excess Demand", round(tot.ex.demand,1), "\n") 
    hist.iter.ex.demand <- rbind(hist.iter.ex.demand,tot.ex.demand)
    
    # Communicate the new prices
    # cat("New Prices", round(prices,2), "\n") 
    hist.iter.prices <- rbind(hist.iter.prices,prices)
    
  } 		# End of iterative process
  
  if (verbose && week == 1){
    # Output evolution of excess demand
    cat("Evolution of excess demand","\n")
    print(round(hist.iter.ex.demand,2))
    
    # Output evolution of prices
    cat("Evolution of prices","\n")
    print(round(hist.iter.prices,3))
    
    # Output initial and final quantities 
    cat("Initial quantities","\n") 
    print(quant)
    
    cat("Final quantities after exchange on the market","\n")
    print(round(desired,3))
  }

  new.values <- list()
  new.values[[1]] <- prices
  new.values[[2]] <- desired
  return(new.values)
  
} # End function market

# *****************************************************************
# Function wealth calculates the wealth of agents

wealth <- function(nagents, ngoods, offset, quant, prices) {
  wealth <- rep(0,nagents)
  for (agent in 1:nagents) { 	# for each agent
    for (good in offset+1:(offset+ngoods)) { 	# for each good 
      wealth[agent]<- sum(quant[agent,]*prices)  
    }}
  return(wealth)
} # End function wealth

# *****************************************************************
# Utilitu function

utility <- function(quant, beta) {
  quant[quant<0]<-0
  apply(beta*log(quant), 1, sum)
} # End function utility


# *****************************************************************
# Maximum possible production given raw material stock

max.production <- function(agent, quant, cons.fixed, unit.cost) {
  prod    <- (quant[agent,] - cons.fixed[agent,]) / unit.cost[agent,]
  prod[,] <- apply(prod, 1, function(p) max(0, min(p)))
  values.per.agent(prod, simplify = F)
} # End function max.production

# *****************************************************************
# Production strategies

# Constant production
const.prod <- function(prod, ...) {
  prod
} # Enf function const.prod 

# Alter next week's production to maximize wealth
max.wealth.prod <- function(prod, offset, quant, prices, beta, 
                            cons.fixed, cons.var, unit.cost, it, week, 
                            sector=AGRC, agent=1, ...) {
  
  # validate sector
  c<-match.call()
  tryCatch(
    match.enum(SECTORS[sector],SECTORS),
    error = function(e) {
      e$message<-sub('x','sector',e$message)
      e$call <- c
      stop(e)
    })

  nagents <- nrow(prod)
  ngoods  <- ncol(prod)
  max.prod <- max.production(1:nagents, quant, cons.fixed, unit.cost)
  exp.prod <- 0
  
  min <- 0
  max <- max(0, max.prod[agent, sector])
  
  for(iteration in 1:it) {
    # cat('.')
    quantiles <- quantile(c(min, max), probs=c(.25,0.5,.75))
    trgWealth <- sapply(quantiles, function(q) {
      prod[agent, sector] <- q
      cons.var[sector,]   <- q * unit.cost[sector,]
      quant <- quant - (cons.fixed + cons.var) + prod

      # Get new values for prices ([[1]]) and adquired quantities ([[2]])
      new.values <- market(nagents, ngoods, offset, quant, prices, beta, 
                           NULL, NULL, it, week+1,verbose=F)
      new.wealth <- wealth(nagents,ngoods,offset, 
                           new.values[[2]], new.values[[1]])

      return(new.wealth[sector])
    })
  
    exp.prod <- quantiles[2]
    if (round(min,3) == round(max,3)) {
      break
    } else if (which.max(trgWealth) == 1) {
      max <- quantiles[2]
    } else if (which.max(trgWealth) == 3) {
      min <- quantiles[2]
    } else { # which.max(trgWealth) == 2
      break
    }
  }
  # cat('\n')
  
  prod[agent, sector] <- exp.prod
  return(prod)
} # End function max.wealth.prod


# Alter next week's production to maximize profit
max.profit.prod <- function(prod, offset, quant, prices, beta, 
                            cons.fixed, cons.var, unit.cost, it, week, 
                            sector=AGRC, agent=1, ...) {
  
  # validate sector
  c<-match.call()
  tryCatch(
    match.enum(SECTORS[sector],SECTORS),
    error = function(e) {
      e$message<-sub('x','sector',e$message)
      e$call <- c
      stop(e)
    })
  
  nagents <- nrow(prod)
  ngoods  <- ncol(prod)
  max.prod <- max.production(1:nagents, quant, cons.fixed, unit.cost)
  exp.prod <- 0
  
  min <- 0
  max <- max(0, max.prod[agent, sector])
  
  for(iteration in 1:it) {
    # cat('.')
    quantiles <- quantile(c(min, max), probs=c(.25,0.5,.75))
    trgProfit <- sapply(quantiles, function(q) {

      prod[agent, sector] <- q
      cons.var[sector,]   <- q * unit.cost[sector,]
      quant <- quant - (cons.fixed + cons.var) + prod
      
      # Get new values for prices ([[1]]) and adquired quantities ([[2]])
      new.values <- market(nagents, ngoods, offset, quant, prices, beta, 
                           NULL, NULL, it, week+1,verbose=F)

      cons.total <- cons.var[agent, ] + cons.fixed[agent, ]
      unit.cost  <- sum(cons.total * prices) / prod[agent, sector]
      profit.margin <- new.values[[1]][sector] - 100 * unit.cost / new.values[[1]][sector] 
      
      return(profit.margin)
    })
    
    exp.prod <- quantiles[2]
    if (round(min,3) == round(max,3)) {
      break
    } else if (which.max(trgProfit) == 1) {
      max <- quantiles[2]
    } else if (which.max(trgProfit) == 3) {
      min <- quantiles[2]
    } else { # which.max(trgProfit) == 2
      break
    }
  }
  # cat('\n')
  
  prod[agent, sector] <- exp.prod
  return(prod)
} # End function max.wealth.prod



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

agents.in.sector <- function(sector, not. = FALSE, data = base) {
  if(!is.vector(sector) && !is.numeric(sector))
    stop('sector: wrong data type ',sQuoted(sector))
  
  # 'not.' in sector means no production (==0) 
  op<-ifelse(not.,`==`,`!=`)
  
  if(length(sector) == 1)
    return(which(op(data[[PROD]][, sector],0)))
  else {
    w <- which(op(data[[PROD]][, sector],0), arr.ind = T)
    w <- data.frame(w[,ncol(w):1])
    w$col <- SECTORS[w$col]
    names(w) <- c('sector','agent')
    return(w)
  }
}

values.per.agent <- function(x, data=base, simplify=T) {
  if(!is.data.frame(x))
    stop('Invalid data type ',sQuote(x), ': expected data.frame, found ',class(x))
  
  if(!identical(dim(x), dim(data[[ASEC]])))
    stop('Invalid size: ',sQuote(x), 
         ': expected ', dim(data[[ASEC]]), ', found ', dim(x))
  
  i.val <- as.matrix(x * (data[[ASEC]]>0))
  
  if(simplify)
    return(i.val[data[[ASEC]]])
  return(i.val)
}

# Explorar estes conceitos
# time-discount ???
# time-preferences vs discount-rate
