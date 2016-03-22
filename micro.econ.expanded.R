Agent.micro.econ <- function(dados,weeks,verbose=TRUE,PROD.FUN=`const.prod`) {
  
  # *************************************************************
  # Read-in data
  
  quant <- dados[[QNTT]]
  prices <- dados[[PRIC]]
  beta <- dados[[BETA]]
  hist <- dados[[HIST]] 
  cons.fixed <- dados[[FCON]] 
  cons.var <- dados[[VCON]] 
  prod <- dados[[PROD]]
  unit.cost <- dados[[UCST]]
  
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
  
  hist.prices <- hist
  hist.prices[1,1:ngoods] <- prices
  
  hist.iter.prices <- hist
  hist.iter.ex.demand <- hist
  hist.iter.prices[1,1:ngoods] <- prices
  
  # Initial wealth
  wealth <- wealth(nagents,ngoods,offset,quant,prices)
  
  hist.wealth <- matrix(rep(0,nagents),1,nagents)
  hist.wealth <- as.data.frame(hist.wealth)
  hist.wealth[1,1:nagents] <- wealth
  names(hist.wealth)<-paste("Agent",1:nagents,sep=' ')

  hist.utility <- hist.wealth
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
    
    prod <- PROD.FUN(prod, offset, quant, prices, beta, cons.fixed, cons.var, unit.cost, it, week)
    cons.var <- apply(prod,1,max) * unit.cost
    quant <- quant - (cons.fixed + cons.var) + prod

    #  cat ("Quantities after production / consumption ", "\n")
    #  print(quant)
    
    # Wealth after production / consumption
    # wealth <- wealth(nagents,ngoods,offset,quant,prices)
    #  cat("Wealth after production / consumption","\n")
    #  print(round(wealth,1))
    
  } # end iterate weeks
  
  if(verbose) {
    cat("Evolution of prices","\n")
    print(hist.prices)
    
    cat("Evolution of wealth of agents","\n")
    print(hist.wealth)
    
    cat("Evolution of utility of agents","\n")
    print(hist.utility)
  }

  return(list(hist.prices, hist.wealth, hist.utility))
} # end function Agent.micro.econ


# *****************************************************************
# Function market establishes new price and quantities (= desired)

market <- function(nagents,ngoods,offset,quant,prices,beta,
                   hist.iter.ex.demand,hist.iter.prices,it,week,
                   verbose=F) { 
  
  
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

wealth <- function(nagents,ngoods,offset,quant,prices) {
  wealth <- rep(0,nagents)
  for (agent in 1:nagents) { 	# for each agent
    for (good in offset+1:(offset+ngoods)) { 	# for each good 
      wealth[agent]<- sum(quant[agent,]*prices)  
    }}
  return(wealth)
}

utility <- function(quant,beta) {
  quant[quant<0]<-0
  apply(beta*log(quant),1,sum)
}

max.production <- function(nagents, quant, cons.fixed, unit.cost) {
  prod <- (quant[nagents,]-cons.fixed[nagents,])/unit.cost[nagents,]
  diag(apply(prod,1,min))
}

const.prod <- function(prod, ...) {
  prod
}

max.wealth.prod <- function(prod, offset, quant, prices, beta, 
                            cons.fixed, cons.var, unit.cost, it, week, sector=AGRC) {
  nagents <- nrow(prod)
  ngoods  <- ncol(prod)
  max.prod <- max.production(1:nagents, quant, cons.fixed, unit.cost)

  min <- 0
  max <- max.prod[sector, sector]
  quit <- FALSE
     
  expected.production <- 0
     
  while(!quit) {
    quarts  <- quantile(c(min, max), probs=c(.25,0.5,.75))
    wealths <- sapply(quarts, function(q) {
      prod[sector, sector] <- q
      cons.var[sector,] <- q * unit.cost[sector,]
      
      quant <- quant - (cons.fixed + cons.var) + prod
      new.values <- market(nagents, ngoods, offset, quant, prices, beta, 
                           NULL, NULL, it, week+1)
      new.prices <- new.values[[1]]
      new.quant  <- new.values[[2]]
      new.wealth <- wealth(nagents,ngoods,offset, new.quant, new.prices)
      return(new.wealth[sector])
    })
  
    if (round(min,2) == round(max,2)) {
      quit <- TRUE
    } else if (which.max(wealths) == 1) {
      max <- quarts[2]
    } else if (which.max(wealths) == 3) {
      min <- quarts[2]
    } else { # which.max(wealths) == 2
      quit <- TRUE
    }
    
    expected.production <- quarts[2]
  }
  
  prod[sector, sector] <- expected.production
  return(prod)
}