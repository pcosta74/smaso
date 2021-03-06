Agent.micro.econ <- function(dados,weeks) {
  # *************************************************************
  # Read-in data
  
  quant <- dados[[1]]
  prices <- dados[[2]]
  beta <- dados[[3]]
  hist <- dados[[4]]
  cons.fixed <- dados[[5]]
  cons.var <- dados[[6]]
  prod <- dados[[7]]
  
  # Number of agents and goods
  nagents <- nrow(quant)
  ngoods <- ncol(quant)
  offset <- 0
  cat("number of agents",nagents,"\n")
  cat("number of goods",ngoods,"\n")
  
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
  
  ### Iterate for weeks
  for (week in 1:weeks) {
    cat("Week no.: ",week,"\n")
    
    # Call market
    new.values <- market(nagents,ngoods,offset,quant,
                         prices,beta,hist.iter.ex.demand,
                         hist.iter.prices,it,week)
    #  print(new.values)
    
    prices <- new.values[[1]]
    hist.prices <- rbind(hist.prices,prices)
    
    quant <- new.values[[2]]
    
    # Initial wealth
    #  cat("Initial wealth","\n")
    #  print(round(wealth,1))
    
    # Wealth after exchange on the market
    wealth <- wealth(nagents,ngoods,offset,quant,prices)
    #  cat("Wealth after exchange on the market","\n")
    #  print(round(wealth,1))
    hist.wealth <- rbind(hist.wealth, wealth)
    
    quant <- quant - (cons.fixed + cons.var) + prod
    
    #  cat ("Quantities after production / consumption ", "\n")
    #  print(quant)
    
    # Wealth after production / consumption
    # wealth <- wealth(nagents,ngoods,offset,quant,prices)
    #  cat("Wealth after production / consumption","\n")
    #  print(round(wealth,1))
    
  } # end iterate weeks
  cat("Evolution of prices","\n")
  print(hist.prices)
  
  cat("Evolution of wealth of agents","\n")
  print(hist.wealth)

} # end function Agent.micro.econ


# *****************************************************************
# Function market establishes new price and quantities (= desired)

market <- function(nagents,ngoods,offset,quant,prices,beta,hist.iter.ex.demand,hist.iter.prices,it,week) {
    # Initialization of variables
    
    # Create a table of desired quantities filled in with 0?s
    desired <- quant - quant
    
    # Write out prices that are used by the agents to optimize quantities
    # cat("Initial prices", round(prices,2),"\n")
    
    # Initial values of excess demand (use values different from 0)
    tot.ex.demand <- rep(1,ngoods)
    
    # Iterative process
    for (iteration in 1:it) {
      # cat("*** Iteration:", iteration, "\n")
      
      # If total excess demand has reached 0 in previous round, terminate iterations
      if (sum(abs(tot.ex.demand)) < 0.2) {
        #  cat("Reached excess demand 0", "\n") &
        break
      }
      
      #### AGENTES
      budget.restr <- rep(0,nagents)
      
      for (agent in 1:nagents) {
        # for each agent
        for (good in offset + 1:(offset + ngoods)) {
          # for each good
          
          # Calculate budget.restr that need to be respected by each agent
          budget.restr[agent] <- sum(quant[agent,] * prices)
          
          # Use the formula to obtain the solution that maximizes utility
          desired[agent,good] <-
            beta[agent,good] * budget.restr[agent] / (sum(beta[agent,]) * prices[good])
        } # agent
      } # good
      # Generate a table nagents x ngoods with ideal quantity for each good and each agent
      ex.demand <- (desired - quant)
      
      #### LEILOEIRO
      
      # For each good
      for (good in 1:ngoods) {
        # Calculate the excess demand
        tot.ex.demand[good] <- sum(ex.demand[,good])
        
        # Adjust the prices
        prices[good] <-
          prices[good] * (1 + tot.ex.demand[good] / (2 * sum(quant[,good])))
        
      } # For each good
      
      # Communicate the values of excess demand
      # cat("Excess Demand", round(tot.ex.demand,1), "\n")
      hist.iter.ex.demand <- rbind(hist.iter.ex.demand,tot.ex.demand)
      
      # Communicate the new prices
      # cat("New Prices", round(prices,2), "\n")
      hist.iter.prices <- rbind(hist.iter.prices,prices)
      
    } 		# End of iterative process
    
    if (week == 1) {
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
  for (agent in 1:nagents) {
    # for each agent
    for (good in offset + 1:(offset + ngoods)) {
      # for each good
      wealth[agent] <- sum(quant[agent,] * prices)
    }
  }
  return(wealth)
}
