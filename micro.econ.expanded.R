source(file.path('.','tree.R'))


Agent.micro.econ <- function(data, weeks, verbose=TRUE, PROD.FUN=`const.prod`, 
                             BETA.VAR = `base.beta`, PRICE.FACTOR = c(-Inf,Inf), ...) {
  
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
  
  # Price Mínimum and Maximum
  price.limits <- list(PRICE.FACTOR[1] * prices,PRICE.FACTOR[2] * prices)  
  
  #browser()
  # Consumption of products to produce 1 unit (by sector, not by agent)
  prod.agents <- rep(0,4) #são sempre 4 bens, o MNY não conta
  for (i in 1:4) {
    prod.agents[i]<-agents.in.sector(i)[1]
  }
  cons.unit.sector <- t(sapply(prod.agents, function(x) unlist(cons.var[x,]/max(prod[x,]))))
  if (ngoods > 4) {
    cons.unit.sector <- rbind(cons.unit.sector,0)
  }
  
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
    new.values <- market(nagents,ngoods,offset,quant,prices,beta,
                         hist.iter.ex.demand,hist.iter.prices,it,week,price.limits)
    #  print(new.values)
    
    prices <- new.values[[1]]
    hist.prices <- rbind(hist.prices,prices)
    
    quant <- new.values[[2]]
    util <- utility(quant,beta)
    hist.utility <- rbind(hist.utility,util)
    
    #CHANGE PREFERENCES
    beta <- BETA.VAR(beta, data[[BETA]], data[[PRIC]], prices, 3, 1)
    #    print(beta)    
    
    # Initial wealth
    #  cat("Initial wealth","\n")
    #  print(round(wealth,1))
    
    # Wealth after exchange on the market
    wealth <- wealth(nagents,ngoods,offset,quant,prices)
    #  cat("Wealth after exchange on the market","\n")
    #  print(round(wealth,1))
    hist.wealth <- rbind(hist.wealth, wealth)
    
    prod <- PROD.FUN(prod, quant, prices, beta, cons.fixed, cons.var, unit.cost, 
                     offset, it, week, price.limits, cons.unit.sector, ...)
    
    cons.var <- t(sapply(1:nagents, function (x) colSums(t(prod)[,x]*cons.unit.sector)))
#   cons.var  <- apply(prod, 1, max) * unit.cost

#   browser()
    
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
                   week, price.min.max, verbose=F) { 
  
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
      prices[good]<-max(price.min.max[[1]][good],
                        min(prices[good]*(1+tot.ex.demand[good]/(2* sum(quant[,good]))),
                            price.min.max[[2]][good]))      
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
# Utility function

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
# Maximum possible production given raw material stock, for more than one sector

multi.max.production <- function(agent, quant, cons.fixed, cons.unit.sector, ngoods) {
  prod.vec <- rep(0,ngoods)
  for (s in 1:ngoods) {
    prod    <- (quant[agent,] - cons.fixed[agent,]) / cons.unit.sector[s,]
    prod <- max(0, min(prod))
    prod.vec[s] <- prod
  }
  return(prod.vec)
} # End function multi.max.production


# *****************************************************************
# Update preferences (betas)

# We use elipses to determine the new beta, given the new price.
# If Price < Setup price: new beta = (1/P)*((B-1) * sqrt(2*P*x - x^2) + M*P)
# Else:                   new beta = (1/(f*P))*(B*sqrt((f^2)*(P^2)-P^2+2*P*x-x^2))
#
# Where:
#   x <- new price
#   P <- initial price (defined in setup)
#   B <- initial beta (defined in setup)
#   M <- maximum preference (tipically = 1)
#   f <- multiplicative factor (if price is growing, the new beta will be 0 when price = initial price*(1+f) ) We use 5 as base case
#

update.betas <- function(beta_inic, orig_prices, prices, f=5, M=1)  {
  temp_prefs <- rep(0,4)
  max_prices <- orig_prices*(1+f)
  for (i in 1:length(orig_prices)) {
    if (prices[i]<orig_prices[i] & prices[i] > 0) {  
      temp_prefs[i] <- (1/orig_prices[i])*((beta_inic[1,i]-1) * sqrt(2*orig_prices[i]*prices[i] - prices[i]^2) + M*orig_prices[i]) ###em beta, assumimos que que são iguais para todos os consumidores; por isso podemos invocar o indice de linha 1 (agente 1)
    } else if (prices[i]>=orig_prices[i] & prices[i] < max_prices[i]){
      temp_prefs[i] <- (1/(f*orig_prices[i]))*(beta_inic[1,i]*sqrt((f^2)*(orig_prices[i]^2)-(orig_prices[i]^2)+2*orig_prices[i]*prices[i]-prices[i]^2))
    } else {
      temp_prefs[i] <- 0.001
    }
  }   
  return(temp_prefs)
}

base.beta <- function(beta, beta_inic, orig_prices, prices,f,M=1) {
  return(beta)
}

var.beta <- function(beta, beta_inic, orig_prices, prices,f,M=1) {
  new.betas <- update.betas(beta_inic, orig_prices, prices,f,M)
  for (i in 1:dim(beta)[1]) {
    beta[i,] <- new.betas
  }
  return(beta)
}


# *****************************************************************
# Production strategies

# Constant production
const.prod <- function(prod, ...) {
  prod
} # Enf function const.prod 

# Alter next week's production to maximize price
max.price.prod <- function(prod, quant, prices, beta, cons.fixed, cons.var, 
                           unit.cost, offset, it, week, price.limits, ...) {
  
  environment(max.FUN.prod) <- environment()
  max.FUN.prod(`predict.price`, ...)
  
} # End function max.price.prod

# Alter next week's production to maximize profit
max.profit.prod <- function(prod, quant, prices, beta, cons.fixed, cons.var, 
                            unit.cost, offset, it, week, price.limits, ...) {
  
  environment(max.FUN.prod) <- environment()
  max.FUN.prod(`predict.profit`, ...)
  
} # End function max.profit.prod

# Alter next week's production to maximize wealth
max.wealth.prod <- function(prod, quant, prices, beta, cons.fixed, cons.var, 
                            unit.cost, offset, it, week, price.limits, ...) {
  
  environment(max.FUN.prod) <- environment()
  max.FUN.prod(`predict.wealth`, ...)
  
} # End function max.wealth.prod

#  ///////////////////////////////////////////////////
# ///////////////////MULTIPRODUCT////////////////////
#///////////////////////////////////////////////////

# Change next week's production to maximize wealth, choosing production of 2 or more products
multi.max.wealth.prod <- function(prod, quant, prices, beta, cons.fixed, cons.var, 
                                  unit.cost, offset, it, week, price.limits, cons.unit.sector, ...) {
  
  environment(multi.max.FUN.prod) <- environment()
  multi.max.FUN.prod(`predict.wealth.multi`, ...)
  
} # End function multi.max.wealth.prod

# Change next week's production to maximize function FUN
multi.max.FUN.prod <- function(FUN, sector=AGRC, agent=1, multi.sectors = c(1,2), ...) {
  
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
#  browser()
  max.prod <- multi.max.production(agent, quant, cons.fixed, cons.unit.sector, ngoods)
  maxim <- sapply(max.prod,function(x) max(0,x))
  minim <- rep(0,ngoods)
  exp.prod <- rep(0,ngoods)
  
  #as linhas seguinte devolvem n matrizes 3x3 para cada uma das n combinações de produtos
  #a matriz indica um 1 se a combinação é possível (existem recursos) ou 0 se não for
  combs.to.test <- combn(multi.sectors,2)
  possib.matrices <- list()
  for (comb in 1:dim(combs.to.test)[2]) { #For each combination of production, get matrix of possibilities
    possib.matrices[[comb]] <- list(combs.to.test[,comb],
                                    multi.prod.mat(maxim[combs.to.test[,comb]][1], minim[combs.to.test[,comb]][1],
                                                   maxim[combs.to.test[,comb]][2], minim[combs.to.test[,comb]][2],
                                                   cons.unit.sector, quant, cons.fixed, agent, combs.to.test[,comb]))  
  }
  
  print(possib.matrices)
  #entre todas as matrizes é escolhida a melhor combinação
  #essa combinação determina o par de produtos cuja alocação de produção será apurada nas iterações seguintes
  #esta implementação só permite escolher a produção entre dois produtos, semana a semana. 
  #(ainda que a combinação de produtos possa ser diferente entre cada semana)
  
  #Abaixo é invocada a função que devolve a lista: (comb de produtos; posição na matriz; produções de cada produto; func objetivo)
  #comb de produtos = c(pA,pB) (em formato de índice)
  #posição na matriz = c(posA, posB) (em formato de índice) -> em que posição da matriz foi encontrada a melhor solução
  #produções de cada produto = c(ProdA,ProdB) (valores de produção)
  #func objectivo = valor
  
  initial.info <-get.best.from.possib.mat(possib.matrices, FUN, prod, quant, prices, beta, cons.fixed, cons.var,
                              unit.cost, offset, it, week, sector, agent, 
                              nagents, ngoods, price.limits, cons.unit.sector)
  print(initial.info)
  possib.matrices <- list()  
  for(iteration in 1:it) {
    cat('.')
    comb  <- initial.info[[1]]
    pos   <- initial.info[[2]]
    coord <- initial.info[[3]]
    val   <- initial.info[[4]]
    if (pos[1] == 2 & pos[2] == 2) break
#    browser()
    if (length(possib.matrices) != 0) {
      if(all(round(as.numeric(dimnames(possib.matrices[[1]][[2]])[[1]]),3)==round(as.numeric(dimnames(possib.matrices[[1]][[2]])[[1]][2]),3)) & 
         all(round(as.numeric(dimnames(possib.matrices[[1]][[2]])[[2]]),3)==round(as.numeric(dimnames(possib.matrices[[1]][[2]])[[2]][2]),3))) break
      }
        
    for (i in 1:2) {
      if (pos[i] == 1 | pos[i] == 2) {
        maxim[comb[i]]<-coord[i]
      } else if (pos[i]==3) {
        minim[comb[i]]<-coord[i]
      }
    }
    possib.matrices[[1]] <- list(comb, multi.prod.mat(maxim[comb[1]], minim[comb[1]], maxim[comb[2]], minim[comb[2]],
                                                   cons.unit.sector, quant, cons.fixed, agent, comb))
    print(possib.matrices)
    initial.info <- get.best.from.possib.mat(possib.matrices, FUN, prod, quant, prices, beta, cons.fixed, cons.var,
                                            unit.cost, offset, it, week, sector, agent, 
                                            nagents, ngoods, price.limits, cons.unit.sector)    
    print(initial.info)
  }
  
  cat('\n')
  
  exp.prod <- rep(0,ngoods)
  exp.prod[initial.info[[1]]]<-initial.info[[3]]
  prod[agent, ] <- exp.prod
  return(prod)
} # End function muti.max.FUN.prod

# multi.prod.mat funtion returns matrix of possible combinations of producing 2 products 
multi.prod.mat <- function(max.prod.row, min.prod.row, max.prod.col, min.prod.col, cons.unit.sector, quant, cons.fixed, agent=1, sectors = c(1,2)) {
  a <- matrix(rep(0,9),3,3)
  rownames(a)<-quantile(c(min.prod.row, max.prod.row),probs = c(.25, .5, .75))
  colnames(a)<-quantile(c(min.prod.col, max.prod.col),probs = c(.25, .5, .75))
  
  production <- rep(0,dim(quant)[2])
  
  for (i in 1:dim(a)[1]) {
    for (j in 1:dim(a)[2]) {
      production[sectors] <- c(as.numeric(rownames(a)[i]), as.numeric(colnames(a)[j]))
      a[i,j] <- all(((quant[agent,] - (cons.unit.sector[sectors[1],]*production[sectors[1]] + cons.unit.sector[sectors[2],]*production[sectors[2]])-cons.fixed[agent,]))> 0)  
    }
  }
  
  return(a)
}

get.best.from.possib.mat <- function(mat.list, FUN, prod, quant, prices, beta, cons.fixed, cons.var,
                                       unit.cost, offset, it, week, sector, agent, 
                                       nagents, ngoods, price.limits, cons.unit.sector, ...) {
  #input list of possibility matrices
  #output vector with best combination, given the maximizing FUN (comb de produtos; posição na matriz; produções de cada produto; valor da func objetivo)

  change.prod <- function(prod, agent, cols.to.change, values.to.change) {
      prod[agent,]<-c(0)
      prod[agent,cols.to.change] <- values.to.change
    return(prod)
  }
  
  best.matrices <- list()
  for (i in 1:length(mat.list)) {
    
    mat.coord <- expand.grid(dimnames(mat.list[[i]][[2]])[[1]],dimnames(mat.list[[i]][[2]])[[2]],stringsAsFactors = F)
    matrix.fun <- apply(mat.coord, 1, function(x) ifelse(mat.list[[i]][[2]][as.character(x[1]),as.character(x[2])]==1,FUN(prod=change.prod(prod, agent, mat.list[[i]][[1]],
                                                                       c(as.numeric(x[1]), as.numeric(x[2]))), 
                                                  quant, prices, beta, cons.fixed, cons.var,
                                                  unit.cost, offset, it, week, sector, agent, 
                                                  nagents, ngoods, price.limits, cons.unit.sector, ...),-Inf))
    
    matrix.fun<-matrix(matrix.fun,sqrt(length(matrix.fun)),sqrt(length(matrix.fun)),byrow = F)
    dimnames(matrix.fun)<-dimnames(mat.list[[i]][[2]])

    #saber qual o maior valor
        
    best.coord <- which(matrix.fun == max(matrix.fun), arr.ind = TRUE,useNames = F)[1,]
    best.prod <- c(as.numeric(dimnames(mat.list[[i]][[2]])[[1]][best.coord[1]]),as.numeric(dimnames(mat.list[[i]][[2]])[[2]][best.coord[2]]))
    best.matrices[[i]] <- list(mat.list[[i]][[1]], best.coord, best.prod, max(matrix.fun))
  }
  
  best.comb.value <- -Inf
  best.comb <- rep(0,ngoods)
  for (i in 1:length(best.matrices)) {
    if (best.matrices[[i]][[4]] >= best.comb.value) { #coloquei o >= porque no caso de ser 0, assigna pelo menos uma matriz
      best.comb.value <- best.matrices[[i]][[4]]
      best.comb <-best.matrices[[i]]
    }
  }
  
#  browser()
  return(best.comb)  
  
}

predict.wealth.multi <- function(prod, quant, prices, beta, cons.fixed, cons.var,
                           unit.cost, offset, it, week, sector, agent, 
                           nagents, ngoods, price.limits, cons.unit.sector, market.vals=F, ...) {
  
  cons.var[agent,]    <- colSums(t(prod)[,agent]*cons.unit.sector)
  quant <- quant - (cons.fixed + cons.var) + prod
  
#  browser()
  
  # Get new values for prices ([[1]]) and adquired quantities ([[2]])
  new.values <- market(nagents, ngoods, offset, quant, prices, beta, 
                       NULL, NULL, it, week+1,verbose=F,price.min.max=price.limits)
  new.wealth <- wealth(nagents,ngoods,offset, 
                       new.values[[2]], new.values[[1]])
  
  if(market.vals) 
    return(append(new.wealth[agent],new.values))
  return(new.wealth[agent])
} # End function predict.wealth
  
  
  
#  //////////////////////////////////////////////////
# ////////////END OF MULTIPRODUCT///////////////////
#//////////////////////////////////////////////////



# Alter next week's production to maximize function FUN
max.FUN.prod <- function(FUN, sector=AGRC, agent=1, ...) {
  
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
    quantiles <- quantile(c(min, max), probs=c(.25, .5, .75))
    trgValues <- sapply(quantiles, FUN, 
                        prod, quant, prices, beta, cons.fixed, cons.var,
                        unit.cost, offset, it, week, sector, agent, 
                        nagents, ngoods, price.limits, ...)
    
    exp.prod <- quantiles[2]
    if (round(min,3) == round(max,3)) {
      break
    } else if (which.max(trgValues) == 1) {
      max <- quantiles[2]
    } else if (which.max(trgValues) == 3) {
      min <- quantiles[2]
    } else { # which.max(trgValues) == 2
      break
    }
  }
  # cat('\n')
  
  prod[agent, sector] <- exp.prod
  return(prod)
} # End function max.FUN.prod



# Alter next week's production to maximize price
planned.price.prod <- function(prod, quant, prices, beta, cons.fixed, cons.var, 
                               unit.cost, offset, it, week, price.limits, ...) {
  
  environment(planned.FUN.prod) <- environment()
  planned.FUN.prod(`predict.price`, ...)
  
} # End function planned.price.prod

# Alter next week's production to maximize profit
planned.profit.prod <- function(prod, quant, prices, beta, cons.fixed, cons.var, 
                                unit.cost, offset, it, week, price.limits, ...) {
  
  environment(planned.FUN.prod) <- environment()
  planned.FUN.prod(`predict.profit`, ...)
  
} # End function planned.profit.prod

# Alter next week's production to maximize wealth
planned.wealth.prod <- function(prod, quant, prices, beta, cons.fixed, cons.var, 
                                unit.cost, offset, it, week, price.limits, ...) {
  
  environment(planned.FUN.prod) <- environment()
  planned.FUN.prod(`predict.wealth`, ...)
  
} # End function planned.wealth.prod


# *****************************************************************
# Plan production to maximize profit

planned.FUN.prod <- function(FUN, sector=AGRC, agent=1, 
                             prod.incr=0.10, periods=10, ...) {

  #force integer numbers
  period.starts <- round(quantile(1:(WEEKS+1), probs=seq(0,1,1/periods)))
  percent.probs <- seq(0,1,prod.incr)
  
  # Forget previous plans (if any)
  if(week == 1) plan <<- NULL
  
  # locate week in periods
  no.weeks <- sum(week >= period.starts)
  no.weeks <- period.starts[no.weeks+1]-period.starts[no.weeks]
  
  if(week %in% period.starts) {
    
    # create plan

    children <- length(percent.probs)

    plan.tree <- tree.new(no.weeks+1, children)
    plan.tree[[1]] <- list(VAR=0, PROD=prod[agent, sector], QNTT=quant, 
                           VCON=cons.var[agent,])
    cat(paste('planning at week #', week, 'for the next',no.weeks,'weeks\n'))
    
    nagents <- nrow(prod)
    ngoods  <- ncol(prod)
    # populate tree with predictions
    goal   <- tree.size(NULL, no.weeks, children)
    open   <- c(1)
    closed <- c()
    
    while(length(open)) {
      # BREADTH-FIRST vs DEPTH-FIRST
      current <- ifelse(TRUE, open[1], open[length(open)])
      children <- tree.node.children(plan.tree, current, index.=T)

      cur.quant <- plan.tree[[current]]$QNTT
      cons.var[agent,]  <- plan.tree[[current]]$VCON
      prod[agent, sector] <- plan.tree[[current]]$PROD
      max.prod <- max.production(1:nagents, cur.quant, cons.fixed, unit.cost)
    
      percentiles <- quantile(c(0,max.prod[agent,sector]), percent.probs)
      tree.node.children(plan.tree, current) <- sapply(percentiles, function(p) {
        # call profit prediction function
        pred <- FUN(p, prod, cur.quant, prices, beta, cons.fixed, 
                    cons.var, unit.cost, offset, it, week, sector, 
                    agent, nagents, ngoods, price.limits, market.vals=T, ...)
        return(list(list(VAR=pred[[1]], PROD=p, QNTT=pred[[3]], 
                         VCON=p*unit.cost[agent,])))
      })
      
      if(current == goal) break
      open   <- append(open[open != current], children)
      closed <- append(closed, current)
    }
    
    # assess best path
    new.prod <- sapply(tree.leaves(plan.tree), function(x) { x$VAR })
    node4max <- as.integer(names(which.max(new.prod)))
    
    # store plan
    plan <<- sapply(tree.path(plan.tree, 1, node4max), function(x,s,a) { x$PROD })
  }
  
  # update week to week of plan
  week <- (week%%no.weeks) + no.weeks*(!week%%no.weeks)
  
  # setup planned production for next week
  prod[agent, sector] <- plan[week + 1]
  return(prod)
} # End planned.profit.prod


#
predict.price <- function(q, prod, quant, prices, beta, cons.fixed, cons.var,
                          unit.cost, offset, it, week, sector, agent, 
                          nagents, ngoods, price.limits, market.vals=F, ...) {
  
  prod[agent, sector] <- q
  cons.var[agent,]   <- q * unit.cost[agent,]
  quant <- quant - (cons.fixed + cons.var) + prod
  
  # Get new values for prices ([[1]]) and adquired quantities ([[2]])
  new.values <- market(nagents, ngoods, offset, quant, prices, beta, 
                       NULL, NULL, it, week+1,verbose=F,price.min.max=price.limits)
  
  if(market.vals) 
    return(append(new.values[[1]][sector], new.values))
  return(new.values[[1]][sector])
} # End function predict.price

#
predict.profit <-function(q, prod, quant, prices, beta, cons.fixed, cons.var,
                          unit.cost, offset, it, week, sector, agent, 
                          nagents, ngoods, price.limits, market.vals=F, ...) {
  
  prod[agent, sector] <- q
  cons.var[agent,]    <- q * unit.cost[agent,]
  quant <- quant - (cons.fixed + cons.var) + prod
  
  # Get new values for prices ([[1]]) and adquired quantities ([[2]])
  new.values <- market(nagents, ngoods, offset, quant, prices, beta, 
                       NULL, NULL, it, week+1,verbose=F,price.min.max=price.limits)
  
  cons.total <- cons.var[agent, ] + cons.fixed[agent, ]
  mon.unit.cost <- sum(cons.total * prices) / prod[agent, sector]
  profit.margin <- new.values[[1]][sector] - 100 * mon.unit.cost / new.values[[1]][sector] 
  
  if(market.vals) 
    return(append(profit.margin,new.values))
  return(profit.margin)
} # End function predict.profit

#
predict.wealth <- function(q, prod, quant, prices, beta, cons.fixed, cons.var,
                           unit.cost, offset, it, week, sector, agent, 
                           nagents, ngoods, price.limits, market.vals=F, ...) {
  
  prod[agent, sector] <- q
  cons.var[agent,]    <- q * unit.cost[agent,]
  quant <- quant - (cons.fixed + cons.var) + prod
  
  # Get new values for prices ([[1]]) and adquired quantities ([[2]])
  new.values <- market(nagents, ngoods, offset, quant, prices, beta, 
                       NULL, NULL, it, week+1,verbose=F,price.min.max=price.limits)
  new.wealth <- wealth(nagents,ngoods,offset, 
                       new.values[[2]], new.values[[1]])
  
  if(market.vals) 
    return(append(new.wealth[agent],new.values))
  return(new.wealth[agent])
} # End function predict.wealth


# *****************************************************************
# Agents per sector

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

# *****************************************************************
# Fetch values per agent from dadosX matrices
# Uses bitmask initialized from original production

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
} # End function values.per.agent
