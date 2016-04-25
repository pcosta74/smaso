source(file.path('.','properties.R'))
source(file.path('.','setup.expanded.R'))
source(file.path('.','micro.econ.expanded.R'))
source(file.path('.','plot.expanded.R'))

# simulation properties

WEEKS <- 16
SETUP <- '4x4'

TESTS <- c(
  #'ALTR.RTIO', 
  #'DBLE.PROD', 
  #'BLNC.MARK', 
  #'SAME.QNTT', 
  #'ALTR.PREF',
  #'MXMZ.WLTH', 
  #'MXMZ.PRFT', 
  #'MXMZ.PRIC',
  'PLAN.WLTH',
  #'PLAN.PRFT',
  #'MXMZ.WLTH.PREF.VAR',
  #'MXMZ.WLTH.REG.PRICES'
  #'MXMZ.WLTH.MULTI.PROD',
  NULL
)



# Create base setup
create.base.setup(SETUP)

#
plot.files <- FALSE
verbose <- TRUE

#RUNNING AGENT#
dim <- dim.setup(SETUP)

# 1: Altered ratio
if ('ALTR.RTIO' %in% TESTS) {
  sector <- CLTH
  agents <- agents.in.sector(sector)
  
  p.mtx <- matrix(1,dim[1],dim[2])
  p.mtx[agents, sector] <- 1.5
  
  v.mtx <- matrix(1,dim[1],dim[2])
  v.mtx[agents,] <- 1.5
  
  create.alter.setup(prod.delta = p.mtx, vcons.delta = v.mtx)
  bs <- Agent.micro.econ(base, WEEKS, verbose)
  as <- Agent.micro.econ(alter,WEEKS, verbose)
  plot.scenarios(WEEKS, bs, as, SETUP, "altered ratio", plot.files)
}

# 2: Productivity
if ('DBLE.PROD' %in% TESTS) {
  sector <- CLTH
  agents <- agents.in.sector(sector)
  
  #p.mtx <- matrix(1,dim[1],dim[2])
  #p.mtx[agents, sector] <- 1.5
  
  v.mtx <- matrix(1,dim[1],dim[2])  #TORNAR O CUSTO METADE, EM VEZ DE DUPLICAR A PRODUÇÃO
  v.mtx[agents,] <- 0.5             
  
  create.alter.setup(vcons.delta = v.mtx)
  bs <- Agent.micro.econ(base, WEEKS, verbose)
  as <- Agent.micro.econ(alter,WEEKS, verbose)
  
  plot.scenarios(WEEKS, bs, as, SETUP, "doubled productivity", plot.files)
}

# 3: Balanced markets
if ('BLNC.MARK' %in% TESTS) {
  dlt <- 0.0384
  sector <- CLTH
  agents <- agents.in.sector(sector)
  others <- agents.in.sector(sector, not. = T)
  
  create.alter.setup()
  alter[[PROD]][agents,sector] <- alter[[PROD]][agents,sector] + dlt
  alter[[VCON]][others,sector] <- alter[[VCON]][others,sector] + dlt/3
  bs <- Agent.micro.econ(base, WEEKS, verbose)
  as <- Agent.micro.econ(alter,WEEKS, verbose)
  
  plot.scenarios(WEEKS, bs, as, SETUP, "balanced market", plot.files)
}

# 4: Same quantitities for all agents
if ('SAME.QNTT' %in% TESTS) {
  sector <- CLTH
  agents <- agents.in.sector(sector)
  
  q.mtx <- matrix(1,dim[1],dim[2])
  q.mtx[agents, sector] <- 0.5
  
  create.alter.setup(quant.delta = q.mtx)
  bs <- Agent.micro.econ(base, WEEKS, verbose)
  as <- Agent.micro.econ(alter,WEEKS, verbose)
  
  plot.scenarios(WEEKS, bs, as, SETUP, "same quantity", plot.files)
}

# 5: Altered preferences
if ('ALTR.PREF' %in% TESTS) {
  p.mtx <- matrix(1,dim[1],dim[2])
  p.mtx[,AGRC] <- 0.7 # 0.35
  p.mtx[,CLTH] <- 4   # 0.20
  
  create.alter.setup(pref.delta = p.mtx)
  bs <- Agent.micro.econ(base, WEEKS, verbose)
  as <- Agent.micro.econ(alter,WEEKS, verbose)
  
  plot.scenarios(WEEKS, bs, as, SETUP, "altered preferences", plot.files)
}

# 6: Maximize wealth
if ('MXMZ.WLTH' %in% TESTS) {
  sector <- CLTH
  agents <- agents.in.sector(sector)[1]
  
  create.alter.setup()
  
  bs <- Agent.micro.econ(base, WEEKS, verbose)
  as <- Agent.micro.econ(alter,WEEKS, verbose, PROD.FUN=`max.wealth.prod`, sector=sector, agent=agents)
  plot.scenarios(WEEKS, bs, as, SETUP, "maximize wealth", plot.files)  
}

# 7: Maximize profit
if ('MXMZ.PRFT' %in% TESTS) {
  sector <- CLTH
  agents <- agents.in.sector(sector)[1]
  
  create.alter.setup()
  
  bs <- Agent.micro.econ(base, WEEKS, verbose)
  as <- Agent.micro.econ(alter,WEEKS, verbose, PROD.FUN=`max.profit.prod`, sector=sector, agent=agents)
  
  plot.scenarios(WEEKS, bs, as, SETUP, "maximize profit", plot.files)  
}

# 8: Maximize price
if ('MXMZ.PRIC' %in% TESTS) {
  sector <- CLTH
  agents <- agents.in.sector(sector)[1]
  
  create.alter.setup()
  
  bs <- Agent.micro.econ(base, WEEKS, verbose)
  as <- Agent.micro.econ(alter,WEEKS, verbose, PROD.FUN=`max.price.prod`, sector=sector, agent=agents)
  
  plot.scenarios(WEEKS, bs, as, SETUP, "maximize price", plot.files)  
}

# 9: Plan production to maximize wealth
if ('PLAN.WLTH' %in% TESTS) {
  sector <- CLTH
  agents <- agents.in.sector(sector)[1]
  
  create.alter.setup()
  
  bs <- Agent.micro.econ(base, WEEKS, verbose)
  as <- Agent.micro.econ(alter,WEEKS, verbose, PROD.FUN=`planned.wealth.prod`,
                         sector=sector, agent=agents, prod.incr=0.1, periods=4)
  
  plot.scenarios(WEEKS, bs, as, SETUP, "planned wealth", plot.files)  
}

# 10: Plan production to maximize profit
if ('PLAN.PRFT' %in% TESTS) {
  sector <- CLTH
  agents <- agents.in.sector(sector)[1]
  
  create.alter.setup()
  
  bs <- Agent.micro.econ(base, WEEKS, verbose)
  as <- Agent.micro.econ(alter,WEEKS, verbose, PROD.FUN=`planned.profit.prod`,
                         sector=sector, agent=agents, prod.incr=0.1, periods=4)
  
  plot.scenarios(WEEKS, bs, as, SETUP, "planned profit", plot.files)  
}


# 11: Maximize wealth with variable preferences
if ('MXMZ.WLTH.PREF.VAR' %in% TESTS) {
  sector <- CLTH #CLTH #AGRC #TRNS #FUEL #HLTH #MNY
  agents <- agents.in.sector(sector)[1]
  
  create.alter.setup()
  
  #compares max.wealth.prod without vs with variable preferences
  bs <- Agent.micro.econ(base, WEEKS, verbose, PROD.FUN=`max.wealth.prod`, sector=sector, agent=agents)
  as <- Agent.micro.econ(alter,WEEKS, verbose, PROD.FUN=`max.wealth.prod`, BETA.VAR = `var.beta`, sector=sector, agent=agents)
  
  plot.scenarios(WEEKS, bs, as, SETUP, "variable preferences", plot.files)  
}

# 12: Maximize wealth with Regulated Prices
if ('MXMZ.WLTH.REG.PRICES' %in% TESTS) {
  sector <- CLTH #CLTH #AGRC #TRNS #FUEL #HLTH #MNY
  agents <- agents.in.sector(sector)[1]
  
  create.alter.setup()
  
  #compares max.wealth.prod without vs with regulated prices ( PRICE.FACTOR = c(min.price.factor,max.price.factor) )
  bs <- Agent.micro.econ(alter,WEEKS, verbose, PROD.FUN=`max.wealth.prod`, sector=sector, agent=agents)
  as <- Agent.micro.econ(alter,WEEKS, verbose, PROD.FUN=`max.wealth.prod`, PRICE.FACTOR = c(0.25,4), sector=sector, agent=agents)
  
  plot.scenarios(WEEKS, bs, as, SETUP, "regulated prices", plot.files)  
}

# 13: Maximize wealth with Multiple Production
if ('MXMZ.WLTH.MULTI.PROD' %in% TESTS) {
  sector <- AGRC #TRNS #FUEL #HLTH #MNY
  agents <- agents.in.sector(sector)[1] 
  multi.sectors <- as.integer(SECTORS)
  
  create.alter.setup()
  
  # compares base scenario with multiple production ability
  bs <- Agent.micro.econ(alter,WEEKS, verbose)
  as <- Agent.micro.econ(alter,WEEKS, verbose, PROD.FUN=`multi.max.wealth.prod`, sector=sector, agent=agents, multi.sectors = multi.sectors)
  
  plot.scenarios(WEEKS, bs, as, SETUP, "multi product", plot.files)
  
  # get multi-prodction into the right place for plotting
  as[[2]] <- as[[6]]
  plot.vars(WEEKS, as, as, 'production', "multi product", ylab='units')
}
