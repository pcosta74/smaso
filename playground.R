source('./properties.R')
source('./setup.expanded.R')
source('./micro.econ.expanded.R')
source('./plot.expanded.R')

# simulation properties
WEEKS <- 20
SETUP <- '4x4'

#
plot.files <- FALSE
verbose <- TRUE

#RUNNING AGENT#
dim <- dim.setup(SETUP)
agrc.agents <- sector.agents(SETUP, AGRC)
clth.agents <- sector.agents(SETUP, CLTH)
trns.agents <- sector.agents(SETUP, TRNS)
fuel.agents <- sector.agents(SETUP, FUEL)
hlth.agents <- sector.agents(SETUP, HLTH)
mony.agents <- sector.agents(SETUP, MONY)

# # 1: Over production
# p.mtx <- matrix(1,dim[1],dim[2])
# p.mtx[clth.agents, CLTH] <- 1.5
# v.mtx <- matrix(1,dim[1],dim[2])
# v.mtx[clth.agents,] <- 1.5
# setup.new(SETUP, prod.delta = p.mtx, vcons.delta = v.mtx)
# 
# bs <- Agent.micro.econ(base, WEEKS, verbose)
# as <- Agent.micro.econ(alter,WEEKS, verbose)
# plot.scenarios(WEEKS, bs, as, SETUP, "altered ratio", plot.files)
# 
# # 2: Productivity
# p.mtx <- matrix(1,dim[1],dim[2])
# p.mtx[clth.agents, CLTH] <- 1.5
# setup.new(SETUP, prod.delta = p.mtx)
# 
# bs <- Agent.micro.econ(base, WEEKS, verbose)
# as <- Agent.micro.econ(alter,WEEKS, verbose)
# plot.scenarios(WEEKS, bs, as, SETUP, "doubled productivity", plot.files)
# 
# # 3: Balanced market
# setup.new(SETUP)
# othr.agents <- setdiff(1:dim[1],clth.agents)
# alter[[VCON]][othr.agents,CLTH] <- alter[[VCON]][othr.agents,CLTH] + 0.0128
# alter[[PROD]][clth.agents,CLTH] <- alter[[PROD]][clth.agents,CLTH] + 0.0384
# 
# bs <- Agent.micro.econ(base, WEEKS, verbose)
# as <- Agent.micro.econ(alter,WEEKS, verbose)
# plot.scenarios(WEEKS, bs, as, SETUP, "balanced market", plot.files)
# 
# # 4: Quantitities
# q.mtx <- matrix(1,dim[1],dim[2])
# q.mtx[clth.agents, CLTH] <- 0.5
# setup.new(SETUP, quant.delta = q.mtx)
# 
# bs <- Agent.micro.econ(base, WEEKS, verbose)
# as <- Agent.micro.econ(alter,WEEKS, verbose)
# plot.scenarios(WEEKS, bs, as, SETUP, "same quantity", plot.files)

# # 5: Preferences
# p.mtx <- matrix(1,dim[1],dim[2])
# p.mtx[,AGRC] <- 0.7 # 0.35
# p.mtx[,CLTH] <- 4   # 0.20
# setup.new(SETUP, pref.delta = p.mtx)
# 
# bs <- Agent.micro.econ(base, WEEKS, verbose)
# as <- Agent.micro.econ(alter,WEEKS, verbose)
# plot.scenarios(WEEKS, bs, as, SETUP, "Altered preferences", plot.files)




# 1: Over production
setup.new(SETUP)

bs <- Agent.micro.econ(base, WEEKS, verbose)
as <- Agent.micro.econ(alter,WEEKS, verbose,PROD.FUN = `max.wealth.prod`)
plot.scenarios(WEEKS, bs, as, SETUP, "altered ratio", plot.files)