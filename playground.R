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
agrc.agents <- sector.agents(SETUP,1)
clth.agents <- sector.agents(SETUP,2)
trns.agents <- sector.agents(SETUP,3)
fuel.agents <- sector.agents(SETUP,4)
hlth.agents <- sector.agents(SETUP,4)
mony.agents <- sector.agents(SETUP,5)

# 1: Over production
p.mtx <- matrix(1,dim[1],dim[2])
p.mtx[clth.agents,2] <- 1.5
v.mtx <- matrix(1,dim[1],dim[2])
v.mtx[clth.agents,] <- 1.5
setup.new(SETUP, prod.delta = p.mtx, vcons.delta = v.mtx)

bs <- Agent.micro.econ(base, WEEKS, verbose)
as <- Agent.micro.econ(alter,WEEKS, verbose)
plot.scenarios(WEEKS, bs, as, SETUP, "altered ratio", plot.files)

# 2: Productivity
p.mtx <- matrix(1,dim[1],dim[2])
p.mtx[clth.agents,2] <- 1.5
setup.new(SETUP, prod.delta = p.mtx)

bs <- Agent.micro.econ(base, WEEKS, verbose)
as <- Agent.micro.econ(alter,WEEKS, verbose)
plot.scenarios(WEEKS, bs, as, SETUP, "doubled productivity", plot.files)

# 3: Balanced market
setup.new(SETUP)
alter[[6]][2,2] <- alter[[6]][setdiff(1:dim[1],2),2] + 0.0128
alter[[7]][2,2] <- alter[[7]][2,2] + 0.0384

bs <- Agent.micro.econ(base, WEEKS, verbose)
as <- Agent.micro.econ(alter,WEEKS, verbose)
plot.scenarios(WEEKS, bs, as, SETUP, "balanced market", plot.files)

# 4: Quantitities
q.mtx <- matrix(1,dim[1],dim[2])
q.mtx[clth.agents,2] <- 0.5
setup.new(SETUP, quant.delta = q.mtx)

bs <- Agent.micro.econ(base, WEEKS, verbose)
as <- Agent.micro.econ(alter,WEEKS, verbose)
plot.scenarios(WEEKS, bs, as, SETUP, "same quantity", plot.files)

# 5: Preferences
p.mtx <- matrix(1,dim[1],dim[2])
p.mtx[agrc.agents,1] <- 0.7
p.mtx[clth.agents,2] <- 4
setup.new(SETUP, pref.delta = p.mtx)

bs <- Agent.micro.econ(base, WEEKS, verbose)
as <- Agent.micro.econ(alter,WEEKS, verbose)
plot.scenarios(WEEKS, bs, as, SETUP, "Altered preferences", plot.files)