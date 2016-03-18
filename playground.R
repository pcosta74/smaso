source('./properties.R')
source('./setup.expanded.R')
source('./plot.expanded.R')
source('./micro.econ.expanded.R')

#RUNNING AGENT#
scenario <- as.character(SCENARIOS[1])
out.files <- FALSE

# Overproduction
mtx<-matrix(1,4,4)
mtx[1,1]<-2
setup.new(scenario, ratio.delta = mtx)

bs <- Agent.micro.econ(base, WEEKS)
as <- Agent.micro.econ(alter,WEEKS)
plot.scenarios(WEEKS, bs, as, scenario, "overproduction", out.files)

#Underproduction
mtx[1,1]<-0.5
setup.new(scenario, ratio.delta = mtx)

bs <- Agent.micro.econ(base, WEEKS)
as <- Agent.micro.econ(alter,WEEKS)
plot.scenarios(WEEKS, bs, as, scenario, "underproduction", out.files)

# Productivity
mtx[1,1]<-2
setup.new(scenario, prod.delta = mtx)

bs <- Agent.micro.econ(base, WEEKS)
as <- Agent.micro.econ(alter,WEEKS)
plot.scenarios(WEEKS, bs, as, scenario, "productivity", out.files)

# Quantitities
mtx[1,1]<-0.1
setup.new(scenario, quant.delta = mtx)

bs <- Agent.micro.econ(base, WEEKS)
as <- Agent.micro.econ(alter,WEEKS)
plot.scenarios(WEEKS, bs, as, scenario, "quantity", out.files)

# Preference
mtx[1,1]<-0.5
setup.new(scenario, pref.delta = mtx)

bs <- Agent.micro.econ(base, WEEKS)
as <- Agent.micro.econ(alter,WEEKS)
plot.scenarios(WEEKS, bs, as, scenario, "preference", out.files)
