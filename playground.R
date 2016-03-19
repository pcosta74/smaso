source('./properties.R')
source('./setup.expanded.R')
source('./plot.expanded.R')
source('./micro.econ.expanded.R')

out.files <- FALSE

#RUNNING AGENT#
setup <- as.character(SETUPS[1])
dim<-dim.setup(setup)

# 1: Over production
p.mtx<-matrix(1,dim[1],dim[2])
p.mtx[2,2]<-1.5
v.mtx<-matrix(1,dim[1],dim[2])
v.mtx[2,]<-1.5
setup.new(setup, prod.delta = p.mtx, vcons.delta = v.mtx)

bs <- Agent.micro.econ(base, WEEKS, TRUE)
as <- Agent.micro.econ(alter,WEEKS, TRUE)
plot.scenarios(WEEKS, bs, as, setup, "altered ratio", out.files)

# 2: Productivity
mtx<-matrix(1,dim[1],dim[2])
mtx[2,2]<-2
setup.new(setup, prod.delta = mtx)

bs <- Agent.micro.econ(base, WEEKS, TRUE)
as <- Agent.micro.econ(alter,WEEKS, TRUE)
plot.scenarios(WEEKS, bs, as, setup, "doubled productivity", out.files)

# 4: Quantitities
mtx<-matrix(1,dim[1],dim[2])
mtx[2,2]<-0.5
setup.new(setup, quant.delta = mtx)

bs <- Agent.micro.econ(base, WEEKS, TRUE)
as <- Agent.micro.econ(alter,WEEKS, TRUE)
plot.scenarios(WEEKS, bs, as, setup, "same quantity", out.files)

# 5: Preferences
mtx<-matrix(1,dim[1],dim[2])
mtx[1,1]<-0.7
mtx[2,2]<-4
setup.new(setup, pref.delta = mtx)

bs <- Agent.micro.econ(base, WEEKS, TRUE)
as <- Agent.micro.econ(alter,WEEKS, TRUE)
plot.scenarios(WEEKS, bs, as, setup, "Altered preferences", out.files)