source(file.path('.','properties.R'))
source(file.path('.','enums.R'))

# export file formats 
PLOT.FILE.FORMATS <- enum('bmp','png','jpeg','tiff')

# plot base and alternative scenarios
plot.scenarios <- function(weeks, bs.scen, as.scen, setup=NULL,
                           description='', out.files=FALSE, file.format=NULL) {
    
    setup <- match.enum(setup, SETUPS)
    file.format <- as.character(match.enum(file.format, PLOT.FILE.FORMATS))

    for(indicator in levels(INDICATORS)) {
      if(out.files) {
        filename <- paste(indicator, description, setup, sep='-')
        filepath <- file.path(paste(filename, file.format, sep='.'))
        eval(call(file.format, filepath, antialias='subpixel',
                  width=PLOT.FILE.WDT, height=PLOT.FILE.HGT, res=PLOT.FILE.RES))
        message(paste("Generated", sQuote(filepath)))
      }
      
      ylab <- switch(indicator,
        'wealth' = 'monetary units',
        'prices' = 'monetaty units',
        'units'
      )
      plot.vars(weeks, bs.scen, as.scen, indicator, description, ylab=ylab)
      
      if(out.files) {
        dev.off()
      }
    }
  }

# plot variables in scenario
plot.vars <- 
  function(weeks, bs.scen, as.scen, indicator, 
           description='', xlab='weeks', ylab='units') {
  
  # validar indicator
  index <- match.enum(indicator,INDICATORS)
  
  # estabelecer limites
  limit <- ncol(bs.scen[[index]])
  ncols <- ceiling(sqrt(limit))
  nrows <- limit%/%ncols
  ncells <- ncols * (nrows + limit%%2)
  
  # graficos na mesma janela
  cells<-1:ncells + 1
  grid<-rbind(rep(1, ncols),matrix(cells, ncol = ncols))
  layout(mat = grid, heights = c(0.15, rep(1, nrow(grid)-1)), respect = TRUE)

  # oma e a margem do titulo principal
  par(oma = c(2,3,2,1))
  
  par(mai=rep(0,4))
  plot.new()
  legend(x='center', horiz = TRUE, bty='n',
         title = expression(bold('Scenarios')), 
         camel.case(c("base", description)),
         lty=1, lwd=PLOT.LWD, cex = 0.8,
         col=c(PLOT.COL.BASE, PLOT.COL.ALTER),
         inset=rep(1.1,4)
  )

  for (column in 1:limit) {
    par(mai=rep(0.35,4))
    
    #determina o valor maximo do eixo YY
    m<-max(bs.scen[[index]][,column], as.scen[[index]][,column], na.rm=T)
    x.ticks<-pretty(0:weeks+1,n=weeks+2)
    y.ticks<-pretty(0:1.1*m)
    
    #desenhar o cenario base
    plot(bs.scen[[index]][,column], 
         type = "l", 
         ann=FALSE,
         axes=FALSE,
         frame.plot = TRUE,
         xlim = c(1,max(x.ticks)),
         ylim = c(0,max(y.ticks)),
         lwd = PLOT.LWD,
         col = PLOT.COL.BASE)
    #desenhar cenario alternativo
    lines(as.scen[[index]][,column], 
          type = "l",
          lwd = PLOT.LWD,
          col = PLOT.COL.ALTER)
    
    #titulos
    title(main=colnames(bs.scen[[index]][column]),line=0.7)
    
    # configuração dos eixos
    axis(side=1, cex.axis=0.7, at=x.ticks, mgp=c(3, 0.5, 0))
    axis(side=2, cex.axis=0.7, at=y.ticks, mgp=c(3, 0.7, 0), las=1)
  }
  
  title(main=camel.case(paste("evolution of", indicator)), 
        outer=TRUE, cex.main=1.5)
  title(xlab = camel.case(xlab),
        ylab = camel.case(ylab), 
        outer=TRUE,
        line=0)
}

