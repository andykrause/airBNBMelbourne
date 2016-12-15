
### Preliminary Commands -----------------------------------------------------------------

 ## Set global options

  options(xtable.comment = FALSE)

 ## Load libraries

  library(spdep)
  library(maptools)
  library(gstat)
  library(ggplot2)
  library(geosphere)
  library(ggmap)
  library(xtable)
  library(chron)
  library(plyr)
  library(sp)
  library(rgeos)
  library(reshape2)
  library(stringr)
  library(RColorBrewer)
  library(Hmisc)

 ## Set path location

  # Get computer names
  comp.name <- Sys.info()['nodename']

  # Assign path based on computer name
  if(comp.name == '7020D-121777-W' | 
      comp.name == 'DESKTOP-1D7JO4J'){
  
    data.path <- 'c:/dropbox/research/airBNB/data/'
  
  } else {
  
    data.path <- 'gideon path'
  
  }

 ## Load working workspace   
  
  load(paste0(data.path, '/analyzed/abb_working.RData'))
  
 ## Load sources (custom function files)

  source("c:/code/dataviztools/ggplottools.R")
  source("c:/code/datamgmttools/dataMungetools.R")
  source("c:/code/research/AirBNBMelbourne/analysis_Functions.R")
  source("c:/code/research/AirBNBMelbourne/dataPrep_Functions.R")

  sm.col <- abb.col[c(2, 1, 3, 5, 6)]
  
### Working analysis ---------------------------------------------------------------------  

 ## Global model

  # Set model specifications
  ltr.mod.spec <- formula(log(event.price) ~ as.factor(bedbath) + as.factor(suburb))
  abb.mod.spec <- formula(log(nightly.rate) ~ as.factor(bedbath) + as.factor(suburb))
 
 # Make comparison between two markets  
  imp.data <- imputeRatesRents(ltr.df=ltr.data, 
                               abb.df=abb.data, 
                               ltr.mod.spec=ltr.mod.spec, 
                               abb.mod.spec=abb.mod.spec,
                               clip.field='suburb')
  
  # Extract out DFs (only those that imputed)
  abb.imp <- imp.data$abb
  ltr.imp <- imp.data$ltr
  
 # Within type imputation revenues
  
  imp.revs <- revenueEngine(abb.imp, 
                            ltr.imp,
                            rate.field='imp.rate',
                            rent.field='imp.rent')
  
  # Add back to DFs
  abb.imp$imp.revenue <- imp.revs$abb
  ltr.imp$imp.revenue <- imp.revs$ltr
  
 # Cross type revenue estimation
  
  # Apply dom to abb
  abb.imp$imp.dom <- imputeDOM(abb.imp, ltr.imp, calc.type='median')
  
  # Apply occ.rate to ltr
  ltr.imp$imp.occ <- imputeOccRate(ltr.imp, abb.imp, calc.type='median')
  
  # Estimate cross revenues
  impx.revs <- revenueEngine(abb.df=ltr.imp, 
                             ltr.df=abb.imp,
                             rate.field='imp.rate',
                             rent.field='imp.rent',
                             occ.field='imp.occ',
                             dom.field='imp.dom')
  
  # Add back to DFs
  abb.imp$imp.ltr.revenue <- impx.revs$ltr
  ltr.imp$imp.abb.revenue <- impx.revs$abb
  
  # Compare revenues
  comp.revs <- compareRevenues(abb.imp, ltr.imp)
  
  # Extract
  abb.revs <- comp.revs$abb
  ltr.revs <- comp.revs$ltr
  
 ## Make a table  
  
  mrkt.table <- createCompTable(abb.revs, ltr.revs, 'sub.mrkt')
  mrkt.table$ID <- factor(mrkt.table$ID, 
                          levels=c('city-core', 'city', 'suburban', 'rural', 'beach'))
  
## make basic 2x2 comparison
  
  twotwo.bar.plot <- 
    ggplot(mrkt.table, 
                            aes(x=ID, weights=Var, fill=ID)) + 
    geom_bar() +
    facet_grid(est ~ data) +
    scale_fill_manual(values=sm.col) +
    xlab('') +
    ylab('% of properties where Airbnb is more profitable') +
    scale_y_continuous(breaks=c(0,.25,.5,.75,1),
                       labels=c('0%', '25%', '50%', '75%', '100%')) +
    theme(legend.position='none')
  
  
  # 
  
  mrkt.table$x <- 0
  mrkt.table$y <- 0
  mrkt.table$good <- c(rep('bad', 5), rep('good', 5), rep('bad', 10))
                       
  blank.plot <- ggplot(mrkt.table, 
         aes(x=x, y=y)) +
    facet_grid(est ~ data) +
    scale_fill_manual(values=c('salmon', 'lightgreen')) +
    theme(legend.position='none',
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) 
  
  
  ann.df <- data.frame(x=rep(0, 4),
                      y=rep(0, 4), 
                      lab=c(paste0('Airbnb Properties: \n (Actual Airbnb Revenue \n vs.',
                                   '\n Imputed Long Term Revenue)'),
                            paste0('Long-Term Properties: \n (Imputed Airbnb Revenue \n vs.',
                                   '\n Actual Long Term Revenue)'),
                            paste0('Airbnb Properties: \n (Imputed Airbnb Revenue \n vs.',
                                   '\n Imputed Long Term Revenue)'),
                            paste0('Long-Term Properties: \n (Imputed Airbnb Revenue \n vs.',
                                   '\n Imputed Long Term Revenue)')),
                      est=c('Actual Rates & Rents', 'Actual Rates & Rents',
                            'Imputed Rates & Rents', 'Imputed Rates & Rents'),
                      data=c('Airbnb', 'Long-Term', 'Airbnb', 'Long-Term'))
  
  reason.df <- data.frame(x=rep(0, 4),
                       y=rep(0, 4), 
                       lab=c(paste0('+ Actual occupancy rates \n',
                                    '+ Actual nightly rates'),
                             paste0('- Imputed occupancy rates \n',
                                    '- Imputed nightly rate estimates'),
                             paste0('+ Actual occupancy rates \n', 
                                    '- Imputed nightly rate estimates'),
                             paste0('- Imputed occupancy rates \n',
                                    '- Imputed nightly rate estimates')),
                       est=c('Actual Rates & Rents', 'Actual Rates & Rents',
                             'Imputed Rates & Rents', 'Imputed Rates & Rents'),
                       data=c('Airbnb', 'Long-Term', 'Airbnb', 'Long-Term'))
  
  num.df <- data.frame(x=rep(0, 4),
                       y=rep(.8, 4),
                       est=as.factor(c('Actual Rates & Rents', 'Imputed Rates & Rents',
                             'Actual Rates & Rents', 'Imputed Rates & Rents')),
                       data=as.factor(c('Airbnb', 'Airbnb', 'Long-Term', 'Long-Term')),
                       count=c(1,3,2,4))
  
  explain.plot <- blank.plot +
    geom_text(data=ann.df,
              label=ann.df$lab,
              size=4.5) +
    geom_text(data=num.df,
              label=num.df$count,
              size=9.5) +
    coord_cartesian(ylim=c(-.5,1))
  
  
  reason.plot <- blank.plot +
    geom_rect(data = mrkt.table, aes(fill=good),
              xmin = -Inf,xmax = Inf,
              ymin = -Inf,ymax = Inf, alpha = 0.4) +
    geom_text(data=reason.df,
              label=reason.df$lab,
              size=4.5) +
    geom_text(data=num.df,
              label=num.df$count,
              size=9.5) +
    coord_cartesian(ylim=c(-.5,1))
  
  
  ## Make a table  
  
  abb.act.table <- mrkt.table[mrkt.table$est == 'Actual Rates & Rents' &
                               mrkt.table$data == 'Airbnb', ]
  
  ## make basic 2x2 comparison
  
  abb.act.plot <- 
    ggplot(abb.act.table, 
           aes(x=ID, weights=Var, fill=ID)) + 
    geom_bar() +
    scale_fill_manual(values=sm.col) +
    xlab('') +
    ylab('% of properties where Airbnb is more profitable') +
    scale_y_continuous(breaks=c(0,.25,.5,.75,1),
                       labels=c('0%', '25%', '50%', '75%', '100%')) +
    theme(legend.position='none') +
    coord_cartesian(ylim=c(0,1))
  
  # Make pref plot
  

  rawocc.pplot <- makePrefPlot(abb.revs,
                               x.field='occ.rate',
                               y.field='abb.act',
                               group.field='sub.mrkt',
                               smooth=TRUE,
                               smooth.span=.75)
  rawocc.pplot <- rawocc.pplot +
    xlab('\nOccupancy Rate') +
    ylab('\n% of Properties where Airbnb is more profitable') +
    scale_x_continuous(breaks=seq(0, 100, by=25),
                       labels=c('0%', '25%', '50%', '75%', '100%')) +
    scale_y_continuous(breaks=seq(0, 1, by=.25),
                       labels=c('0%', '25%', '50%', '75%', '100%')) +
    scale_color_manual(values=sm.col)
  
  ## Look at occupancy rates
  
  abb.revs$sub.mrkt <- factor(abb.revs$sub.mrkt, 
                         levels=c('city-core', 'city', 'suburban', 'rural', 'beach'))
  ggplot(abb.revs, aes(x=occ.rate, group=sub.mrkt, color=sub.mrkt, fill=sub.mrkt)) +
    geom_density(alpha=.3) +
    facet_wrap(~sub.mrkt) +
    scale_color_manual(values=sm.col) +
    scale_fill_manual(values=sm.col) +
    scale_x_continuous(breaks=seq(0, 1, by=.5),
                       labels=c('0%', '50%', '100%')) +
    ylab('') +
    xlab('Occupancy Rate') +
    theme(legend.position='none',
          axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.y=element_blank())

  ## Add the quantile location
  
  abb.revs$occ.qtl <- makeWtdQtl(abb.revs$occ.rate, 
                                      return.type='rank') 
  
  ## Make quartile location plot
  
  qtlocc.pplot <- makePrefPlot(abb.revs,
                               x.field='occ.qtl',
                               y.field='abb.act',
                               group.field='sub.mrkt',
                               smooth=TRUE,
                               smooth.span=.75)
  qtlocc.pplot <- qtlocc.pplot +
    xlab('\nQualtile of Occupancy Rate') +
    ylab('\n% of Properties where Airbnb is more profitable') +
    scale_x_continuous(breaks=seq(0, 100, by=25),
                       labels=c('0', '25th', '50th', '75th', '100th')) +
    scale_y_continuous(breaks=seq(0, 1, by=.25),
                       labels=c('0%', '25%', '50%', '75%', '100%')) +
    scale_color_manual(values=sm.col)
  
  
  # Make the rate heatmap  
  
  rate.heatmap <- makeHeatMap(abb.revs,
                              x.field='occ.rate',
                              y.field='med.rate',
                              alpha.field='abb.act',
                              bins=c(.05, 30),
                              add.points=TRUE)
  rate.heatmap <- rate.heatmap +
    xlab('\n Occupancy Rate') +
    ylab('\n Nightly Rate') +
    scale_x_continuous(breaks=seq(0, 1, by=.25),
                       labels=c('0%', '25%', '50%', '75%', '100%')) +
    theme(legend.position='none')
 
  ## Add the rate quartle location   
  
  abb.revs$rate.qtl <- makeWtdQtl(abb.revs$med.rate, 
                                  return.type='rank') 
  
  ## Make quartile heat map  
  
  qtl.heatmap <- makeHeatMap(abb.revs,
                             x.field='occ.qtl',
                             y.field='rate.qtl',
                             alpha.field='abb.act',
                             bins=c(5,5))+
  xlab('\n Occupancy Rate Quantile') +
    ylab('\n Nightly Rate Quantile') +
    scale_x_continuous(breaks=seq(0, 100, by=25),
                       labels=c('0%', '25th', '50th', '75th', '100th')) +
    scale_y_continuous(breaks=seq(0, 100, by=25),
                       labels=c('0%', '25th', '50th', '75th', '100th')) +
    
    theme(legend.position='none')
  
  
### Adding the SVM separation
  
  
  
  library(kernlab)
  
  
  xy <- cbind(abb.revs$occ.rate, abb.revs$nightly.rate)
  
  aaa <- ksvm(x=xy,
              y=abb.revs$abb.act,
              data=xy,
              type='C-svc',
              kernel='polydot',
              kpar=list(degree=4))
  
  abb.revs$svm <- aaa@fitted
  
  pred.abb <- expand.grid(seq(0,1,by=.005),
                          seq(min(abb.revs$nightly.rate-25),
                              max(abb.revs$nightly.rate+25),
                              by=1))
  tt <- predict(aaa, pred.abb)
  pred.abb$ok <- tt
  
  
  makeHeatMap(pred.abb,
              x.field='Var1',
              y.field='Var2',
              alpha.field='ok',
              bins=c(.02, 5)) +
    geom_point(data=abb.revs,aes(x=occ.rate, y=nightly.rate), size=.1, alpha=.1) +
    ylab('Nightly Rate') +
    xlab('Occupancy Rate')
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
  
  
  glob <- fullMarketAnalysis(ltr.df=ltr.data,
                             abb.df=abb.data,
                             ltr.mod.spec=ltr.mod.spec,
                             abb.mod.spec=abb.mod.spec,
                             exch.rate=exch.rate,
                             clip.field='suburb')
  
  ltr.type <- split(ltr.dataf, ltr.dataf$type)
  abb.type <- split(abb.dataf, abb.dataf$type)
  
  apt <- fullMarketAnalysis(ltr.df=ltr.type[[1]],
                            abb.df=abb.type[[1]],
                                 ltr.mod.spec=ltr.mod.spec,
                                 abb.mod.spec=abb.mod.spec,
                                 exch.rate=exch.rate,
                                 clip.field='suburb')
  
  
  house <- fullMarketAnalysis(ltr.df=ltr.type[[2]],
                              abb.df=abb.type[[2]],
                              ltr.mod.spec=ltr.mod.spec,
                              abb.mod.spec=abb.mod.spec,
                              exch.rate=exch.rate,
                              clip.field='suburb')
  
  ggplot(glob$abb, aes(x=longitude, y=latitude)) + 
    geom_point(size=.1) + 
    stat_bin2d(data=apt$abb, aes(alpha=..count.., fill=as.factor(abb.act)), 
               binwidth=c(.005,.005)) + 
    coord_cartesian(xlim=c(144.95,145.05), ylim=c(-37.75,-37.95))
  
  
  
  ggplot(glob$abb, aes(x=longitude, y=latitude)) + geom_point(size=.1) + stat_bin2d(data=apt$abb, aes(alpha=..count.., fill=as.factor(abb.act)), binwidth=c(.005,.005)) + coord_cartesian(xlim=c(144.95,145.05), ylim=c(-37.75,-37.95))