
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