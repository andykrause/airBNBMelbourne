
### Preliminary Commands -----------------------------------------------------------------

 ## Set global options

  options(xtable.comment = FALSE)

 ## Load libraries

  library(spdep)
  library(maptools)
  library(gstat)
  library(kernlab)
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
  library(ROCR)

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
  ltr.mod.spec <- formula(log(event.price) ~ as.factor(type) + 
                            as.factor(bedbath) + as.factor(suburb) + 
                            as.factor(ltr.month))
  abb.mod.spec <- formula(log(nightly.rate) ~ as.factor(type) + 
                            as.factor(bedbath) + as.factor(suburb))
 
 # Make comparison between two markets  
  imp.data <- imputeRatesRents(ltr.df=ltr.data, 
                               abb.df=abb.data, 
                               ltr.mod.spec=ltr.mod.spec, 
                               abb.mod.spec=abb.mod.spec,
                               clip.field='suburb')
  
  # Extract out DFs (only those that imputed)
  abb.imp <- imp.data$abb
  ltr.imp <- imp.data$ltr
  
  # Extract models
  abb.sum <- summary(imp.data$abb.mod)
  ltr.sum <- summary(imp.data$ltr.mod)
  
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
  mrkt.table$est[c(1:5, 11:15)] <- 'Imp. Rates & Rents'
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
                            'Imp. Rates & Rents', 'Imp. Rates & Rents'),
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
                             'Imp. Rates & Rents', 'Imp. Rates & Rents'),
                       data=c('Airbnb', 'Long-Term', 'Airbnb', 'Long-Term'))
  
  num.df <- data.frame(x=rep(0, 4),
                       y=rep(.8, 4),
                       est=as.factor(c('Actual Rates & Rents', 'Imp. Rates & Rents',
                             'Actual Rates & Rents', 'Imp. Rates & Rents')),
                       data=as.factor(c('Airbnb', 'Airbnb', 'Long-Term', 'Long-Term')),
                       count=c(1,3,2,4))
  
  explain.plot <- blank.plot +
    geom_text(data=ann.df,
              label=ann.df$lab,
              size=3) +
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
              size=3) +
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
  abb.revs$rate.qtl <- makeWtdQtl(abb.revs$med.rate, 
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
  
  rate.hm <- makeHeatMap(abb.revs,
                x.field='occ.rate',
                y.field='nightly.rate',
                color.field='abb.act',
                bins=c(.05, 25),
                svm=F, 
                alpha.count=T,
                add.points=T,
                fill.colors=c(abb.col[1], abb.col[5]))
  
  rate.hm <- rate.hm +
    xlab('\n Occupancy Rate') +
    ylab('\n Nightly Rate') +
    scale_x_continuous(breaks=seq(0, 1, by=.25),
                       labels=c('0%', '25%', '50%', '75%', '100%')) +
    theme(legend.position='bottom')
  
 # Rate SVM heatmap
  
  rate.hm.svm <- makeHeatMap(abb.revs,
                         x.field='occ.rate',
                         y.field='nightly.rate',
                         color.field='abb.act',
                         bins=c(.05, 25),
                         svm=T, 
                         alpha.count=F,
                         add.points=T,
                         fill.colors=c(abb.col[1], abb.col[5]))
  
  rate.hm.svm <- rate.hm.svm +
    xlab('\n Occupancy Rate') +
    ylab('\n Nightly Rate') +
    scale_x_continuous(breaks=seq(0, 1, by=.25),
                       labels=c('0%', '25%', '50%', '75%', '100%')) +
    theme(legend.position='bottom')
  
  

  ## Make quartile heat map  
  
  qtl.hm <- makeHeatMap(abb.revs,
                         x.field='occ.qtl',
                         y.field='rate.qtl',
                         color.field='abb.act',
                         bins=c(5, 5),
                         svm=F, 
                         alpha.count=T,
                         add.points=T,
                         fill.colors=c(abb.col[1], abb.col[5]))
  
  qtl.hm <- qtl.hm +
    xlab('\n Quantile of Occupancy Rate') +
    ylab('\n Quantile of Nightly Rate') +
    scale_x_continuous(breaks=seq(0, 100, by=25),
                       labels=c('0', '25th', '50th', '75th', '100th')) +
    scale_y_continuous(breaks=seq(0, 100, by=25),
                       labels=c('0', '25th', '50th', '75th', '100th')) +
    theme(legend.position='bottom')
  
  ## Make quartile heat map  
  
  qtl.hm.svm <- makeHeatMap(abb.revs,
                        x.field='occ.qtl',
                        y.field='rate.qtl',
                        color.field='abb.act',
                        bins=c(5, 5),
                        svm=T, 
                        alpha.count=F,
                        add.points=T,
                        fill.colors=c(abb.col[1], abb.col[5]))
  
  qtl.hm.svm <- qtl.hm.svm +
    xlab('\n Quantile of Occupancy Rate') +
    ylab('\n Quantile of Nightly Rate') +
    scale_x_continuous(breaks=seq(0, 100, by=25),
                       labels=c('0', '25th', '50th', '75th', '100th')) +
    scale_y_continuous(breaks=seq(0, 100, by=25),
                       labels=c('0', '25th', '50th', '75th', '100th')) +
    theme(legend.position='bottom')
  
  
  ## Get SVM Counts for market analysis
  
  svm.rate <- makeSVM(abb.revs,
                     x.field='occ.rate',
                     y.field='nightly.rate',
                     z.field='abb.act',
                     svm.type='C-svc',
                     svm.kernel='polydot',
                     poly.degree=4,
                     expand.factor=100)
  
  svm.qtl <- makeSVM(abb.revs,
                      x.field='occ.qtl',
                      y.field='rate.qtl',
                      z.field='abb.act',
                      svm.type='C-svc',
                      svm.kernel='polydot',
                      poly.degree=4,
                      expand.factor=100)
  
  market.ratio <- data.frame(type=c('rate', 'qtl'),
                             actual=rep(mean(abb.revs$abb.act), 2),
                             fitted=c(mean(svm.rate$orig$fitted),
                                      mean(svm.qtl$orig$fitted)),
                             svm=c(mean(svm.rate$pred$pred),
                                   mean(svm.qtl$pred$pred)))
  
### Split by Type -----------------------------------------------------------------------
  
  # Set model specifications
  ltr.mod.spec <- formula(log(event.price) ~ as.factor(bedbath) + as.factor(suburb) + 
                            as.factor(ltr.month))
  abb.mod.spec <- formula(log(nightly.rate) ~ as.factor(bedbath) + as.factor(suburb))
  
  # Select data
  abb.apt <- which(abb.revs$type == 'Apartment')
  ltr.apt <- which(ltr.revs$type == 'Apartment')

  # Estimate House Results  
  house <- fullMarketAnalysis(ltr.df=ltr.revs[-ltr.apt,],
                              abb.df=abb.revs[-abb.apt, ],
                              ltr.mod.spec=ltr.mod.spec,
                              abb.mod.spec=abb.mod.spec,
                              clip.field='suburb',
                              market.field='none',
                              mrkt.col=sm.col,
                              heat.col=c(abb.col[1], abb.col[5]))

   apt <- fullMarketAnalysis(ltr.df=ltr.revs[ltr.apt,],
                             abb.df=abb.revs[abb.apt, ],
                             ltr.mod.spec=ltr.mod.spec,
                             abb.mod.spec=abb.mod.spec,
                             clip.field='suburb',
                             market.field='sub.mrkt',
                             mrkt.col=sm.col,
                             heat.col=c(abb.col[1], abb.col[5]))

### Split by Submarkets ------------------------------------------------------------------   
   
  # Set model specifications
  ltr.mod.spec <- formula(log(event.price) ~ as.factor(type) + 
                          as.factor(bedbath) +
                          as.factor(suburb) + 
                          as.factor(ltr.month))
  abb.mod.spec <- formula(log(nightly.rate) ~ as.factor(type) + 
                          as.factor(bedbath) +
                          as.factor(suburb))

  #  Extract levels and set capture
  s.levels <- levels(abb.revs$sub.mrkt)
  sm.results <- list()
  sm.pos <- 1

  # Estimate results
  for(sm in s.levels){
  
    abb.x <- which(abb.revs$sub.mrkt == sm)
    ltr.x <- which(ltr.revs$sub.mrkt == sm)
  
    x.res <- fullMarketAnalysis(ltr.df=ltr.revs[ltr.x,],
                                abb.df=abb.revs[abb.x, ],
                                ltr.mod.spec=ltr.mod.spec,
                                abb.mod.spec=abb.mod.spec,
                                clip.field='suburb',
                                market.field='sub.mrkt',
                                mrkt.col=sm.col,
                                heat.col=c(abb.col[1], abb.col[5]))
  
    sm.results[[sm.pos]] <- x.res
    sm.pos <- sm.pos + 1
  
  }
  names(sm.results) <- as.character(s.levels)

### Estimate at bed/bath level -----------------------------------------------------------  

  # Set model specifications
  ltr.mod.spec <- formula(log(event.price) ~ as.factor(type) + 
                            as.factor(suburb) + 
                            as.factor(ltr.month))
  abb.mod.spec <- formula(log(nightly.rate) ~ as.factor(type) + 
                            as.factor(suburb))
  
  # Set levels and capture
  bb.levels <- levels(abb.revs$bedbath)
  bb.results <- list()
  bb.pos <- 1

  # Estimate results
  for(bb in bb.levels){
  
    abb.x <- which(abb.revs$bedbath == bb)
    ltr.x <- which(ltr.revs$bedbath == bb)
  
    x.res <- fullMarketAnalysis(ltr.df=ltr.revs[ltr.x,],
                                abb.df=abb.revs[abb.x, ],
                                ltr.mod.spec=ltr.mod.spec,
                                abb.mod.spec=abb.mod.spec,
                                clip.field='suburb',
                                market.field='bedbath',
                                mrkt.col=sm.col,
                                heat.col=c(abb.col[1], abb.col[5]))
  
    bb.results[[bb.pos]] <- x.res
    bb.pos <- bb.pos + 1
  
  }
  names(bb.results) <- as.character(bb.levels)
  
### Split by submarket and type ----------------------------------------------------------  
  
  # Set model specifications
  ltr.mod.spec <- formula(log(event.price) ~ as.factor(suburb) + 
                            as.factor(ltr.month))
  abb.mod.spec <- formula(log(nightly.rate) ~ as.factor(suburb))
  
  # Create smt variable  
  abb.revs$smt <- as.factor(paste0(abb.revs$type, '.', abb.revs$sub.mrkt))
  ltr.revs$smt <- as.factor(paste0(ltr.revs$type, '.', ltr.revs$sub.mrkt))

  # Create levels and captures
  smt.levels <- levels(abb.revs$smt)
  smt.results <- list()
  smt.pos <- 1

  # Estimate the smt models
  for(smt in smt.levels){
  
    abb.x <- which(abb.revs$smt == smt)
    ltr.x <- which(ltr.revs$smt == smt)
  
    x.res <- fullMarketAnalysis(ltr.df=ltr.revs[ltr.x,],
                                abb.df=abb.revs[abb.x, ],
                                ltr.mod.spec=ltr.mod.spec,
                                abb.mod.spec=abb.mod.spec,
                                clip.field='suburb',
                                market.field='smt',
                                mrkt.col=sm.col,
                                heat.col=c(abb.col[1], abb.col[5]))
  
    smt.results[[smt.pos]] <- x.res
    smt.pos <- smt.pos + 1
  }
  names(smt.results) <- as.character(smt.levels)
  
### Build logit models on this -----------------------------------------------------------
  
 ## Create combined data
  
  smt.data <- rbind.fill(lapply(smt.results, function(x) x$abb))
  
  # Fix cancel.policy issue
  ss60 <- which(smt.data$cancel.policy == 'Super Strict 60 Days')
  smt.data$cancel.policy[ss60] <- 'Strict'
  cpmiss <- which(smt.data$cancel.policy == '')
  smt.data <- smt.data[-cpmiss, ]
  
 ## Build models  
  
  # Structural Only model
  mod.str <- glm(abb.act~type+as.factor(bedbath),
                 family=binomial(link='logit'),
                 data=smt.data)
  
  # +Submarket model
  mod.sm <- glm(abb.act~type+as.factor(bedbath)+
                  sub.mrkt,
                 family=binomial(link='logit'),
                 data=smt.data)

  # +Host model
  mod.host <- glm(abb.act~type+as.factor(bedbath)+
                    sub.mrkt+
                    I(max.guests/bedrooms)+min.stay+
                    I(cancel.policy=='Flexible') + I(cancel.policy=='Strict'),
                  family=binomial(link='logit'),
                  data=smt.data)
  
  # Suburb model
  mod.sub <- glm(abb.act~type+as.factor(bedbath)+
                   suburb,
                 family=binomial(link='logit'),
                 data=smt.data)
  
  # Suburb Host model
  mod.subh <- glm(abb.act~type+as.factor(bedbath)+
                  suburb+
                  max.guests+min.stay+
                  I(cancel.policy=='Flexible') + I(cancel.policy=='Strict'),
                  family=binomial(link='logit'),
                  data=smt.data)
  
 ## Extract Coef
  
  str.ct <- summary(mod.str)$coef
  sm.ct <- summary(mod.sm)$coef
  host.ct <- summary(mod.host)$coef

  # Assign SS
  f.l <- nrow(host.ct)
  assignSS <- function(x){
    
    y<-rep('   ', length(x))
    y[x<.1] <- '*  '
    y[x<.05] <- '** '
    y[x<.01] <- '***'
    
    y
  }
  
  # Build coefs with SS
  str.coef <- sprintf("%.3f" ,str.ct[,1])
  str.sig <- assignSS(str.ct[,4])
  str.coef <- c(paste0(str.coef, str.sig), rep(' ', f.l - nrow(str.ct)))
  
  sm.coef <- sprintf("%.3f" ,sm.ct[,1])
  sm.sig <- assignSS(sm.ct[,4])
  sm.coef <- c(paste0(sm.coef, sm.sig), rep(' ', f.l - nrow(sm.ct)))
  
  host.coef <- sprintf("%.3f" ,host.ct[,1])
  host.sig <- assignSS(host.ct[,4])
  host.coef <- c(paste0(host.coef, host.sig), rep(' ', f.l - nrow(host.ct)))
  
  # Combine into table
  mod.table <- data.frame(Variable=rownames(host.ct),
                          Str.Model=str.coef,
                          Sm.Model=sm.coef,
                          Host.Model=host.coef)
  
  # Fix variable names
  mod.table$Variable <- c('Intercept', 'House', '1Bed/1Bath','2Bed/1Bath',
                          '3Bed/1Bath', '3Bed/2Bath', '4Bed/2Bath', 'City',
                          'Suburban','Rural', 'Beach', 'Guests/Bedroom',
                          'Min. Stay', 'Flexible Cancel', 'Strict Cancel')
  
  
 ## Create model diagnostics  
  
  # Custom function
  logDx <- function(log.model, data, resp.var){
    
    pred <- prediction(predict(log.model, data, type='response'), resp.var)
    auc <- performance(pred, measure='auc')
    ll <- logLik(log.model)
    AIC <- AIC(log.model)
    
    return(list(AIC=AIC,
                logLik=ll,
                auc=auc))
  }
  
  # Extract diagnostics
  str.dx <- logDx(mod.str, smt.data, smt.data$abb.act)
  sm.dx <- logDx(mod.sm, smt.data, smt.data$abb.act)
  host.dx <- logDx(mod.host, smt.data, smt.data$abb.act)
  
  # Create diagnostic table
  diag.table <- data.frame(Variable=c('', 'Diagnostics', 'AIC', 'LogLik', 'AUC'),
                           Str.Model=c('', '', round(str.dx$AIC,0), 
                                       round(str.dx$logLik, 0),
                                       round(as.numeric(str.dx$auc@y.values), 3)),
                           Sm.Model=c('', '', round(sm.dx$AIC,0), 
                                      round(sm.dx$logLik, 0),
                                      round(as.numeric(sm.dx$auc@y.values), 3)),
                           Host.Model=c('', '', round(host.dx$AIC,0), 
                                        round(host.dx$logLik, 0),
                                        round(as.numeric(host.dx$auc@y.values), 3)))
 
 ## Combine into full table of coef and diags
  
 full.table <- rbind(mod.table, diag.table)

### Save workspace -----------------------------------------------------------------------
  
  save.image("C:/Dropbox/Research/airBNB/data/analyzed/abb_results.RData")

  save(abb.revs, ltr.revs, clean.count, blank.plot,
       rate.hm, rate.hm.svm, qtl.hm, qtl.hm.svm, market.ratio, 
       house, apt, sm.results, full.table, smt.results, bb.results,
       abb.sum, ltr.sum, ann.df, mrkt.table, num.df, reason.df,
       file="C:/Dropbox/Research/airBNB/data/analyzed/abb_objs.RData")
 
