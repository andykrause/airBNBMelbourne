
### Basic engine for calculating the revenues for abb and ltr ----------------------------

revenueEngine <- function(abb.df,
                          ltr.df,
                          rate.field='med.rate',
                          rent.field='event.price',
                          occ.field='occ.rate',
                          dom.field='dom',
                          cost.list=NULL)
{
  
 ## Calculate Airbnb Revenues  
  
  abb.rev <- abb.df[ ,rate.field] * 365 * abb.df[ ,occ.field]

 ## Calculate long term rental revenues  
    
  ltr.rev <- ltr.df[, rent.field] * (52 - (ltr.df[ ,dom.field] / 7))
  
 ## Save for future costs
  
 ## Return values  
  
  return(list(abb=abb.rev,
              ltr=ltr.rev))
  
}

### Cross impute rates and rents ---------------------------------------------------------

imputeRatesRents <- function(ltr.df,
                             abb.df,
                             ltr.mod.spec,
                             abb.mod.spec,
                             clip.fields=NULL)
{
  
 ## Arguments
  
  # ltr.df:  data.frame of long term rental observations
  # abb.df:  data.frame of airbnb properties
  # ltr.mod.spec:  specification for rent price model
  # abb.mod.spec:  specification for airbnb properties
  # clip.field: field to ensure factors match between rent and abb
  
 ## Remove those within the clip field that isn't present in both  
  
  if(!is.null(clip.fields)){
    
    for(i.cf in 1:length(clip.fields)){
    
      # Find the fields that are used to clip
      l.cf <- which(names(ltr.df) == clip.fields[i.cf])
      a.cf <- which(names(abb.df) == clip.fields[i.cf])
    
      # Get IDs for those to be removed
      id.l <- ltr.df[ ,l.cf] %in% names(table(as.character(abb.df[ ,a.cf])))
      id.a <- abb.df[ ,a.cf] %in% names(table(as.character(ltr.df[ ,l.cf])))
    
      # Filter out obs missing matched factors  
      ltr.df <- ltr.df[id.l, ]
      abb.df <- abb.df[id.a, ]
    
    }
    
 }

 ## Add the monthly factors

  abb.df$ltr.month <- 12
    
 ## Build regression models for rental values
  
  ltr.mod <- lm(ltr.mod.spec, data=ltr.df)

 ## Add the fitted values to the long term data
  
  ltr.df$imp.rent <- exp(ltr.mod$fitted)

 ## Add the predicted values to the short term data
  
  abb.df$imp.rent <- exp(predict(ltr.mod, abb.df))

 ## Build regression models
  
  abb.mod <- lm(abb.mod.spec, data=abb.df)

 ## Add the fitted values to the long term data
  
  abb.df$imp.rate <- exp(abb.mod$fitted)

 ## Add the predicted values to the short term data
  
  ltr.df$imp.rate <- exp(predict(abb.mod, ltr.df))

 ## Return Values  
  
  return(list(abb=abb.df,
              ltr=ltr.df,
              abb.mod=abb.mod,
              ltr.mod=ltr.mod))
}

### Assign quartile values based on a give vector, weighted if necessary -----------------

makeWtdQtl <- function(data.vec, 
                       wgts=rep(1,length(data.vec)),
                       return.type='rank')
{
  
 ## Load required library  
  
  require(Hmisc)
  
 ## Set the adjustment jitter to prevent identical breaks  
  
  adj.jit <- abs(mean(data.vec) / 100000)
  
 ## Calculate the weighted quantiles 0  to 1000  
  
  wtd.qtl <- Hmisc::wtd.quantile(data.vec + runif(length(data.vec), 0, adj.jit), 
                                 weights=wgts, 
                                 probs=seq(0, 1, .01))

 ## Fix the ends
  
  # Minimum
  if(wtd.qtl[1] > min(data.vec)){
    wtd.qtl[1] <- min(data.vec) - adj.jit
  }
  
  # Maximum
  if(wtd.qtl[length(wtd.qtl)] < max(data.vec)){
    wtd.qtl[length(wtd.qtl)] <- max(data.vec) + adj.jit
  }
    
 ##  Convert to a vector of quantile indicators 
  
  qtl.vec <- as.numeric(as.factor(cut(data.vec, 
                                      breaks=(wtd.qtl + seq(0, 1, .01) * adj.jit))))
  
 ## Return value
  
  if(return.type == 'rank'){
    return(qtl.vec)
  } else {
    return(wtd.qtl)
  }
  
}

### Impute days on market for the airbnb properties --------------------------------------

imputeDOM <- function(abb.df,
                      ltr.df,
                      calc.type='median'){
  
 ## If median type  
  
  if(calc.type == 'median'){
    dom.qtl <- makeWtdQtl(ltr.df$dom, return.type='raw')
    abb.df$imp.dom <- dom.qtl[51]
  }
  
 ## if model type  
  
  if(calc.type == 'model'){
    
    # Save for later
    
  }
  
  ## Return Values
  
  return(abb.df$imp.dom)
  
}

### Impute occupancy rate for the ltr properties -----------------------------------------

imputeOccRate <- function(ltr.df,
                          abb.df, 
                          calc.type='median',
                          submarket=NULL){
  
  ## If median type  
  
  if(calc.type == 'median'){
    occ.qtl <- makeWtdQtl(abb.df$occ.rate, return.type='raw',
                          wgts=(abb.df$bookings + abb.df$available.days))
    ltr.df$imp.occ <- occ.qtl[51]
  }
  
  ## If model type
  
  if(calc.type == 'model'){
    
    # Save for later
    
  }
  
  ## Return Values
  
  return(ltr.df$imp.occ)
  
}

### Compare revenues between abb and ltr -------------------------------------------------

compareRevenues <- function(abb.df, 
                            ltr.df){
  
  ## Abb act (1) to abb imp ltr (4)
  
  abb.df$abb.prem <- abb.df$revenue - abb.df$imp.ltr.revenue
  abb.df$abb.act <- ifelse(abb.df$abb.prem > 0, 1, 0)
  
  ## Abb imp (2) to abb imp ltr (4)
  
  abb.df$abb.prem.imp <- abb.df$imp.revenue - abb.df$imp.ltr.revenue
  abb.df$abb.imp <- ifelse(abb.df$abb.prem.imp > 0, 1, 0)
  
  ## Abb imp (2) to ltr act (3)
  
  ltr.df$abb.prem <- ltr.df$imp.abb.revenue - ltr.df$revenue
  ltr.df$abb.act <- ifelse(ltr.df$abb.prem > 0, 1, 0)
  
  ## Abb imp (2) to ltr imp (4)
  
  ltr.df$abb.prem.imp <- ltr.df$imp.abb.revenue - ltr.df$imp.revenue
  ltr.df$abb.imp <- ifelse(ltr.df$abb.prem.imp > 0, 1, 0)
  
  ## Return Values 
  
  return(list(abb=abb.df,
              ltr=ltr.df))  
  
}

### Wrapper for all numeric comparison calculations --------------------------------------

revCompWrapper <- function(ltr.df,
                           abb.df,
                           ltr.mod.spec,
                           abb.mod.spec,
                           clip.field='suburb'){  
  

 ## Impute rates and rents
  
  imp.data <- imputeRatesRents(ltr.df=ltr.df, 
                               abb.df=abb.df, 
                               ltr.mod.spec=ltr.mod.spec, 
                               abb.mod.spec=abb.mod.spec,
                               clip.field=clip.field)
  
  # Extract out DFs
  abb.df <- imp.data$abb
  ltr.df <- imp.data$ltr
  
 ## Within type imputation revenues
  
  imp.revs <- revenueEngine(abb.df, 
                            ltr.df,
                            rate.field='imp.rate',
                            rent.field='imp.rent')
  
  # Add back to DFs
  abb.df$imp.revenue <- imp.revs$abb
  ltr.df$imp.revenue <- imp.revs$ltr
  
 ## Cross type revenue estimation
  
  # Apply dom to abb
  abb.df$imp.dom <- imputeDOM(abb.df, ltr.df, calc.type='median')
  
  # Apply occ.rate to ltr
  ltr.df$imp.occ <- imputeOccRate(ltr.df, abb.df, calc.type='median')
  
  # Estimate cross revenues
  impx.revs <- revenueEngine(abb.df=ltr.df, 
                             ltr.df=abb.df,
                             rate.field='imp.rate',
                             rent.field='imp.rent',
                             occ.field='imp.occ',
                             dom.field='imp.dom')
  
   # Add back to DFs
   abb.df$imp.ltr.revenue <- impx.revs$ltr
   ltr.df$imp.abb.revenue <- impx.revs$abb
  
 ## Compare revenues
  
  comp.revs <- compareRevenues(abb.df, ltr.df)
  
 ## Return Values   
  
  return(list(abb=comp.revs$abb,
              ltr=comp.revs$ltr))    
  
}

### Create comparison table --------------------------------------------------------------

createCompTable <- function(abb.df, 
                            ltr.df,
                            split.field=NULL){
  
  if(is.null(split.field)){
    
  } else {
    
    # Calculate cross-tab values
    abb.imp <- tapply2DF(abb.df$abb.imp, abb.df[ ,split.field], mean)
    abb.act <- tapply2DF(abb.df$abb.act, abb.df[ ,split.field], mean)
    ltr.imp <- tapply2DF(ltr.df$abb.imp, ltr.df[, split.field], mean)
    ltr.act <- tapply2DF(ltr.df$abb.act, ltr.df[, split.field], mean)
    
    # Add names
    abb.imp$est <- ltr.imp$est <- 'Imputed Rates & Rents'
    abb.imp$data <- abb.act$data <- 'Airbnb'
    ltr.act$est <- abb.act$est <- 'Actual Rates & Rents'
    ltr.act$data <- ltr.imp$data <- 'Long-Term'
    
    # Combine into table
    rate.table <- rbind(abb.imp, abb.act, ltr.imp, ltr.act)
    
    # Reorder factors
    if(split.field == 'sub.mrkt'){
    
      rate.table$ID <- factor(rate.table$ID, 
                              levels=c('city-core', 'city', 'beach',
                                       'suburban', 'rural'))
        
    }
    
    
  }
  
  ## Return Values
  
  return(rate.table)
  
}

### Creating preference plots ------------------------------------------------------------  

makePrefPlot <- function(pref.data,
                         x.field,
                         y.field,
                         group.field='none',
                         metric='mean',
                         cumulative=FALSE,
                         smooth=FALSE,
                         smooth.span=.15){
  
  
  ## Fixing some variables
  
  if(x.field == 'occ.rate'){
    
    pref.data[, x.field] <- round(100 * pref.data[, x.field], 0)
    
  }
  
  
 ## Extract function of analysis
  
  metric.fnct <- get(metric)
  
 ## Set up capture list
  
  pref.list <- list()
  
 ## Loop through and create the preference plots  
  
  for(i.pl in 1:100){
    
    # Extract the ith data
    if(cumulative){
      pref.df <- pref.data[pref.data[,x.field] >= i.pl, ]
    } else {
      pref.df <- pref.data[pref.data[,x.field] == i.pl, ]
    }
    
    
    if(group.field != 'none'){
      
      # Create the table
      pref.table <- tapply2DF(pref.df[ ,y.field], 
                              pref.df[ ,group.field],
                              metric.fnct)
    } else {
      
      pref.table <- data.frame(ID='all', 
                               Var=metric.fnct(pref.df[,y.field], na.rm=T))
    }
    
    # Add the x variable
    pref.table$x.var <- i.pl
    
    # Add to the capture list
    pref.list[[i.pl]] <- pref.table
    
  }
  
 ## Convert to a data.frame  
  
  pref.full <- rbind.fill(pref.list)
  
  if(group.field == 'sub.mrkt'){
    pref.full$ID <- factor(pref.full$ID, 
                           levels=c('city-core', 'city', 'suburban', 'rural', 'beach'))
    
  }
  
 ## Creat the base plot  
  
  pref.plot <- ggplot(pref.full,
                      aes(x=x.var, y=Var, group=ID, color=ID))
  
 ## Add lines  
  
  if(smooth){
    pref.plot <- pref.plot + stat_smooth(se=FALSE, size=2, 
                                         span=smooth.span)
  } else {
    pref.plot <- pref.plot + geom_line()
  }
  
 ## Return Values
  
  return(pref.plot)
  
}

### Create heatmaps of profitability -----------------------------------------------------
makeHeatMap <- function(hm.data,
                        x.field,
                        y.field,
                        color.field,
                        bins=c(10, 10),
                        fill.colors=c('red', 'forestgreen'),
                        alpha.count=TRUE,
                        alpha.fill=1,
                        add.points=FALSE,
                        hexmap=FALSE,
                        point.data=NULL,
                        svm=FALSE,
                        return.svm=FALSE,
                        svm.opts=list(type='C-svc',
                                      kernel='polydot',
                                      poly.degree=2,
                                      expand.factor=100)){
  
 ## Prepare the plotting data  
  
  # If SVM
  if(svm){
    
    # create svm analysis
    svm.obj <- makeSVM(hm.data,
                       x.field=x.field,
                       y.field=y.field,
                       z.field=color.field,
                       svm.type=svm.opts$type,
                       svm.kernel=svm.opts$kernel,
                       poly.degree=svm.opts$poly.degree,
                       expand.factor=svm.opts$expand.factor)
    
    # Convert initial data to point data
    point.data <- hm.data
    point.data$x <- point.data[,x.field]
    point.data$y <- point.data[,y.field]
    
    # Add predicted values to data
    hm.data <- svm.obj$pred
    names(hm.data) <- c('x.var', 'y.var', 'fill.var')
    
  # if not SVM  
  } else {
    
    # Set up X, Y and fill variables
    hm.data$x.var <- hm.data[ ,x.field]
    hm.data$y.var <- hm.data[ ,y.field]
    hm.data$fill.var <- hm.data[ ,color.field]  
    
  }
  
 ## Make the plot  
  
  # Set up the basics
  hm.plot <- ggplot(data=hm.data,
                    aes(x=x.var, y=y.var))

  # If adding by count
  if(alpha.count){
    
    # If Hex
    if(hexmap){
      hm.plot <- hm.plot + 
        stat_binhex(data=hm.data,
                    aes(alpha=..count.., fill=as.factor(fill.var)),
                    binwidth=bins, 
                    na.rm=T) +
        guides(alpha=FALSE)
    
    # If not hex
    } else {
      hm.plot <- hm.plot + 
        stat_bin2d(data=hm.data,
                   aes(alpha=..count.., fill=as.factor(fill.var)),
                   binwidth=bins) +
        guides(alpha=FALSE)
    }
    
  # if not adding by count  
  } else {
    
    # if hex
    if(hexmap){
      hm.plot <- hm.plot + 
        stat_binhex(data=hm.data,
                    aes(fill=as.factor(fill.var)),
                    binwidth=bins) +
        guides(alpha=FALSE)
      
    # if not hex
    } else {
      hm.plot <- hm.plot + 
        stat_bin2d(data=hm.data,
                   aes(fill=as.factor(fill.var)),
                   binwidth=bins)  +
        guides(alpha=FALSE)
    }
  }
  
  # Adding points
  
  if(add.points){
    if(is.null(point.data)){
      hm.plot <- hm.plot + geom_point(size=.1, color='gray50', alpha=.35, 
                                      show.legend=FALSE)
    } else {
      hm.plot <- hm.plot + geom_point(data=point.data,
                                      aes(x=x, y=y),
                                      size=.1, color='gray50', alpha=.35, 
                                      show.legend=FALSE)
    }
  }
  
  # Add fill legend
  hm.plot <- hm.plot + 
    scale_fill_manual(values=fill.colors,
                      name='',
                      labels=c('Long Term Preferred     ',
                               'Airbnb Preferred        '))
  
 ## Return Values  
  
  # If return SVM data
  if(return.svm){
    return(list(svm=hm.data,
                map=hm.plot))
  
  # if not returnign SVM data  
  } else {
    return(hm.plot)
  }
  
}

### Create SVM prediction data ----------------------------------------------------------- 

makeSVM <- function(svm.data,
                    x.field,
                    y.field,
                    z.field,
                    svm.type='C-svc',
                    svm.kernel='polydot',
                    poly.degree=4,
                    expand.factor=100){
  
  
  # Create XY data
  xy.data <- cbind(svm.data[,x.field], svm.data[,y.field])
  
  # Specificy SVM
  svm.obj <- ksvm(x=xy.data,
                  y=svm.data[,z.field],
                  data=xy.data,
                  type=svm.type,
                  kernel=svm.kernel,
                  kpar=list(degree=poly.degree))
  
  # Add the fitted to the data
  svm.data$fitted <- svm.obj@fitted
  
  ## Make predictions over grid
  
  # Set up grid
  x.range <- max(svm.data[ ,x.field]) - min(svm.data[ ,x.field])
  x.inc <- x.range / expand.factor
  xx.min <- min(svm.data[, x.field]) - x.inc
  xx.max <- max(svm.data[, x.field]) + x.inc
  
  y.range <- max(svm.data[ ,y.field]) - min(svm.data[ ,y.field])
  y.inc <- y.range / expand.factor
  yy.min <- min(svm.data[, y.field]) - y.inc
  yy.max <- max(svm.data[, y.field]) + y.inc
  
  pred.grid <- expand.grid(seq(xx.min, xx.max, x.inc),
                           seq(yy.min, yy.max, y.inc))  
  
  # Make the predictions
  svm.pred <- predict(svm.obj, pred.grid)
  
  # Add predictions to the 
  pred.grid$pred <- svm.pred
  
  ## Return values
  
  return(list(orig=svm.data,
              pred=pred.grid))
  
}


### Calculate the full market score ------------------------------------------------------

calcMarketScore <- function(mrkt.data,
                            calc.field){
  
  bin.count <- table(paste0(round(mrkt.data$rate.qtl, -1), ".", 
                            round(mrkt.data$occ.qtl, -1)),
                     mrkt.data[,calc.field])
  
  bin.dif <-(bin.count[ ,2] - bin.count[ ,1])
  mrkt.value <- length(which(bin.dif > 0)) / length(bin.dif)
  
  return(mrkt.value)
}

### Full market analysis wrapper ---------------------------------------------------------

fullMarketAnalysis <- function(ltr.df,
                               abb.df,
                               ltr.mod.spec,
                               abb.mod.spec,
                               clip.field,
                               market.field='sub.mrkt',
                               mrkt.col=NULL,
                               heat.col=c('red', 'green'))

{  
  
 ## Fix null
  
  if(market.field != 'type'){
    if(length(table(abb.df$type)) == 1 |
       length(table(ltr.df$type)) == 1){
    
      ltr.mod.spec <- update(ltr.mod.spec, . ~ . - as.factor(type))
      abb.mod.spec <- update(abb.mod.spec, . ~ . - as.factor(type))
      
    }
  }
  
    
 ## Make comparison between two markets  
  
  mrkt.comp <- revCompWrapper(ltr.df=ltr.df,
                              abb.df=abb.df,
                              ltr.mod.spec=ltr.mod.spec,
                              abb.mod.spec=abb.mod.spec,
                              clip.field='suburb')   
  
  
 ## Make a simple table of comparison
  
  mrkt.table <- createCompTable(mrkt.comp$abb, mrkt.comp$ltr, market.field)
  
  if(market.field == 'sub.mrkt'){
    mrkt.table$ID <- factor(mrkt.table$ID, 
                            levels=c('city-core', 'city', 'suburban', 'rural', 'beach'))
  }
  
 ## make basic 2x2 comparison
  
  # Set colors
  if(is.null(mrkt.col)){
    mrkt.col <- 1:(nrow(mrkt.table)/4)
  }
  
  # Make Plot
  twotwo.bar.plot <- 
    ggplot(mrkt.table, 
           aes(x=ID, weights=Var, fill=ID)) + 
    geom_bar() +
    facet_grid(est ~ data) +
    scale_fill_manual(values=mrkt.col) +
    xlab('') +
    ylab('% of properties where Airbnb is more profitable') +
    scale_y_continuous(breaks=c(0,.25,.5,.75,1),
                       labels=c('0%', '25%', '50%', '75%', '100%')) +
    theme(legend.position='none')
  
 ## Make simple 1 Actual Airbnb plot  
  
  # Extract data  
  
  abb.act.table <- mrkt.table[mrkt.table$est == 'Actual Rates & Rents' &
                                mrkt.table$data == 'Airbnb', ]
  
  # Make plot
  
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
    coord_cartesian(ylim=c(0, 1))
  
 ## Preferred option by occupancy rate
  
  rawocc.pplot <- makePrefPlot(mrkt.comp$abb,
                               x.field='occ.rate',
                               y.field='abb.act',
                               group.field=market.field,
                               smooth=TRUE,
                               smooth.span=.75)
  rawocc.pplot <- rawocc.pplot +
    xlab('\nOccupancy Rate') +
    ylab('\n% of Properties where Airbnb is more profitable') +
    scale_x_continuous(breaks=seq(0, 100, by=25),
                       labels=c('0%', '25%', '50%', '75%', '100%')) +
    scale_y_continuous(breaks=seq(0, 1, by=.25),
                       labels=c('0%', '25%', '50%', '75%', '100%')) +
    scale_color_manual(values=mrkt.col)
  
 ## Add the quantile locations
  
  mrkt.comp$abb$occ.qtl <- makeWtdQtl(mrkt.comp$abb$occ.rate, 
                                      return.type='rank') 
  mrkt.comp$abb$rate.qtl <- makeWtdQtl(mrkt.comp$abb$med.rate, 
                                       return.type='rank') 
  
  
 ## Make quartile location plot
  
  qtlocc.pplot <- makePrefPlot(mrkt.comp$abb,
                               x.field='occ.qtl',
                               y.field='abb.act',
                               group.field=market.field,
                               smooth=TRUE,
                               smooth.span=.75)
  qtlocc.pplot <- qtlocc.pplot +
    xlab('\nQualtile of Occupancy Rate') +
    ylab('\n% of Properties where Airbnb is more profitable') +
    scale_x_continuous(breaks=seq(0, 100, by=25),
                       labels=c('0', '25th', '50th', '75th', '100th')) +
    scale_y_continuous(breaks=seq(0, 1, by=.25),
                       labels=c('0%', '25%', '50%', '75%', '100%')) +
    scale_color_manual(values=mrkt.col)
  
 ## Make the rate heatmap  
  
  rate.hm <- makeHeatMap(mrkt.comp$abb,
                         x.field='occ.rate',
                         y.field='nightly.rate',
                         color.field='abb.act',
                         bins=c(.05, 25),
                         svm=F, 
                         alpha.count=T,
                         add.points=T,
                         fill.colors=heat.col)
  
  rate.hm <- rate.hm +
    xlab('\n Occupancy Rate') +
    ylab('\n Nightly Rate') +
    scale_x_continuous(breaks=seq(0, 1, by=.25),
                       labels=c('0%', '25%', '50%', '75%', '100%')) +
    theme(legend.position='bottom')
  
  # Rate SVM heatmap
  
  rate.hm.svm <- makeHeatMap(mrkt.comp$abb,
                             x.field='occ.rate',
                             y.field='nightly.rate',
                             color.field='abb.act',
                             bins=c(.05, 25),
                             svm=T, 
                             alpha.count=F,
                             add.points=T,
                             fill.colors=heat.col)
  
  rate.hm.svm <- rate.hm.svm +
    xlab('\n Occupancy Rate') +
    ylab('\n Nightly Rate') +
    scale_x_continuous(breaks=seq(0, 1, by=.25),
                       labels=c('0%', '25%', '50%', '75%', '100%')) +
    theme(legend.position='bottom')
  
  ## Make quartile heat map  
  
  qtl.hm <- makeHeatMap(mrkt.comp$abb,
                        x.field='occ.qtl',
                        y.field='rate.qtl',
                        color.field='abb.act',
                        bins=c(5, 5),
                        svm=F, 
                        alpha.count=T,
                        add.points=T,
                        fill.colors=heat.col)
  
  qtl.hm <- qtl.hm +
    xlab('\n Quantile of Occupancy Rate') +
    ylab('\n Quantile of Nightly Rate') +
    scale_x_continuous(breaks=seq(0, 100, by=25),
                       labels=c('0', '25th', '50th', '75th', '100th')) +
    scale_y_continuous(breaks=seq(0, 100, by=25),
                       labels=c('0', '25th', '50th', '75th', '100th')) +
    theme(legend.position='bottom')
  
  ## Make quartile heat map  
  
  qtl.hm.svm <- makeHeatMap(mrkt.comp$abb,
                            x.field='occ.qtl',
                            y.field='rate.qtl',
                            color.field='abb.act',
                            bins=c(5, 5),
                            svm=T, 
                            alpha.count=F,
                            add.points=T,
                            fill.colors=heat.col)
  
  qtl.hm.svm <- qtl.hm.svm +
    xlab('\n Quantile of Occupancy Rate') +
    ylab('\n Quantile of Nightly Rate') +
    scale_x_continuous(breaks=seq(0, 100, by=25),
                       labels=c('0', '25th', '50th', '75th', '100th')) +
    scale_y_continuous(breaks=seq(0, 100, by=25),
                       labels=c('0', '25th', '50th', '75th', '100th')) +
    theme(legend.position='bottom')
  
 ## Get SVM Counts for market analysis
  
  svm.rate <- makeSVM(mrkt.comp$abb,
                      x.field='occ.rate',
                      y.field='nightly.rate',
                      z.field='abb.act',
                      svm.type='C-svc',
                      svm.kernel='polydot',
                      poly.degree=4,
                      expand.factor=100)
  
  svm.qtl <- makeSVM(mrkt.comp$abb,
                     x.field='occ.qtl',
                     y.field='rate.qtl',
                     z.field='abb.act',
                     svm.type='C-svc',
                     svm.kernel='polydot',
                     poly.degree=4,
                     expand.factor=100)
  
 ## Calculate the full market score  
  
  market.ratio <- data.frame(type=c('rate', 'qtl'),
                             actual=rep(mean(mrkt.comp$abb$abb.act), 2),
                             fitted=c(mean(svm.rate$orig$fitted),
                                      mean(svm.qtl$orig$fitted)),
                             svm=c(mean(svm.rate$pred$pred),
                                   mean(svm.qtl$pred$pred)))
  
 ## Return values
  
  return(list(ltr=mrkt.comp$ltr,
              abb=mrkt.comp$abb,
              mrkt.table=mrkt.table,
              plot.2.2=twotwo.bar.plot,
              plot.1=abb.act.plot,
              rawocc.plot=rawocc.pplot,
              qtlocc.plot=qtlocc.pplot,
              qtl.hm=qtl.hm,
              qtl.hm.svm=qtl.hm.svm,
              rate.hm=rate.hm,
              rate.hm.svm=rate.hm.svm,
              mrkt.score=market.ratio))
}


#   if(mrkt.occ){
#     
#     market <- abb.df[,mrkt]
#     market.df <- tapply(abb.df$occ.rate, market, median)
#     market.df <- as.data.frame(market.df)
#     names(market.df) <- 'occ.rate'
#     market.df$market <- rownames(market.df)
#     rownames(market.df) <- 1:nrow(market.df)
#     
#     abb.df$market.occ <- market.df$occ.rate[match(abb.df[ ,mrkt],
#                                                   market.df$market)]
#     rent.df$market.occ <- market.df$occ.rate[match(rent.df[ ,mrkt],
#                                                    market.df$market)]
#   }
#   
