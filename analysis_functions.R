
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
                             exch.rate,
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
  
  ltr.df$imp.rate <- exp(predict(abb.mod, ltr.df)) * exch.rate

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
  
  return(abb.df)
  
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
  
  return(ltr.df)
  
}

### Compare revenues between abb and ltr -------------------------------------------------

compareRevenues <- function(abb.df, 
                            ltr.df){
  
  ## Abb act (1) to abb imp ltr (4)
  
  abb.df$abb.prem <- abb.df$abb.rev.act - abb.df$ltr.rev.imp
  abb.df$abb.act <- ifelse(abb.df$abb.prem > 0, 1, 0)
  
  ## Abb imp (2) to abb imp ltr (4)
  
  abb.df$abb.prem.imp <- abb.df$abb.rev.imp - abb.df$ltr.rev.imp
  abb.df$abb.imp <- ifelse(abb.df$abb.prem.imp > 0, 1, 0)
  
  ## Abb imp (2) to ltr act (3)
  
  ltr.df$abb.prem <- ltr.df$abb.rev.imp - ltr.df$ltr.rev.act
  ltr.df$abb.act <- ifelse(ltr.df$abb.prem > 0, 1, 0)
  
  ## Abb imp (2) to ltr imp (4)
  
  ltr.df$abb.prem.imp <- ltr.df$abb.rev.imp - ltr.df$ltr.rev.imp
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
                           exch.rate,
                           clip.field='suburb'){  
  
 ## Calculate the actual revenues  
  
  act.revs <- revenueEngine(abb.df, ltr.df) 
  
  # Add back to DFs
  abb.df$abb.rev.act <- act.revs$abb
  ltr.df$ltr.rev.act <- act.revs$ltr
  
 ## Impute rates and rents
  
  imp.data <- imputeRatesRents(ltr.df=ltr.df, 
                               abb.df=abb.df, 
                               ltr.mod.spec=ltr.mod.spec, 
                               abb.mod.spec=abb.mod.spec,
                               exch.rate=exch.rate,
                               clip.field='suburb')
  
  # Extract out DFs
  abb.df <- imp.data$abb
  ltr.df <- imp.data$ltr
  
 ## Within type imputation revenues
  
  imp.revs <- revenueEngine(abb.df, 
                            ltr.df,
                            rate.field='imp.rate',
                            rent.field='imp.rent')
  
  # Add back to DFs
  abb.df$abb.rev.imp <- imp.revs$abb
  ltr.df$ltr.rev.imp <- imp.revs$ltr
  
 ## Cross type revenue estimation
  
  # Apply dom to abb
  abb.df <- imputeDOM(abb.df, ltr.df, calc.type='median')
  
  # Apply occ.rate to ltr
  ltr.df <- imputeOccRate(ltr.df, abb.df, calc.type='median')
  
  # Estimate cross revenues
  impx.revs <- revenueEngine(abb.df=ltr.df, 
                             ltr.df=abb.df,
                             rate.field='imp.rate',
                             rent.field='imp.rent',
                             occ.field='imp.occ',
                             dom.field='imp.dom')
  
   # Add back to DFs
   abb.df$ltr.rev.imp <- impx.revs$ltr
   ltr.df$abb.rev.imp <- impx.revs$abb
  
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
    abb.imp$est <- ltr.imp$est <- 'imputed'
    abb.imp$data <- abb.act$data <- 'abb'
    ltr.act$est <- abb.act$est <- 'actual'
    ltr.act$data <- ltr.imp$data <- 'ltr'
    
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
                         group.field,
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
    
    # Create the table
    pref.table <- tapply2DF(pref.df[ ,y.field], 
                            pref.df[ ,group.field],
                            metric.fnct)
    
    # Add the x variable
    pref.table$x.var <- i.pl
    
    # Add to the capture list
    pref.list[[i.pl]] <- pref.table
    
  }
  
 ## Convert to a data.frame  
  
  pref.full <- rbind.fill(pref.list)
  
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
                        alpha.field,
                        bins=c(10, 10)){
  
  
  hm.data$x.var <- hm.data[ ,x.field]
  hm.data$y.var <- hm.data[ ,y.field]
  hm.data$fill.var <- hm.data[ ,alpha.field]  
  
  hm.plot <- ggplot(data=hm.data,
                    aes(x=x.var, y=y.var)) +
    stat_bin2d(data=hm.data,
               aes(alpha=..count.., fill=as.factor(fill.var)),
               binwidth=bins) +
    scale_fill_manual(values=c('red', 'forestgreen'))
  
  return(hm.plot)
  
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
                               exch.rate,
                               clip.field
)
{  
  
 ## Make comparison between two markets  
  
  mrkt.comp <- revCompWrapper(ltr.df=ltr.df,
                              abb.df=abb.df,
                              ltr.mod.spec=ltr.mod.spec,
                              abb.mod.spec=abb.mod.spec,
                              exch.rate=exch.rate,
                              clip.field='suburb')   
  
  
 ## Make a simple table of comparison
  
  mrkt.table <- createCompTable(mrkt.comp$abb, mrkt.comp$ltr, 'sub.mrkt')
  
 ## make basic 2x2 comparison
  
  twotwo.bar.plot <- ggplot(mrkt.table, aes(x=ID, weights=Var, fill=ID)) + 
    geom_bar() +
    facet_grid(data ~ est)
  
 ## Preferred option by occupancy rate
  
  rawocc.pplot <- makePrefPlot(mrkt.comp$abb,
                               x.field='occ.rate',
                               y.field='abb.act',
                               group.field='sub.mrkt',
                               smooth=TRUE,
                               smooth.span=.35)
  
 ## Add the quantile location
  
  mrkt.comp$abb$occ.qtl <- makeWtdQtl(mrkt.comp$abb$occ.rate, 
                                      return.type='rank') 
  
 ## Make quartile location plot
  
  qtlocc.pplot <- makePrefPlot(mrkt.comp$abb,
                               x.field='occ.qtl',
                               y.field='abb.act',
                               group.field='sub.mrkt',
                               smooth=TRUE,
                               smooth.span=.35)
  
 ## Add the rate quartle location   
  
  mrkt.comp$abb$rate.qtl <- makeWtdQtl(mrkt.comp$abb$med.rate, 
                                       return.type='rank') 
  
 ## Make quartile heat map  
  
  qtl.heatmap <- makeHeatMap(mrkt.comp$abb,
                             x.field='occ.qtl',
                             y.field='rate.qtl',
                             alpha.field='abb.act',
                             bins=c(5,5))
  
 ## Make the rate heatmap  
  
  rate.heatmap <- makeHeatMap(mrkt.comp$abb,
                              x.field='occ.rate',
                              y.field='med.rate',
                              alpha.field='abb.act',
                              bins=c(.05, 30))
  
 ## Calculate the overall market score
  
  market.score <- calcMarketScore(mrkt.comp$abb,
                                  calc.field='abb.act')
  
 ## Return values
  
  return(list(ltr=mrkt.comp$ltr,
              abb=mrkt.comp$abb,
              mrkt.table=mrkt.table,
              plot.2.2=twotwo.bar.plot,
              rawocc.plot=rawocc.pplot,
              qtlocc.plot=qtlocc.pplot,
              qtl.hm=qtl.heatmap,
              rate.hm=rate.heatmap,
              mrkt.score=market.score))
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
