
















### Calculate the occupancy rates for each date in a range -------------------------------

abbCalcDateRates <- function(daily.df){
  
  ## Create table of counts by status
  
  status.table <- table(daily.df$date, daily.df$status)
  
  ## Convert to a data.frame  
  
  status.df <- data.frame(Avail=status.table[,1],
                          Resv=status.table[,2],
                          Block=status.table[,3],
                          lp.type=daily.df$lp.type[1])
  
  ## Add date   
  
  status.df$date <- as.Date(rownames(status.df))
  
  ## Add reserved rate (occupancy rate)  
  
  status.df$occ.rate <- status.df$Resv / (status.df$Resv + status.df$Avail)
  
  ## Return values  
  
  return(status.df)
  
}

### Impute the missing daily observations ------------------------------------------------

abbImputeDaily <- function(prop.df, 
                           daily.df, 
                           rates.df){
  
  # prop.df: data.frame of AirBNB property data
  # daily.df: data.frame of existing AirBNB daily observations
  # rates.df: data.frame of daily rate data (from abbCalcDateRates() function)
  
  ## Isolate those needing imputation  
  
  # Isolate
  imp.df <- prop.df[prop.df$impute != 'no',]
  
  # Compute property specific rates
  imp.df$avail.rate <- imp.df$available.days/imp.df$total.days
  imp.df$block.rate <- imp.df$blocked.days/imp.df$total.days
  imp.df$resv.rate <- imp.df$reserved.days/imp.df$total.days
  
  ## Loop through each property and impute  
  
  # Create capture list
  imp.list <- list()
  
  # Start loop
  for(i.i in 1:nrow(imp.df)){
    
    # Extract ith property data and daily data
    i.imp <- imp.df[i.i, ]
    i.daily <- daily.df[daily.df$property.id == i.imp$property.id,]
    i.min <- min(i.daily$date)
    
    # Calculate the days that are missing
    miss.days <- rates.df$date[!rates.df$date %in% i.daily$date]
    miss.days <- miss.days[miss.days < i.min]
    
    # Calculate the occupancy rates for missing and matching days
    miss.rates <- rates.df[rates.df$date %in% miss.days, ]
    match.rates <- rates.df[!rates.df$date %in% miss.days, ]
    
    # Make adjustment to the property specific occ rates based on observed days bias
    resv.adj <- median(miss.rates$occ.rate) / median(match.rates$occ.rate)
    i.resrate <- i.imp$resv.rate * resv.adj
    i.blockrate <- i.imp$block.rate
    i.availrate <- 1 - i.blockrate - i.resrate
    
    # Create an day-wise adjustment figure for each day
    miss.adj <- miss.rates$occ.rate / mean(miss.rates$occ.rate)
    
    # Estimate number of day of each status that should occur
    res.count <- round(i.resrate * length(miss.days), 0)
    avail.count <- round(i.availrate * length(miss.days), 0)
    block.count <- length(miss.days) - res.count - avail.count
    if(avail.count <= 0){
      block.count <- block.count + avail.count
      avail.count <- 0
    }
    
    # Select a random # of blocked dates
    blocked <- sample(1:length(miss.days), block.count)
    
    # Remove blocked
    if(length(blocked) > 0){
      miss.rem <- miss.adj[-blocked]
      rem.dates <- miss.days[-blocked]
      block.dates <- miss.days[blocked]
    } else {
      miss.rem <- miss.adj
      rem.dates <- miss.days
      block.dates <- NULL
    }
    
    # Set up probability of remaining dates reserved
    resv.prob <- runif(res.count + avail.count)
    resv.prob.adj <- resv.prob * miss.rem
    
    # Select reserved and available dates by random
    res.id <- order(resv.prob.adj, decreasing=T)[1:res.count]
    if(length(res.id) == 0 | is.na(res.id)){
      res.dates <- avail.dates <- NULL
    } else {
      res.dates <- rem.dates[res.id]
      avail.dates <- rem.dates[-res.id] 
    }
    
    # Build data.frame of new results
    new.daily <- data.frame(property.id=i.imp$property.id,
                            date=c(res.dates, avail.dates, block.dates),
                            status=c(rep('R', length(res.dates)),
                                     rep('A', length(avail.dates)),
                                     rep('B', length(block.dates))),
                            price=i.daily$price[1],
                            booked.date='imputed',
                            reservation.id=NA,
                            lp.type=i.imp$lp.type)
    
    # Fix if numeric dates
    if(class(new.daily$date) == 'numeric'){
      new.daily$date <- as.Date(new.daily$date, origin='1970-01-01')
    }
    
    # Order by date
    new.daily <- new.daily[order(new.daily$date), ]
    
    # Add to capture list
    imp.list[[i.i]] <- new.daily
  }
  
  ## Convert to a data.frame
  
  imp.daily <- rbind.fill(imp.list)
  
  ## Return Values  
  
  return(imp.daily)
  
}








### Compare revenues between abb and ltr -------------------------------------------------

compareRevenues <- function(str.df){
  
  ## Actual Revenue
  
  str.df$str.obs.prem <- str.df$str.obs.revenue - str.df$ltr.imp.revenue
  str.df$str.obs.pref <- ifelse(str.df$str.obs.prem > 0, 1, 0)
  
  ## Extrapolated Revenue
  
  str.df$str.act.prem <- str.df$str.act.revenue - str.df$ltr.imp.revenue
  str.df$str.act.pref <- ifelse(str.df$str.act.prem > 0, 1, 0)
  
  ## Potential Revenue
  
  str.df$str.pot.prem <- str.df$str.pot.revenue - str.df$ltr.imp.revenue
  str.df$str.pot.pref <- ifelse(str.df$str.pot.prem > 0, 1, 0)
  
  ## Return Values 
  
  return(str.df[,c('property.id', 'str.obs.prem', 'str.obs.pref',
                   'str.act.prem', 'str.act.pref', 
                   'str.pot.prem', 'str.pot.pref')])  
  
}





### Creating preference plots ------------------------------------------------------------  

abbPrefPlot <- function(pref.data,
                        x.field,
                        split.field='none',
                        metric='mean',
                        cumulative=FALSE,
                        quartile=FALSE,
                        smooth=FALSE,
                        smooth.span=.15,
                        spl.col){
  
  
  ## Fixing some variables
  
  if(x.field == 'occ' | x.field == 'occ.rate'){
    
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
      pref.df <- pref.data[pref.data[ ,x.field] >= i.pl, ]
    } else {
      pref.df <- pref.data[pref.data[ ,x.field] == i.pl, ]
    }
    
    
    if(split.field != 'none'){
      
      if(nrow(pref.df) > 0){
        # Create the table
        pref.table <- tapply2DF(pref.df$pref, 
                                pref.df[ ,split.field],
                                metric.fnct)
      } else {
        pref.table <- NULL
      }  
    } else {
      
      pref.table <- data.frame(ID='all', 
                               Var=metric.fnct(pref.df$pref, na.rm=T))
    }
    
    if(nrow(pref.df) > 0){
      
      # Add the x variable
      pref.table$x.var <- i.pl
      
      # Add to the capture list
      pref.list[[i.pl]] <- pref.table
    }
  }
  
  ## Convert to a data.frame  
  
  pref.full <- rbind.fill(pref.list)
  
  ## Fix Factor Levels
  
  if(split.field == 'geo.mrkt'){
    pref.full$ID <- factor(pref.full$ID, 
                           levels=c('city-core', 'city', 'suburban', 'rural', 'beach'))
  }
  if(split.field == 'host.type'){
    pref.full$ID <- factor(pref.full$ID, 
                           levels=c('Profit Seeker', 'Opportunistic Sharer', 
                                    'Multi-Platform User', 'Unknown'))
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
  
  ## Add specific plot outputs
  
  if(x.field == 'occ'){
    pref.plot <- pref.plot + 
      xlab('\nOccupancy Rate') +
      scale_x_continuous(breaks=seq(0, 100, by=25),
                         labels=c('0%', '25%', '50%', '75%', '100%')) 
  }
  if(x.field == 'occ.qtl'){
    pref.plot <- pref.plot + 
      xlab('\nOccupancy Rate (Quantile)') +
      scale_x_continuous(breaks=seq(0, 100, by=25),
                         labels=c('0th', '25th', '50th', '75th', '100th')) 
  }
  
  
  
  ## Add global plot options
  
  pref.plot <- pref.plot +  
    ylab('\n% of Properties where STR is Preferable') +
    scale_y_continuous(breaks=seq(0, 1, by=.25),
                       labels=c('0%', '25%', '50%', '75%', '100%')) +
    scale_colour_manual(values=spl.col,
                        name='')
  
  
  ## Return Values
  
  return(list(data=pref.full,
              plot=pref.plot))
  
}


### Create heatmaps of profitability -----------------------------------------------------

abbHeatMap <- function(hm.data,
                       x.field,
                       y.field,
                       pref.field,
                       bins=NULL,
                       fill.colors=c('red', 'forestgreen'),
                       alpha.count=TRUE,
                       alpha.fill=1,
                       add.points=FALSE,
                       hexmap=FALSE,
                       point.data=NULL,
                       svm=FALSE,
                       quantile=FALSE,
                       return.svm=FALSE,
                       svm.opts=list(type='C-svc',
                                     kernel='polydot',
                                     poly.degree=2,
                                     expand.factor=100)){
  
  ## Set bins
  
  if(is.null(bins)){
    bins <- c(0, 0)
    if(x.field == 'occ' | x.field == 'occ.rate') bins[1] <- .05
    if(x.field == 'occ.qtl') bins[1] <- 5
    if(y.field == 'med.rate') bins[2] <- 25
    if(y.field == 'rate.qtl') bins[2] <- 5
  }
  
  ## Prepare the plotting data  
  
  # If SVM
  if(svm){
    
    # create svm analysis
    svm.obj <- makeSVM(hm.data,
                       x.field=x.field,
                       y.field=y.field,
                       z.field=pref.field,
                       svm.type=svm.opts$type,
                       svm.kernel=svm.opts$kernel,
                       poly.degree=svm.opts$poly.degree,
                       expand.factor=svm.opts$expand.factor,
                       quantile=quantile,
                       bins=bins)
    
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
    hm.data$fill.var <- hm.data[ ,pref.field] 
    
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
  
  ## Tidy up plot
  
  if(x.field == 'occ' | x.field == 'occ.rate'){
    hm.plot <- hm.plot +
      xlab('\n Occupancy Rate') +
      scale_x_continuous(breaks=seq(0, 1, by=.25),
                         labels=c('0%', '25%', '50%', '75%', '100%'))  
  }
  if(x.field == 'occ.qtl'){
    hm.plot <- hm.plot +
      xlab('\n Occupancy Rate (Quantile)') +
      scale_x_continuous(breaks=seq(0, 100, by=25),
                         labels=c('0th', '25th', '50th', '75th', '100th'))  
  }
  if(y.field == 'nightly.rate'){
    hm.plot <- hm.plot +
      ylab('\n Nightly Rate') 
  }
  if(y.field == 'rate.qtl'){
    hm.plot <- hm.plot +
      ylab('\n Nightly Rate (Quantile)') +
      scale_y_continuous(breaks=seq(0, 100, by=25),
                         labels=c('0th', '25th', '50th', '75th', '100th'))  
  }
  
  # Add fill legend
  hm.plot <- hm.plot + 
    scale_fill_manual(values=fill.colors,
                      name='',
                      labels=c('Long Term Preferred     ',
                               'Short Term Preferred     ')) +
    theme(legend.position='bottom')
  
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
                    expand.factor=100,
                    quantile=FALSE,
                    bins=c(1, 1)){
  
  
  # Create XY data
  xy.data <- cbind(svm.data[,x.field], svm.data[,y.field])
  
  # Hack if all 0s
  if(sum(svm.data[,z.field]) == 0) svm.data[1,z.field] <- 1
  
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
  if(quantile){
    
    pred.grid <- expand.grid(seq(.5, 99.5, by=bins[1]),
                             seq(.5, 99.5, by=bins[2]))
    
  } else {
    
    x.range <- max(svm.data[ ,x.field]) - min(svm.data[ ,x.field])
    x.inc <- x.range / expand.factor
    xx.min <- min(svm.data[, x.field]) 
    xx.max <- max(svm.data[, x.field]) 
    
    y.range <- max(svm.data[ ,y.field]) - min(svm.data[ ,y.field])
    y.inc <- y.range / expand.factor
    yy.min <- min(svm.data[, y.field]) 
    yy.max <- max(svm.data[, y.field]) 
    
    pred.grid <- expand.grid(seq(xx.min, xx.max, x.inc),
                             seq(yy.min, yy.max, y.inc)) 
    
  }
  
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


### Full wrapper for the analysis and viz functions --------------------------------------

abbPrefAnalysisViz <- function(ic.df,
                               pref.type,
                               split.field,
                               facet.field){
  
  
  ## Set preference and occupancy rates
  
  if(pref.type == 'Actual'){
    ic.df$pref <- ic.df$str.act.pref
    ic.df$diff <- ic.df$str.act.prem
    ic.df$occ <- ic.df$occ.rate
    ic.df$occ.qtl <- ic.df$occ.qtl
  }
  if(pref.type == 'Extrapolated'){
    ic.df$pref <- ic.df$str.ext.pref
    ic.df$diff <- ic.df$str.ext.prem
    ic.df$occ <- ic.df$occ.rate
    ic.df$occ.qtl <- ic.df$occ.qtl
  }
  if(pref.type == 'Potential'){
    ic.df$pref <- ic.df$str.pot.pref
    ic.df$diff <- ic.df$str.pot.prem
    ic.df$occ <- ic.df$pot.occ.rate
    ic.df$occ.qtl <- ic.df$pot.occ.qtl
  }
  
  ## Make the basic comparison table
  
  # Full preference table
  pref.table <- abbCreateCompTable(ic.df=ic.df,
                                   split.field=split.field)
  
  # Convert to wide format for output
  pref.table.wide <- dcast(pref.table, ID ~ rev.type, value.var='Var')
  
  # Limit to selected preference rate measure
  pref.table <- pref.table[pref.table$rev.type == pref.type, ]
  
  ## Make the basic comparison bar chart  
  
  # Set colors
  
  spl.col <- 1:nrow(pref.table)
  
  if(split.field == 'geo.mrkt'){
    spl.col <- c("#FA5863", "#00758C", "#FCB30E", "#4DE26E", "#8B0E52")
  }
  
  if(split.field == 'host.type'){
    spl.col <- c("#8B0E52", "#04D3BF", "#565E61", "#9CA19B")
  }
  
  # Build the bar chart
  pref.bar <- 
    ggplot(pref.table, aes(x=ID, weights=Var, fill=ID)) + 
    geom_bar() +
    scale_fill_manual(values=spl.col) +
    xlab('') +
    ylab('% of properties where STR is preferred') +
    scale_y_continuous(breaks=c(0, .25, .5, .75, 1),
                       labels=c('0%', '25%', '50%', '75%', '100%')) +
    theme(legend.position='none') +
    coord_cartesian(ylim=c(0, 1))
  
  ## 1 Dimensional Pref Plots (Occ.Rate)
  
  occ.1pp <- abbPrefPlot(pref.data=ic.df,
                         x.field='occ',
                         split.field=split.field,
                         smooth=TRUE,
                         smooth.span=.66,
                         spl.col=spl.col)
  
  occq.1pp <- abbPrefPlot(pref.data=ic.df,
                          x.field='occ.qtl',
                          split.field=split.field,
                          smooth=TRUE,
                          smooth.span=.66,
                          spl.col=spl.col)
  
  ## 2 Dimensional Pref Plots (Occ + Nightly Rate)
  
  # Rate v Rate
  rate.hm <- abbHeatMap(ic.df,
                        x.field='occ',
                        y.field='nightly.rate',
                        pref.field='pref',
                        svm=F, 
                        alpha.count=T,
                        add.points=T,
                        fill.colors=c(abb.col[1], abb.col[5]))
  
  # Rate v Rate SVM
  rate.hm.svm <- abbHeatMap(ic.df,
                            x.field='occ',
                            y.field='nightly.rate',
                            pref.field='pref',
                            svm=T, 
                            alpha.count=F,
                            add.points=T,
                            fill.colors=c(abb.col[1], abb.col[5]))
  
  # Qtl vs Qtl
  qtl.hm <- abbHeatMap(ic.df,
                       x.field='occ.qtl',
                       y.field='rate.qtl',
                       pref.field='pref',
                       svm=F, 
                       alpha.count=T,
                       add.points=T,
                       fill.colors=c(abb.col[1], abb.col[5]))
  
  # QTL vs QTL SVM
  qtl.hm.svm <- abbHeatMap(ic.df,
                           x.field='occ.qtl',
                           y.field='rate.qtl',
                           pref.field='pref',
                           svm=T, 
                           alpha.count=F,
                           add.points=T,
                           quantile=T,
                           fill.colors=c(abb.col[1], abb.col[5]))
  
  ## Make Market Stats Table
  
  # Rate Numbers
  svm.rate <- makeSVM(ic.df,
                      x.field='occ.rate',
                      y.field='nightly.rate',
                      z.field='str.act.pref',
                      svm.type='C-svc',
                      svm.kernel='polydot',
                      poly.degree=4,
                      expand.factor=100)
  
  # Qtl Numbers
  svm.qtl <- makeSVM(ic.df,
                     x.field='occ.qtl',
                     y.field='rate.qtl',
                     z.field='str.act.pref',
                     svm.type='C-svc',
                     svm.kernel='polydot',
                     poly.degree=4,
                     expand.factor=100)
  
  # Complete Table
  market.stats <- data.frame(type=c('rate', 'qtl'),
                             actual=rep(mean(str_df.ic$str.act.pref), 2),
                             fitted=c(mean(svm.rate$orig$fitted),
                                      mean(svm.qtl$orig$fitted)),
                             svm=c(mean(svm.rate$pred$pred),
                                   mean(svm.qtl$pred$pred)))
  
  ## Return Values
  
  return(list(pref.table=pref.table,
              pref.table.wide=pref.table.wide,
              pref.bar=pref.bar,
              occ.1pp=occ.1pp,
              occq.1pp=occq.1pp,
              rate.hm=rate.hm,
              rate.hm.svm=rate.hm.svm,
              qtl.hm=qtl.hm,
              qtl.hm.svm=qtl.hm.svm,
              market.stats=market.stats))
  
  
}



### Custom function for building revenue density plots -----------------------------------


buildRevDensPlot <- function(str_df,
                             rev.types=c('act', 'pot'),
                             rev.cols=1:(length(rev.types) + 1),
                             facet=TRUE){
  
  ## Build the dataset  
  
  # Actual
  if('act' %in% rev.types){
    stra.rev <- str_df[,c('property.id', 'str.act.revenue')]
    stra.rev$tenure <- 'Short-Term (Actual)   '
    names(stra.rev)[2] <- 'revenue'
  } else {
    stra.rev <- NULL
  }
  
  # Potential
  if('pot' %in% rev.types){
    strp.rev <- str_df[,c('property.id', 'str.pot.revenue')]
    strp.rev$tenure <- 'Short-Term (Potential)   '
    names(strp.rev)[2] <- 'revenue'
  } else {
    strp.rev <- NULL
  }
  
  # Long-Term
  ltr.rev <- str_df[,c('property.id', 'ltr.imp.revenue')]
  ltr.rev$tenure <- 'Long-Term   '
  names(ltr.rev)[2] <- 'revenue'
  
  # Combine
  revdens.data <- rbind(stra.rev, strp.rev, ltr.rev)
  
  ## Build a plot
  
  rd.plot <- ggplot(revdens.data, 
                    aes(x=revenue, fill=tenure, color=tenure)) +
    geom_density(alpha=.5) +
    scale_fill_manual(values=rev.cols) +
    scale_color_manual(values=rev.cols) +
    xlab('\nAnnual Revenue') +
    scale_x_continuous(breaks=c(seq(0, 75000, by=25000)),
                       labels=c('$0', '$25k', '$50k', '$75k'))+
    theme(legend.position='none',
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y=element_blank()) +
    coord_cartesian(xlim=c(0, 85000))
  
  if(facet){
    rd.plot <- rd.plot + facet_wrap(~tenure)
  }
  
  ## Return 
  
  return(rd.plot)
  
}  

