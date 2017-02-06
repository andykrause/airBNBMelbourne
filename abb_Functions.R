##########################################################################################
#                                                                                        #    
#  Custom functions for working with APM and AirDNA data in the ABB Project              #
#                                                                                        #
##########################################################################################

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

### Correct the APM data dates -----------------------------------------------------------

apmFixDates <- function(x.date){
  
  # x.date:  date vector
  
  ## Convert to character (from factor or numberic)
  
  temp.date <- as.character(x.date)
  
  ## Remove Time suffixes  
  
  temp.date <- str_replace_all(temp.date, ' 0:00', '')
  
  ## Standardize all years to 2000s  
  
  temp.date <- str_replace_all(temp.date, '/20', '/')
  
  ## Return values as date format  
  
  return(as.Date(temp.date, "%d/%m/%y"))   
}

### Calculate the booking status ---------------------------------------------------------

abbCalcBookStr <- function(id, 
                           book.data){
  
  id.book.data <- book.data[book.data$property.id == id, ]
  id.data <- id.book.data$status
  
  if(length(id.data) > 1){
    
    # Find min and max date
    id.min <- min(id.book.data$date)
    id.max <- max(id.book.data$date)
    
    # Divide by status and collapse
    status.string <- id.data[1]
    
    for(ss in 2:length(id.data)){
      
      if(id.data[ss] == id.data[[ss - 1]]){
        status.string <- paste0(status.string, id.data[[ss]])
      } else {
        status.string <- paste0(status.string, '.' ,id.data[[ss]])
      }
    }
    
    # Collapse into list objects
    ss.list <- as.list(strsplit(status.string, '[.]')[[1]])
    
    # Grab the first status of each
    sg.obj <- substr(ss.list[[1]], 1, 1)
    for(sg in 1:length(ss.list)){
      sg.obj[sg] <- substr(ss.list[[sg]], 1, 1)  
    }
    
    # Find location of three types
    id.B <- which(unlist(sg.obj) == 'B')
    id.R <- which(unlist(sg.obj) == 'R')
    id.A <- which(unlist(sg.obj) == 'A')
    
    # Extract
    if(length(id.R) > 0){
      r.list <- ss.list[id.R]
    }
    if(length(id.A) > 0){
      a.list <- ss.list[id.A]
      avails <- unlist(lapply(a.list, nchar))
      avail.rate <- sum(avails) / length(id.data)
      
    } else {
      
      avail.rate <- 0
      
    }
    
    if(length(id.B) > 0){
      b.list <- ss.list[id.B] 
      
      # Count longest and blocked times
      blocks <- unlist(lapply(b.list, nchar))
      
      block.rate <- sum(blocks) / length(id.data)
      longest.block <- max(blocks)
      med.block <- median(blocks)
      nbr.block <- length(id.B)
      
    } else {
      
      block.rate <- 0
      longest.block <- 0
      med.block <- 0
      nbr.block <- 0
      
    }
    
    total.days <- length(id.data)  
    
  } else {
    
    block.rate <- NA
    longest.block <- NA
    med.block <- NA
    nbr.block <- NA
    total.days <- NA
    id.min <- NA
    id.max <- NA
    avail.rate <- NA

  }  
  
  ## Return Values
  
  return(data.frame(id=id,
                    min.date=id.min,
                    max.date=id.max,
                    total.days=total.days,
                    block.rate=block.rate,
                    avail.rate=avail.rate,
                    longest.block=longest.block,
                    nbr.block=nbr.block,
                    med.block=med.block))
}

### Set the cleaning counter -------------------------------------------------------------

setCleanCount <- function(){
  
  # Make counts of initial sizes
  str.orig <- nrow(str.data)
  daily.orig <- nrow(daily.data)
  ltr.orig <- nrow(ltr.data)
  list.orig <- nrow(listing.data)
  
  # Create initial data.frame
  clean.df <- data.frame(operation='initial',
                         str=str.orig,
                         daily=daily.orig,
                         ltr=ltr.orig,
                         list=list.orig)
  
  # Assign initial values to globalEnv
  assign('clean.count', clean.df, envir=.GlobalEnv)
  assign('str.run.total', nrow(str.data), envir=.GlobalEnv)
  assign('ltr.run.total', nrow(ltr.data), envir=.GlobalEnv)
  assign('list.run.total', nrow(listing.data), envir=.GlobalEnv)
  assign('daily.run.total', nrow(daily.data), envir=.GlobalEnv)
  
}

### Cleaning counting updater ------------------------------------------------------------

countCleaning <- function(operation){

  # Count recent cuts  
  str.cut <- str.run.total - nrow(str.data)
  daily.cut <- daily.run.total - nrow(daily.data)
  ltr.cut <- ltr.run.total - nrow(ltr.data)
  listing.cut <- list.run.total - nrow(listing.data)
  
  # Build new dataframe
  new.df <- data.frame(operation=operation,
                       str=str.cut,
                       daily=daily.cut,
                       ltr=ltr.cut,
                       list=listing.cut)
  
  # Add to existing DF
  comb.df <- rbind(clean.count, new.df)
  
  # Assign temp data to globalEnv
  assign('clean.count', comb.df, envir=.GlobalEnv)
  assign('str.run.total', nrow(str.data), envir=.GlobalEnv)
  assign('ltr.run.total', nrow(ltr.data), envir=.GlobalEnv)
  assign('list.run.total', nrow(listing.data), envir=.GlobalEnv)
  assign('daily.run.total', nrow(daily.data), envir=.GlobalEnv)
  
}

### Cross impute rates and rents ---------------------------------------------------------

imputeRents <- function(ltr.df,
                        str.df,
                        mod.spec,
                        clip.fields=NULL)
{
  
  ## Arguments
  
  # ltr.df:  data.frame of long term rental observations
  # str.df:  data.frame of airbnb properties
  # ltr.mod.spec:  specification for rent price model
  # str.mod.spec:  specification for airbnb properties
  # clip.field: field to ensure factors match between rent and str
  
  ## Remove those within the clip field that isn't present in both  
  
  if(!is.null(clip.fields)){
    
    for(i.cf in 1:length(clip.fields)){
      
      # Find the fields that are used to clip
      l.cf <- which(names(ltr.df) == clip.fields[i.cf])
      s.cf <- which(names(str.df) == clip.fields[i.cf])
      
      # Get IDs for those to be removed
      id.l <- ltr.df[ ,l.cf] %in% names(table(as.character(str.df[ ,s.cf])))
      id.s <- str.df[ ,s.cf] %in% names(table(as.character(ltr.df[ ,l.cf])))
      
      # Filter out obs missing matched factors  
      ltr.df <- ltr.df[id.l, ]
      str.df <- str.df[id.s, ]
      
    }
    
  }
  
  ## Add the monthly factors
  
  str.df$ltr.month <- 12
  
  ## Build regression models for rental values
  
  ltr.mod <- lm(mod.spec, data=ltr.df)
  
  ## Add the predicted values to the short term data
  
  imp.rent <- exp(predict(ltr.mod, str.df))

  ## Return Values  
  
  return(list(str=data.frame(property.id=str.df$property.id,
                             imp.rent=imp.rent),
              mod=ltr.mod))
              
}

### Basic engine for calculating the revenues for abb and ltr ----------------------------

revenueEngine <- function(str.df,
                          ltr.df,
                          rate.field='med.rate',
                          rent.field='event.price',
                          occ.field='occ.rate',
                          dom.field='dom',
                          cost.list=NULL)
{
  
  ## Calculate Airbnb Revenues  
  
  str.rev <- str.df[, rate.field] * 365 * str.df[ ,occ.field]
  
  ## Calculate long term rental revenues  
  
  ltr.rev <- ltr.df[, rent.field] * (52 - (ltr.df[ ,dom.field] / 7))
  
  ## Save for future costs
  
  ## Return values  
  
  return(list(str=str.rev,
              ltr=ltr.rev))
  
}

### Impute days on market for the airbnb properties --------------------------------------

imputeDOM <- function(str.df,
                      ltr.df,
                      calc.type='median'){
  
  ## If median type  
  
  if(calc.type == 'median'){
    dom.qtl <- makeWtdQtl(ltr.df$dom, return.type='raw')
    str.df$imp.dom <- dom.qtl[51]
  }
  
  ## if model type  
  
  if(calc.type == 'model'){
    
    # Save for later
    
  }
  
  ## Return Values
  
  return(str.df$imp.dom)
  
}

### Impute occupancy rate for the ltr properties -----------------------------------------

imputeOccRate <- function(ltr.df,
                          str.df, 
                          calc.type='median',
                          submarket=NULL){
  
  ## If median type  
  
  if(calc.type == 'median'){
    occ.qtl <- makeWtdQtl(str.df$occ.rate, return.type='raw',
                          wgts=(str.df$bookings + str.df$available.days))
    ltr.df$imp.occ <- occ.qtl[51]
  }
  
  ## If model type
  
  if(calc.type == 'model'){
    
    # Save for later
    
  }
  
  ## Return Values
  
  return(ltr.df$imp.occ)
  
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

### Compare revenues between abb and ltr -------------------------------------------------

compareRevenues <- function(str.df){
  
  ## Actual Revenue
  
  str.df$str.act.prem <- str.df$str.act.revenue - str.df$ltr.imp.revenue
  str.df$str.act.pref <- ifelse(str.df$str.act.prem > 0, 1, 0)
  
  ## Extrapolated Revenue
  
  str.df$str.ext.prem <- str.df$str.ext.revenue - str.df$ltr.imp.revenue
  str.df$str.ext.pref <- ifelse(str.df$str.ext.prem > 0, 1, 0)
  
  ## Potential Revenue
  
  str.df$str.pot.prem <- str.df$str.pot.revenue - str.df$ltr.imp.revenue
  str.df$str.pot.pref <- ifelse(str.df$str.pot.prem > 0, 1, 0)
  
  ## Return Values 
  
  return(str.df[,c('property.id', 'str.act.prem', 'str.act.pref',
                   'str.ext.prem', 'str.ext.pref', 
                   'str.pot.prem', 'str.pot.pref')])  
  
}

### Create comparison table --------------------------------------------------------------

createCompTable <- function(str.df,
                            split.field=NULL){
  
  if(split.field == 'none'){
    
    str.act <- mean(str.df$str.act.pref)
    str.ext <- mean(str.df$str.ext.pref)
    str.pot <- mean(ltr.df$str.pot.pref)
    
    rate.table <- data.frame(ID='all',
                             var=c(str.act, str.ext, str.pot),
                             rev.type=c('Actual', 'Extrapolated', 
                                        'Potential'))
    
  } else {
    
    # Calculate cross-tab values
    str.act <- tapply2DF(str.df$str.act.pref, str.df[ ,split.field], mean)
    str.ext <- tapply2DF(str.df$str.ext.pref, str.df[ ,split.field], mean)
    str.pot <- tapply2DF(str.df$str.pot.pref, str.df[, split.field], mean)
    
    # Add names
    str.act$rev.type <- 'Actual'
    str.ext$rev.type <- 'Extrapolated'
    str.pot$rev.type <- 'Potential'
    
    # Combine into table
    rate.table <- rbind(str.act, str.ext, str.pot)
    
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
  
  if(x.field == 'occ.rate' | x.field == 'pot.occ.rate'){
    
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
  if(group.field == 'host.type'){
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
                               str.df,
                               ltr.mod.spec,
                               str.mod.spec,
                               clip.field,
                               market.field='none',
                               mrkt.col=NULL,
                               heat.col=c('red', 'green'))
  
{  
  
  ## Fix null
  
  if(market.field != 'type'){
    if(length(table(str.df$type)) == 1 |
       length(table(ltr.df$type)) == 1){
      
      ltr.mod.spec <- update(ltr.mod.spec, . ~ . - as.factor(type))
      str.mod.spec <- update(str.mod.spec, . ~ . - as.factor(type))
      
    }
  }
  
  
  ## Make comparison between two markets  
  
  mrkt.comp <- revCompWrapper(ltr.df=ltr.df,
                              str.df=str.df,
                              ltr.mod.spec=ltr.mod.spec,
                              str.mod.spec=str.mod.spec,
                              clip.field='suburb')   
  
  
  ## Make a simple table of comparison
  
  mrkt.table <- createCompTable(mrkt.comp$str, mrkt.comp$ltr, market.field)
  
  if(!is.null(market.field) && market.field == 'sub.mrkt'){
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
  
  str.act.table <- mrkt.table[mrkt.table$est == 'Actual Rates & Rents' &
                                mrkt.table$data == 'Airbnb', ]
  
  # Make plot
  
  str.act.plot <- 
    ggplot(str.act.table, 
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
  
  rawocc.pplot <- makePrefPlot(mrkt.comp$str,
                               x.field='occ.rate',
                               y.field='str.act',
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
  
  mrkt.comp$str$occ.qtl <- makeWtdQtl(mrkt.comp$str$occ.rate, 
                                      return.type='rank') 
  mrkt.comp$str$rate.qtl <- makeWtdQtl(mrkt.comp$str$med.rate, 
                                       return.type='rank') 
  
  
  ## Make quartile location plot
  
  qtlocc.pplot <- makePrefPlot(mrkt.comp$str,
                               x.field='occ.qtl',
                               y.field='str.act',
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
  
  rate.hm <- makeHeatMap(mrkt.comp$str,
                         x.field='occ.rate',
                         y.field='nightly.rate',
                         color.field='str.act',
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
  
  rate.hm.svm <- makeHeatMap(mrkt.comp$str,
                             x.field='occ.rate',
                             y.field='nightly.rate',
                             color.field='str.act',
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
  
  qtl.hm <- makeHeatMap(mrkt.comp$str,
                        x.field='occ.qtl',
                        y.field='rate.qtl',
                        color.field='str.act',
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
  
  qtl.hm.svm <- makeHeatMap(mrkt.comp$str,
                            x.field='occ.qtl',
                            y.field='rate.qtl',
                            color.field='str.act',
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
  
  svm.rate <- makeSVM(mrkt.comp$str,
                      x.field='occ.rate',
                      y.field='nightly.rate',
                      z.field='str.act',
                      svm.type='C-svc',
                      svm.kernel='polydot',
                      poly.degree=4,
                      expand.factor=100)
  
  svm.qtl <- makeSVM(mrkt.comp$str,
                     x.field='occ.qtl',
                     y.field='rate.qtl',
                     z.field='str.act',
                     svm.type='C-svc',
                     svm.kernel='polydot',
                     poly.degree=4,
                     expand.factor=100)
  
  ## Calculate the full market score  
  
  market.ratio <- data.frame(type=c('rate', 'qtl'),
                             actual=rep(mean(mrkt.comp$str$str.act), 2),
                             fitted=c(mean(svm.rate$orig$fitted),
                                      mean(svm.qtl$orig$fitted)),
                             svm=c(mean(svm.rate$pred$pred),
                                   mean(svm.qtl$pred$pred)))
  
  ## Return values
  
  return(list(ltr=mrkt.comp$ltr,
              str=mrkt.comp$str,
              mrkt.table=mrkt.table,
              plot.2.2=twotwo.bar.plot,
              plot.1=str.act.plot,
              rawocc.plot=rawocc.pplot,
              qtlocc.plot=qtlocc.pplot,
              qtl.hm=qtl.hm,
              qtl.hm.svm=qtl.hm.svm,
              rate.hm=rate.hm,
              rate.hm.svm=rate.hm.svm,
              mrkt.score=market.ratio))
}

### Wrapper for all numeric comparison calculations --------------------------------------

revCompWrapper <- function(ltr.df,
                           str.df,
                           ltr.mod.spec,
                           str.mod.spec,
                           clip.field='suburb'){  
  
  
  ## Impute rates and rents
  
  imp.data <- imputeRatesRents(ltr.df=ltr.df, 
                               str.df=str.df, 
                               ltr.mod.spec=ltr.mod.spec, 
                               str.mod.spec=str.mod.spec,
                               clip.field=clip.field)
  
  # Extract out DFs
  str.df <- imp.data$str
  ltr.df <- imp.data$ltr
  
  ## Within type imputation revenues
  
  imp.revs <- revenueEngine(str.df, 
                            ltr.df,
                            rate.field='imp.rate',
                            rent.field='imp.rent')
  
  # Add back to DFs
  str.df$imp.revenue <- imp.revs$str
  ltr.df$imp.revenue <- imp.revs$ltr
  
  ## Cross type revenue estimation
  
  # Apply dom to str
  str.df$imp.dom <- imputeDOM(str.df, ltr.df, calc.type='median')
  
  # Apply occ.rate to ltr
  ltr.df$imp.occ <- imputeOccRate(ltr.df, str.df, calc.type='median')
  
  # Estimate cross revenues
  impx.revs <- revenueEngine(str.df=ltr.df, 
                             ltr.df=str.df,
                             rate.field='imp.rate',
                             rent.field='imp.rent',
                             occ.field='imp.occ',
                             dom.field='imp.dom')
  
  # Add back to DFs
  str.df$imp.ltr.revenue <- impx.revs$ltr
  ltr.df$imp.str.revenue <- impx.revs$str
  
  ## Compare revenues
  
  comp.revs <- compareRevenues(str.df, ltr.df)
  
  ## Return Values   
  
  return(list(str=comp.revs$str,
              ltr=comp.revs$ltr))    
  
}
