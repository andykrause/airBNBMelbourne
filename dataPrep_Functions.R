##########################################################################################
#                                                                                        #    
#  Functions for working with APM and AirDNA data                                        #
#                                                                                        #
##########################################################################################

### Analyze the daily data for each property ---------------------------------------------

abbAnalyzeDaily <- function(daily.df){
  
 ## Calculate minimum and maximum dates in the range  
  
  min <- min(daily.df$date)
  max <- max(daily.df$date)
  
 ## Calculate the Total days, and count each status  
  
  total.days <- nrow(daily.df)
  blocked <- length(which(daily.df$status == 'B'))
  available <- length(which(daily.df$status == 'A'))
  reserved <- length(which(daily.df$status == 'R'))
  
 ## Return as a data.frame  
  
  return(data.frame(property.id = unique(daily.df$property.id),
                    min.date = min,
                    max.date = max,
                    total.days = total.days,
                    blocked.days = blocked,
                    available.days = available,
                    reserved.days = reserved))
}

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

