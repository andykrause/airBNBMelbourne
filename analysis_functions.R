
crossImputeModel <- function(rent.df,
                             abb.df,
                             rent.mod.spec,
                             abb.mod.spec,
                             geo.field=NULL,
                             geo.value=NULL,
                             clip.field=NULL)
{
  
 ## Arguments
  
  # rent.df:  data.frame of long term rental observations
  # abb.df:  data.frame of airbnb properties
  # rent.mod.spec:  specification for rent price model
  # abb.mod.spec:  specification for airbnb properties
  # geo.field: field to split submarkets by
  # geo.value: specific geography to use
  # clip.field: field to ensure factors match between rent and abb
  
 ## Extract Data from geo
  
  if(!is.null(geo.field)){
    
    # Create IDs for inclusion in the geo
    id.rgeo <- which(rent.df[,geo.field] == geo.value)
    id.ageo <- which(abb.df[,geo.field] == geo.value)
    
    # Extract and create new DFs
    rent.df <- rent.df[id.rgeo, ]
    abb.df <- abb.df[id.ageo, ]
    
  }
  
 ## Split data by Property Type  
  
  rent.house <- rent.df[rent.df$type == 'House', ]
  rent.apt <- rent.df[rent.df$type == 'Apartment', ]
  abb.house <- abb.df[abb.df$type == 'House', ]
  abb.apt <- abb.df[abb.df$type == 'Apartment', ]
  
 ## Remove those within the clip field that isn't present in both  
  
  if(!is.null(clip.field)){
    
    # Find the fields that are used to clip
    r.cf <- which(names(rent.df) == clip.field)
    a.cf <- which(names(abb.df) == clip.field)
    
    # Get IDs for those to be removed
    id.rh <- rent.house[ ,r.cf] %in% names(table(as.character(abb.house[ ,a.cf])))
    id.ra <- rent.apt[ ,r.cf] %in% names(table(as.character(abb.apt[ ,a.cf])))
    id.ah <- abb.house[ ,a.cf] %in% names(table(as.character(rent.house[ ,r.cf])))
    id.aa <- abb.apt[ ,a.cf] %in% names(table(as.character(rent.apt[ ,r.cf])))
    
    # Filter out obs missing matched factors  
    rent.house <- rent.house[id.rh, ]
    rent.apt <- rent.apt[id.ra, ]
    abb.house <- abb.house[id.ah, ]
    abb.apt <- abb.apt[id.aa, ]
  
  }
  
 ## Build regression models for rental values
  
  rh.mod <- lm(rent.mod.spec, data=rent.house)
  ra.mod <- lm(rent.mod.spec, data=rent.apt)
  
 ## Add the fitted values to the long term data
  
  rent.house$imp.rent <- exp(rh.mod$fitted)
  rent.apt$imp.rent <- exp(ra.mod$fitted)
  
 ## Add the predicted values to the short term data
  
  abb.house$imp.rent <- exp(predict(rh.mod, abb.house))
  abb.apt$imp.rent <- exp(predict(ra.mod, abb.apt))
  
 ## Build regression models
  
  ah.mod <- lm(abb.mod.spec, data=abb.house)
  aa.mod <- lm(abb.mod.spec, data=abb.apt)
  
 ## Add the fitted values to the long term data
  
  abb.house$imp.rate <- exp(ah.mod$fitted)
  abb.apt$imp.rate <- exp(aa.mod$fitted)
  
 ## Add the predicted values to the short term data
  
  rent.house$imp.rate <- exp(predict(ah.mod, rent.house))
  rent.apt$imp.rate <- exp(predict(aa.mod, rent.apt))
  
 ### Merge back together
  
  abb.data <- rbind(abb.house, abb.apt)
  rent.data <- rbind(rent.house, rent.apt)
  
 ## Estimate Max Revenue
 
  abb.data$imprent.revenue <- abb.data$imp.rent * 52
  abb.data$imprate.revenue <- abb.data$imp.rate * 365
  
  rent.data$imprent.revenue <- rent.data$imp.rent * 52
  rent.data$imprate.revenue <- rent.data$imp.rate * 365
  
 ## Return Values  
  
  return(list(abb=abb.data,
              rent=rent.data,
              abb.h.mod=ah.mod,
              abb.a.mod=aa.mod,
              rent.h.mod=rh.mod,
              rent.a.mod=ra.mod))
}













calcOPQuantiles <- function(sm.df,
                            daily.df){
  
  ## Extract limited daily data  
  
  daily.sm <- daily.df[daily.df$property.id %in% sm.df$property.id, ]  
  
  ## Calculate the number of open days  
  
  # Table all status types
  status.table <- as.data.frame(table(daily.sm$status, daily.sm$property.id))
  
  # cast into a wide data frame
  status.df <- dcast(status.table, Var2 ~ Var1)
  
  # All all.days and open days values
  status.df$all.days <- rowSums(status.df[ ,2:4])
  status.df$open.days <- status.df$all.days - status.df$B
  
  # Remove those with no open days or no reservations
  #status.df <- status.df[status.df$open.days != 0, ]
  #status.df <- status.df[status.df$R != 0, ]
  
 ## Calculate occupancy rates  
  
  # Calc property specific rate
  status.df$occ.rate <- status.df$R / status.df$open.days
  status.df$occ.rate[is.na(status.df$occ.rate)] <- 0
  status.df$occ.rate[!is.finite(status.df$occ.rate)] <- 0

  # Set up clean set for computation
  status.comp <- status.df[!is.na(status.df$occ.rate), ]
  status.comp <- status.comp[status.comp$occ.rate > 0, ]

  # Quantiles for submarket occupancy rates
  occ.qtl <- Hmisc::wtd.quantile(status.comp$occ.rate, 
                                 weights=status.comp$open.days, 
                                 probs=seq(0, 1, .01))
  occ.qtl[[1]] <- -101/10000000
  
  ## Calculate rate quantiles
  
  # Limit to non-blocked days
  open.daily <- daily.sm[daily.sm$status != 'B', ]
  
  # Calculate quantiles
  rate.qtl <- quantile(open.daily$price, probs=seq(0, 1, .01))
  
 ## Add to sm data.frame
  
  # Add occupancy rate
  sm.df$occ.rate <- status.df$occ.rate[match(sm.df$property.id, status.df$Var2)]
  sm.df$occ.qtl <- as.numeric(as.factor(cut(sm.df$occ.rate, 
                                            breaks=(occ.qtl + (1:101/10000000)))))
  sm.df$rate.qtl <- as.numeric(as.factor(cut(sm.df$nightly.rate, 
                                             breaks=(rate.qtl + (1:101/100000)))))
  
  ## Return Values
  
  return(list(abb.df=sm.df,
              occ.qtl=occ.qtl,
              rate.qtl=rate.qtl))
}

### Calculate the quantiles (rent and DOM) for the long term rentals ---------------------

calcLTQuantiles <- function(rent.df){
  
 ## Calc the rent quantiles
  
  rent.qtl <- quantile(rent.df$event.price, probs=seq(0, 1, 0.01))
  
 ## Days on market quantiles  
  
  dom.qtl <- quantile(rent.df$dom, probs=seq(0, 1, 0.01))
  
 ## Return values  
  
  return(list(rent.df = rent.df,
              rent.qtl = rent.qtl,
              dom.qtl = dom.qtl))
}

### Calculate the revenues for each product mix ------------------------------------------

calcRevenues <- function(abb.qtls,
                         rent.qtls,
                         mrkt.occ=FALSE,
                         mrkt=NULL){
  
  ## Strip out property data  
  
  abb.df <- abb.qtls$abb.df
  rent.df <- rent.qtls$rent.df
  
  ## Strip out quantile data  
  
  occ.qtl <- abb.qtls$occ.qtl
  dom.qtl <- rent.qtls$dom.qtl
  
 ## If using market based occupancy rates  
  
  if(mrkt.occ){
    
    market <- abb.df[,mrkt]
    market.df <- tapply(abb.df$occ.rate, market, median)
    market.df <- as.data.frame(market.df)
    names(market.df) <- 'occ.rate'
    market.df$market <- rownames(market.df)
    rownames(market.df) <- 1:nrow(market.df)
    
    abb.df$market.occ <- market.df$occ.rate[match(abb.df[ ,mrkt],
                                                  market.df$market)]
    rent.df$market.occ <- market.df$occ.rate[match(rent.df[ ,mrkt],
                                                   market.df$market)]
  }
  
  ## Estimate airbnb properties (rents and rates)  
  
  abb.df$rent.rev <- abb.df$imp.rent * (52 - (dom.qtl[51]/7))
  
  if(mrkt.occ){
    abb.df$abb.rev <- abb.df$imp.rate * 365 * abb.df$market.occ
    abb.df$abb.revx <- abb.df$nightly.rate * 365 * abb.df$market.occ
  } else {
    abb.df$abb.rev <- abb.df$imp.rate * 365 * occ.qtl[51]
    abb.df$abb.revx <- abb.df$nightly.rate * 365 * abb.df$occ.rate
  }  
  
  ## Estimate long term properties (rent and rates)  
  
  rent.df$rent.rev <- rent.df$imp.rent * (52 - (dom.qtl[51]/7))
  rent.df$rent.revx <- rent.df$event.price * (52 - (rent.df$dom/7))
  if(mrkt.occ){
    rent.df$abb.rev <- rent.df$imp.rate * 365 * rent.df$market.occ
  } else {
    rent.df$abb.rev <- rent.df$imp.rate * 365 * occ.qtl[51]
  }
  
 ## Save for future cost estimates


 ## Calculate abb premium
  
  abb.df$abb.prem <- abb.df$abb.rev - abb.df$rent.rev
  abb.df$abb.prem.act <- abb.df$abb.revx - abb.df$rent.rev
  rent.df$abb.prem <- rent.df$abb.rev - rent.df$rent.rev
  rent.df$abb.prem.act <- rent.df$abb.rev - rent.df$rent.revx

 ## Combine
  
  # Prepare ABB data
  abb.comb <- abb.df[, c('property.id', 'sub.mrkt', 'product', 'suburb',
                         'imp.rent', 'imp.rate', 'rent.rev', 'abb.rev',
                         'latitude', 'longitude',
                         'abb.revx', 'nightly.rate',
                         'abb.prem', 'abb.prem.act')]
  names(abb.comb)[c(1,11,12)] <- c('id', 'act.rev','act.rr')
  abb.comb$id <- paste0('abb.', abb.comb$id)
  abb.comb$data <- 'abb'

  # Prepare rent data
  rent.comb <- rent.df[, c('id.key', 'sub.mrkt', 'product', 'suburb',
                           'imp.rent', 'imp.rate', 'rent.rev', 'abb.rev',
                           'latitude', 'longitude',
                           'rent.revx', 'event.price',
                           'abb.prem', 'abb.prem.act')]
  names(rent.comb)[c(1,11,12)] <- c('id', 'act.rev','act.rr')
  rent.comb$id <- paste0('rent.', rent.comb$id)
  rent.comb$data <- 'rent'
  
  # Combine
  all.comb <- rbind(abb.comb, rent.comb)
  all.comb$abb.better <- ifelse(all.comb$abb.prem > 0, 1, 0)
  
 ## Return values
  
  return(all.comb)
  
}
