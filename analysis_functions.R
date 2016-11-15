


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



crossImputeModel <- function(rent.df,
                             abb.df,
                             rent.mod.spec,
                             abb.mod.spec,
                             geo.field=NULL,
                             geo.value=NULL,
                             clip.field=NULL)
{
  
  # Extract Data from geo
  
  if(!is.null(geo.field)){
    
    id.rgeo <- which(rent.df[,geo.field] == geo.value)
    id.ageo <- which(abb.df[,geo.field] == geo.value)
    
    rent.df <- rent.df[id.rgeo, ]
    abb.df <- abb.df[id.ageo, ]
    
  }
  
  rent.house <- rent.df[rent.df$type == 'House', ]
  rent.apt <- rent.df[rent.df$type == 'Apartment', ]
  abb.house <- abb.df[abb.df$type == 'House', ]
  abb.apt <- abb.df[abb.df$type == 'Apartment', ]
  
  ## Remove those within the vacant short term suburbs  
  
  if(!is.null(clip.field)){
    
    r.cf <- which(names(rent.df) == clip.field)
    a.cf <- which(names(abb.df) == clip.field)
    
    rent.house <- rent.house[rent.house[,r.cf] %in% names(table(
      as.character(abb.house[,a.cf]))), ]
    rent.apt <- rent.apt[rent.apt[,r.cf] %in% names(table(
      as.character(abb.apt[,a.cf]))), ]
    
    abb.house <- abb.house[abb.house[,a.cf] %in% names(table(
      as.character(rent.house[,r.cf]))), ]
    abb.apt <- abb.apt[abb.apt[,a.cf] %in% names(table(
      as.character(rent.apt[,r.cf]))), ]
    
  }
  
  ## Build regression models
  
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
  
  ##
  
  return(list(abb=abb.data,
              rent=rent.data,
              abb.h.mod=ah.mod,
              abb.a.mod=aa.mod,
              rent.h.mod=rh.mod,
              rent.a.mod=ra.mod))
}

