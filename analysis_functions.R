
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
                       wgts=rep(1,length(data.vec)))
{
  
 ## Load required library  
  
  require(Hmisc)
  
 ## Set the adjustment jitter to prevent identical breaks  
  
  adj.jit <- abs(mean(data.vec) / 10000)
  
 ## Calculate the weighted quantiles 0  to 1000  
  
  wtd.qtl <- Hmisc::wtd.quantile(data.vec + runif(length(data.vec), -adj.jit, adj.jit), 
                                 weights=wgts, 
                                 probs=seq(0, 1, .01))
  
 ##  Convert to a vector of quantile indicators 
  
  qtl.vec <- as.numeric(as.factor(cut(data.vec, 
                                      breaks=(wtd.qtl + seq(0, 1, .01) * adj.jit))))
  
 ## Return value
  
  return(qtl.vec)
  
}
























### Calculate the Airbnb quantiles -------------------------------------------------------

calcABBQtls <- function(abb.df){
  
 # Quantiles for market occupancy rates (with a small jitter)
 
  occ.qtl <- Hmisc::wtd.quantile(abb.df$occ.rate + runif(nrow(abb.df), -.0001, .0001), 
                                 weights=(abb.df$bookings + abb.df$available.days), 
                                 probs=seq(0, 1, .01))

 # Quantile for market nightly rates
  
  rate.qtl <- Hmisc::wtd.quantile(abb.df$med.rate + runif(nrow(abb.df), -1, 1), 
                                  weights=(abb.df$bookings + abb.df$available.days), 
                                  probs=seq(0, 1, .01))
  
 # Add quantile ranking to data
  abb.df$occ.qtl <- as.numeric(as.factor(cut(abb.df$occ.rate, 
                                             breaks=(occ.qtl + (1:101/1000000)))))
  
 # Add quantile ranking to data
  abb.df$rate.qtl <- as.numeric(as.factor(cut(abb.df$med.rate, 
                                               breaks=(rate.qtl + (1:101/1000000)))))
  
 return(list(abb.df=abb.df,
              occ.qtl=occ.qtl,
              rate.qtl=rate.qtl))
}

### Calculate the quantiles (rent and DOM) for the long term rentals ---------------------

calcRentQtls <- function(rent.df){
  
 ## Calc the rent quantiles
  
  rent.qtl <- quantile(rent.df$event.price, 
                       probs=seq(0, 1, 0.01)) 
  
 ## Days on market quantiles  
  
  dom.qtl <- quantile(rent.df$dom + runif(nrow(rent.df), -.1, .1), 
                      probs=seq(0, 1, 0.01))  
  
 ## Add quantile ranking to data
  
  rent.df$dom.qtl <- as.numeric(as.factor(cut(rent.df$dom, 
                                              breaks=(dom.qtl + (1:101/1000000)))))
  
 ## Add quantile ranking to data
  
  rent.df$rent.qtl <- as.numeric(as.factor(cut(rent.df$event.price, 
                                               breaks=(rent.qtl + (1:101/1000000)))))
  
 ## Return values  
  
  return(list(rent.df = rent.df,
              rent.qtl = rent.qtl,
              dom.qtl = dom.qtl))
}

### Calculate the revenues for each product mix ------------------------------------------

imputeRevenues <- function(abb.qtls,
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
  } else {
    abb.df$abb.rev <- abb.df$imp.rate * 365 * occ.qtl[51]
  }  
  
 ## Estimate long term properties (rent and rates)  
  
  rent.df$rent.rev <- rent.df$imp.rent * (52 - (dom.qtl[51]/7))
  
  if(mrkt.occ){
    rent.df$abb.rev <- rent.df$imp.rate * 365 * rent.df$market.occ
  } else {
    rent.df$abb.rev <- rent.df$imp.rate * 365 * occ.qtl[51]
  }
  
 ## Save for future cost estimates

 ## Return Values
  
  return(list(abb=abb.df,
              rent=rent.df))
}

### Full return comparison function ------------------------------------------------------

compReturns <- function(rent.df=rent.dataf, 
                        abb.df=abb.dataf, 
                        rent.mod.spec=rent.mod.spec, 
                        abb.mod.spec=abb.mod.spec,
                        exch.rate=exch.rate,
                        comp.field='sub.mrkt',
                        clip.fields='suburb'){  
  
 ## Arguments
  
  # rent.df: data.frame of rental obs
  # abb.df: data.frame of airbnb obs
  # rent.mod.spec: rental data model specification
  # abb.mod.spec: airbnb model specification
  # exch.rate: A$ in US$
  # clip.fields: fields that are factors in the model specifications
  
 ## Impute rates and rents 
  
  imp.data <- crossImputeModel(rent.df=rent.df, 
                               abb.df=abb.df, 
                               rent.mod.spec=rent.mod.spec, 
                               abb.mod.spec=abb.mod.spec,
                               exch.rate=exch.rate,
                               clip.fields=clip.fields)
  
 ## Calculate market occ and rate quantiles
  
  # Calculate Rate/Rent/OccRate/DOM Quantiles
  abb.qtls <- calcABBQtls(imp.data$abb)
  rent.qtls <- calcRentQtls(imp.data$rent)  
  
 ## Calculate the revenues
  
  rev.data <- imputeRevenues(abb.qtls=abb.qtls, 
                             rent.qtls=rent.qtls)
  
 ## Compare ABB to Rent
  
  # Abb
  abb.df <-rev.data$abb
  abb.df$abb.prem <- abb.df$abb.rev - abb.df$rent.rev
  abb.df$abb.prem.act <- abb.df$revenue - abb.df$rent.rev
  abb.df$abb <- ifelse(abb.df$abb.prem > 0, 1, 0)
  abb.df$abb.act <- ifelse(abb.df$abb.prem.act > 0, 1, 0)
  
  # Rent
  rent.df <-rev.data$rent
  rent.df$abb.prem <- rent.df$abb.rev - rent.df$rent.rev
  rent.df$abb.prem.act <- rent.df$abb.rev - rent.df$revenue
  rent.df$abb <- ifelse(rent.df$abb.prem > 0, 1, 0)
  rent.df$abb.act <- ifelse(rent.df$abb.prem.act > 0, 1, 0)
  
  # Make table of comparisons
  abb.imp <- tapply2DF(abb.df$abb, abb.df[,comp.field], mean)
  abb.act <- tapply2DF(abb.df$abb.act, abb.df[,comp.field], mean)
  rent.imp <- tapply2DF(rent.df$abb, rent.df[,comp.field], mean)
  rent.act <- tapply2DF(rent.df$abb.act, rent.df[,comp.field], mean)
  abb.imp$est <- rent.imp$est <- 'imputed'
  abb.imp$data <- abb.act$data <- 'abb'
  rent.act$est <- abb.act$est <- 'actual'
  rent.act$data <- rent.imp$data <- 'rent'
  rate.table <- rbind(abb.imp, abb.act, rent.imp, rent.act)
  
 ## Plot comparisons
  
  # Across all data and est
  rate.plot <- ggplot(rate.table, aes(x=ID, weights=Var)) + geom_bar() +
    facet_grid(data ~ est)
  
  # Just actual AirBNB
  act.plot <- ggplot(rate.table[rate.table$data == 'abb' & rate.table$est == 'actual', ],
                     aes(x=ID, weights=Var)) + 
    geom_bar() 
  
  
  q.list <- list()
  abb.df$oc <- round(abb.df$occ.rate, 2)
  for(q in 1:100){
    
    aa <- abb.df[abb.df$oc == q/100, ]
    x.act <- tapply2DF(aa$abb.act, aa$sub.mrkt, mean)
    x.act$qtl <- q
    q.list[[q]] <- x.act
    
  }
  
  jj <- rbind.fill(q.list)
  occ.plot <- ggplot(jj, aes(x=qtl, y=Var, group=ID, color=ID)) + 
    geom_point() + 
    stat_smooth(se=F,span=.66, n=222)
  
  
  q.list <- list()
  #abb.df$oc <- round(abb.df$occ.rate, 2)
  for(q in 1:100){
    
    aa <- abb.df[abb.df$rate.qtl == q, ]
    x.act <- tapply2DF(aa$abb.act, aa$sub.mrkt, mean)
    x.act$qtl <- q
    q.list[[q]] <- x.act
    
  }
  
  jj <- rbind.fill(q.list)
  price.plot <- ggplot(jj, aes(x=qtl, y=Var, group=ID, color=ID)) + 
    geom_point() + 
    stat_smooth(se=F,span=.66, n=222)
  
  point.plot <- ggplot(data = abb.df,
                       aes(x = occ.qtl,
                           y = rate.qtl,
                           color=as.factor(abb.act))) +
    geom_point()
  
  heat.map <- ggplot(data = abb.df,
                     aes(x = occ.qtl,
                         y = rate.qtl)) + 
    stat_bin2d(data=abb.df, aes(alpha=..count.., fill=as.factor(abb.act)),
               binwidth=c(10, 10))
  
  a<- table(paste0(round(abb.df$rate.qtl, -1), 
                   ".", 
                   round(abb.df$occ.qtl, -1)),
            abb.df$abb.act)
  c<-(a[,2]-a[,1])
  ab <- length(which(c>0))/length(c)
  re <- length(which(c<0))/length(c)
  market.value <- ab/re
  
  return(list(market.value=market.value,
              abb=abb.df,
              rent=rent.df,
              rate.plot=rate.plot,
              occ.plot=occ.plot,
              point.plot=point.plot,
              price.plot=price.plot,
              heat.map=heat.map
  ))
  
}   
