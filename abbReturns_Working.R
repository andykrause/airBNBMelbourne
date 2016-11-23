
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
  
  load("C:/Dropbox/Research/airBNB/data/analyzed/abb_working.RData")
  
 ## Load sources (custom function files)

  source("c:/code/dataviztools/ggplottools.R")
  source("c:/code/datamgmttools/dataMungetools.R")
  source("c:/code/research/AirBNBMelbourne/analysis_Functions.R")
  source("c:/code/research/AirBNBMelbourne/dataPrep_Functions.R")

  ltr.dataf <- rent.dataf
  
### Working analysis ---------------------------------------------------------------------  

  exch.rate <- 1.32

 ## Calculate actual revenue for study period for AirBnb
  
  # Determine # of booking for each airbnb
  abb.dataf$bookings <- (abb.dataf$total.days - 
                          abb.dataf$blocked.days - 
                           abb.dataf$available.days)
  
  # Determine property specific occupancy rate
  abb.dataf$occ.rate <- (abb.dataf$bookings / 
                           (abb.dataf$total.days - abb.dataf$blocked.days))
  
  # Set the blocked rate
  abb.dataf$blocked.rate <- abb.dataf$blocked.days / abb.dataf$total.days
  
  # Set the extrapolation parameter for those on the site for less than whole period
  abb.dataf$extr.par <- 366 / abb.dataf$total.days
  
  # Get all of the reservations
  resv.daily <- daily.dataf[daily.dataf$status == 'R' &
                             daily.dataf$booked.date != 'imputed' &
                              daily.dataf$price < 600, ]
  
  # Calculate the property specific median nightly rate
  med.daily <- tapply2DF(resv.daily$price, resv.daily$property.id, median)
  
  # Add median rate to the property  
  abb.dataf$med.rate <- med.daily$Var[match(abb.dataf$property.id, med.daily$ID)] 
  
  # Calculate the total revenue  
  abb.dataf$revenue <- (abb.dataf$med.rate * exch.rate * 
                          abb.dataf$extr.par * abb.dataf$bookings)
  
  # Remove those with no revenue
  abb.dataf <- abb.dataf[!is.na(abb.dataf$revenue), ]
  
 ## Calculate ltral revenues  
  
  # Calculate the actual revenue
  ltr.dataf$revenue <- ltr.dataf$event.price * (52 - ltr.dataf$dom/7)
  
  # Remove those with very low revenues (due to very long DOMs)
  ltr.dataf <- ltr.dataf[ltr.dataf$revenue > 5000, ]
  
 ## Save for future costs
  
### Comparison
  
 ## Data prep
    
  # Re-order factors
  abb.dataf$bedbath <- factor(abb.dataf$bedbath, 
                              levels=c('2..2', '1..1', '2..1', '3..1', '3..2', '4..2'))
  ltr.dataf$bedbath <- factor(ltr.dataf$bedbath, 
                               levels=c('2..2', '1..1', '2..1', '3..1', '3..2', '4..2'))

  # Add product as a variable
  abb.dataf$product <- paste0(substr(abb.dataf$type, 1, 1), abb.dataf$bedbath)
  ltr.dataf$product <- paste0(substr(ltr.dataf$type, 1, 1), ltr.dataf$bedbath)

  # Convert suburbs from factor to character
  abb.dataf$suburb <- as.character(abb.dataf$suburb)
  ltr.dataf$suburb <- as.character(ltr.dataf$suburb)

 ## Global model

  # We begin by imputing rents and rates for all properties using a global model with 
  # suburb fixed effects.  
 
  # Set model specifications
  ltr.mod.spec <- formula(log(event.price) ~ as.factor(bedbath) + as.factor(suburb))
  abb.mod.spec <- formula(log(nightly.rate) ~ as.factor(bedbath) + as.factor(suburb))
 
  
  glob.comp <- revCompWrapper(ltr.df=ltr.dataf,
                              abb.df=abb.dataf,
                              ltr.mod.spec=ltr.mod.spec,
                              abb.mod.spec=abb.mod.spec,
                              exch.rate=exch.rate,
                              clip.field='suburb')   
   

  glob.table <- createCompTable(glob.comp$abb, glob.comp$ltr, 'sub.mrkt')
     
  
  
  
   
   
   
 ## 
  
  abb.imp <- tapply2DF(abb.df$abb.better, abb.df$sub.mrkt, mean)
  abb.act <- tapply2DF(abb.df$abb.better.act, abb.df$sub.mrkt, mean)
  rent.imp <- tapply2DF(rent.df$abb.better, rent.df$sub.mrkt, mean)
  rent.act <- tapply2DF(rent.df$abb.better.act, rent.df$sub.mrkt, mean)
  abb.imp$est <- rent.imp$est <- 'imputed'
  abb.imp$data <- abb.act$data <- 'abb'
  rent.act$est <- abb.act$est <- 'actual'
  rent.act$data <- rent.imp$data <- 'rent'
  rate.table <- rbind(abb.imp, abb.act, rent.imp, rent.act)
  
  ggplot(rate.table, aes(x=ID, weights=Var)) + geom_bar() +
    facet_grid(data ~ est)
  
  
  q.list <- list()
  for(q in 1:100){
    
    aa <- abb.df[abb.df$occ.rate >= q/100, ]
    x.act <- tapply2DF(aa$abb.better.act, aa$sub.mrkt, mean)
    x.act$qtl <- q
    q.list[[q]] <- x.act
    
  }
  
  jj <- rbind.fill(q.list)
  ggplot(jj, aes(x=qtl, y=Var, group=ID, color=ID)) + geom_line()
  
  q.list <- list()
  abb.df$block.rate <- abb.df$blocked.days/abb.df$total.days
  for(q in 1:100){
    
    aa <- abb.df[abb.df$block.rate >= q/100, ]
    x.act <- tapply2DF(aa$abb.better.act, aa$sub.mrkt, mean)
    x.act$qtl <- q
    q.list[[q]] <- x.act
    
  }
  
  jj <- rbind.fill(q.list)
  ggplot(jj, aes(x=qtl, y=Var, group=ID, color=ID)) + geom_line()
  
  
  

 

#   ggplot(data = ss,
#             aes(x = occ.rate,
#                 y = nightly.rate, 
#                 color=as.factor(ABB))) +
#     geom_point()
#   
#   ggplot(data = ss,
#             aes(x = oq,
#                 y = pq, 
#                 color=as.factor(ABB))) +
#     geom_point()
#   
#   
#   
   
  dd <- abb.df[abb.df$occ.rate > .01, ]
  
  dd <- dd[dd$sub.mrkt == 'rural', ]
  
  
   ggplot(data = dd,
             aes(x = occ.qtl,
                 y = rate.qtl)) +
     stat_bin2d(data=dd, aes(alpha=..count.., fill=as.factor(abb.better.act)),
                binwidth=c(10, 10))
   
   ggplot(data = dd,
          aes(x = occ.qtl,
              y = rate.qtl)) +
     stat_bin2d(data=dd, fill=as.factor(dd$abb.better.act), binwidth=c(10, 10))
   
   
   
#   
#   ggplot(data = ss,
#             aes(x = occ.rate,
#                 y = nightly.rate)) +
#     stat_bin2d(data=ss, aes(fill=as.factor(ABB)),
#                binwidth=c(.03, 20))
#   
#   
#   
#     
#   ggplot(data = ss,
#             aes(x = oq,
#                 y = pq)) +
#     stat_bin2d(data=ss, aes(alpha=..count.., fill=as.factor(ABB)),
#                binwidth=c(10, 10))
#   ggplot(data = ss,
#             aes(x = oq,
#                 y = pq)) +
#     stat_bin2d(data=ss, aes(fill=as.factor(ABB)),
#                binwidth=c(5, 5))
#   
#   
#   
# 
#   stat_summary_hex(fun = function(x) sum(x))
# }

core.imp <- crossImputeModel(rent.dataf, abb.dataf, 
                             rent.mod.spec=rent.mod.spec, 
                             abb.mod.spec=abb.mod.spec,
                             geo.field='sub.mrkt',
                             geo.value='city-core',
                             clip.field='suburb')
city.imp <- crossImputeModel(rent.dataf, abb.dataf, 
                             rent.mod.spec=rent.mod.spec, 
                             abb.mod.spec=abb.mod.spec,
                             geo.field='sub.mrkt',
                             geo.value='city',
                             clip.field='suburb')
sub.imp <- crossImputeModel(rent.dataf, abb.dataf, 
                            rent.mod.spec=rent.mod.spec, 
                            abb.mod.spec=abb.mod.spec,
                            geo.field='sub.mrkt',
                            geo.value='suburban',
                            clip.field='suburb')
rural.imp <- crossImputeModel(rent.dataf, abb.dataf, 
                              rent.mod.spec=rent.mod.spec, 
                              abb.mod.spec=abb.mod.spec,
                              geo.field='sub.mrkt',
                              geo.value='rural',
                              clip.field='suburb')
beach.imp <- crossImputeModel(rent.dataf, abb.dataf, 
                              rent.mod.spec=rent.mod.spec, 
                              abb.mod.spec=abb.mod.spec,
                              geo.field='sub.mrkt',
                              geo.value='beach',
                              clip.field='suburb')

abb.imp <- rbind(core.imp$abb, city.imp$abb, sub.imp$abb, rural.imp$abb, beach.imp$abb)
rent.imp <- rbind(core.imp$rent, city.imp$rent, sub.imp$rent, rural.imp$rent,
                  beach.imp$rent)

glob.abb <- glob.imp$abb
glob.rent <- glob.imp$rent

abb.dataf$imp.rate.sm <- abb.imp$imp.rate[match(abb.dataf$property.id,
                                                abb.imp$property.id)]
abb.dataf$imp.rent.sm <- abb.imp$imp.rent[match(abb.dataf$property.id,
                                                abb.imp$property.id)]
rent.dataf$imp.rent.sm <- rent.imp$imp.rent[match(rent.dataf$id.key,
                                                  rent.imp$id.key)]
rent.dataf$imp.rate.sm <- rent.imp$imp.rate[match(rent.dataf$id.key,
                                                  rent.imp$id.key)]

abb.dataf$imp.rate.gl <- glob.abb$imp.rate[match(abb.dataf$property.id,
                                                 glob.abb$property.id)]
abb.dataf$imp.rent.gl <- glob.abb$imp.rent[match(abb.dataf$property.id,
                                                 glob.abb$property.id)]
rent.dataf$imp.rate.gl <- glob.rent$imp.rate[match(rent.dataf$id.key,
                                                   glob.rent$id.key)]
rent.dataf$imp.rent.gl <- glob.rent$imp.rent[match(rent.dataf$id.key,
                                                   glob.rent$id.key)]

abb.dataf$data <- 'abb'
rent.dataf$data <- 'rent'
comp.data <- rbind(abb.dataf[,c('type','data', 'imp.rate.gl', 'imp.rate.sm',
                                'imp.rent.sm', 'imp.rent.gl', 'sub.mrkt')],
                   rent.dataf[,c('type','data', 'imp.rate.gl', 'imp.rate.sm',
                                 'imp.rent.sm', 'imp.rent.gl', 'sub.mrkt')])


#xx <- melt(comp.data, id.vars=c('data', 'type', 'sub.mrkt')) 

ggplot(comp.data, aes(x=imp.rate.gl, y=imp.rate.sm, group=sub.mrkt,
                      color=sub.mrkt)) +
  geom_point() + facet_grid(sub.mrkt~data) +
  stat_smooth()


ggplot()+
  geom_density(data=comp.data, aes(x=imp.rent.gl, group=sub.mrkt,
                                   color=sub.mrkt)) +
  geom_density(data=comp.data, aes(x=imp.rent.sm, group=sub.mrkt,
                                   color=sub.mrkt)) + 
  facet_grid(sub.mrkt~data) 


ggplot()+
  geom_density(data=comp.data, aes(x=imp.rate.gl, group=sub.mrkt,
                                   color=sub.mrkt)) +
  geom_density(data=comp.data, aes(x=imp.rate.sm, group=sub.mrkt,
                                   color=sub.mrkt)) + 
  facet_grid(sub.mrkt~data) 
