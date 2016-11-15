
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

  data.path <- 'c:/dropbox/research/airBNB/data/'

 ## Load sources (custom function files)

  source("c:/code/dataviztools/ggplottools.R")
  source("c:/code/datamgmttools/dataMungetools.R")
  source("c:/code/research/AirBNBMelbourne/analysis_Functions.R")
  source("c:/code/research/AirBNBMelbourne/dataPrep_Functions.R")

### Load working workspace ---------------------------------------------------------------  
  
  load("C:/Dropbox/Research/airBNB/data/analyzed/abb_working.RData")

### Working analysis ---------------------------------------------------------------------  

  
  
  
  
    
## Impute prices

#Before imputing the rents and rates, we fix the product factors so that the 
  #models use a 2bed/2bath unit as the control unit against which others are 
  #compared.  We also de-factorize the suburb variable for easier use in the 
  #regression models that follow. 

 
## Re-order factors

abb.dataf$bedbath <- factor(abb.dataf$bedbath, 
                            levels=c('2..2', '1..1', '2..1', '3..1', '3..2', '4..2'))
rent.dataf$bedbath <- factor(rent.dataf$bedbath, 
                             levels=c('2..2', '1..1', '2..1', '3..1', '3..2', '4..2'))

## Convert suburbs from factor to character

abb.dataf$suburb <- as.character(abb.dataf$suburb)
rent.dataf$suburb <- as.character(rent.dataf$suburb)

 
### Global model

#We begin by imputing rents and rates for all properties using a global model with 
#suburb fixed effects.  
 
## Set model specifications

rent.mod.spec <- formula(log(event.price) ~ as.factor(bedbath) + as.factor(suburb))
abb.mod.spec <- formula(log(nightly.rate) ~ as.factor(bedbath) + as.factor(suburb))
 
## Impute globally 

glob.imp <- crossImputeModel(rent.dataf, 
                             abb.dataf, 
                             rent.mod.spec=rent.mod.spec, 
                             abb.mod.spec=abb.mod.spec,
                             clip.field='suburb')

#We then calculate the gross hypothetical returns by multiplying the imputed rents 
#and rates by months and days, respectively. After this we must substract lost income 
#due to vacancy and due to costs.
#
#For long-term rentals the primary lost income due to vacny is 

abb.df <- glob.imp$abb
abb.df$product <- paste0(substr(abb.df$type, 1, 1), abb.df$bedbath)

xx <- split(abb.df, abb.df$product)

sm.df <- xx[[1]]

g <- lapply(xx, calcOPQuantiles, daily.df=daily.dataf)
gg <- calcOPQuantiles(abb.df, daily.df=daily.dataf)

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
#   ggplot(data = ss,
#             aes(x = occ.rate,
#                 y = nightly.rate)) +
#     stat_bin2d(data=ss, aes(alpha=..count.., fill=as.factor(ABB)),
#                binwidth=c(.03, 50))
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
