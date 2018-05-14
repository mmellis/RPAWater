### Example script with input dataframe
library(dplyr)
library(tidyr)
library(ggplot2)

# Input initial values dataframe
init<-read.csv('rawdata/RPAWaterInputs.csv', na.strings=c('NA','-', '#VALUE!', '#DIV/0!'))
   names(init)<-tolower(names(init))
   names(init)<-gsub('perunit', 'wpu', names(init))
   names(init)<-sub('inc.in.millions','inc',names(init))
   names(init)<-sub('acresirrig','acres',names(init))
#init<-select(init, -ir.growth, -ir.decay) %>% rename(ir.growth=iracre.growth, ir.decay=iracre.decay)

      
# Input drivers
pop<-read.csv("rawdata/Popdata.csv"); stopifnot(!any(sapply(pop[,-1],is.factor)))
pop<-gather(pop, "year", "pop",-c(1:2) ) 
  pop$year<-as.numeric(substr(pop$year, 8,11))

inc<-read.csv("rawdata/Incdata.csv"); stopifnot(!any(sapply(inc[,-1],is.factor)))
inc<-gather(inc, "year", "inc",-c(1:2) ) 
  inc$year<-as.numeric(substr(inc$year, 9,12))
  inc$inc<-inc$inc * left_join(inc, pop)$pop  # Convert to total income ??
  
acre<-read.csv("rawdata/Acredata.csv"); stopifnot(!any(sapply(acre[,-1],is.factor)))
acre<-gather(acre, "year","acres", -c(1)) 
  acre$year<-as.numeric(substr(acre$year, 9,12))
  #acre<-subset(acre, year %in% unique(pop$year))  

drivers<-left_join(full_join(pop,inc),acre); rm(pop,inc,acre)

# Match scales for initial and drivers data
subset(init, fips=='1001')[, c('pop','inc','acres')]
subset(drivers, fips=='1001' & year==2015)

scaleI<-median(floor(log10(init$pop)),na.rm=T)
scaleD<-median(floor(log10(subset(drivers, year==2015)$pop)))
  stopifnot(scaleI==scaleD)
#  init$pop<-init$pop*10^(scaleD-scaleI)


scaleI<-median(floor(log10(init$inc)),na.rm=T)
scaleD<-median(floor(log10(subset(drivers, year==2015)$inc)))
  stopifnot(scaleI==scaleD)
#  init$inc<-init$inc*10^(scaleD-scaleI) ### SOMETHING IS AMISS HERE!!!

# income is in per capita personal income for projections, but possibly 
  # total income for init data
  #init$inc<-init$inc/init$pop


# Pull projection values from initial and drivers dataframes                           
   init.yr<-unique(init$year); stopifnot(length(init.yr)==1)

   proj.yrs<-sort(unique(drivers$year))
   proj.scenario<-unique(drivers$scenario)
   proj.model<-NA

   both.fips<-intersect(init$fips, drivers$fips)
   both.sector<-c('dp','ic','ir')

# Create empty df for projections
wd <- expand.grid(year     = c(init.yr, proj.yrs), 
                  fips     = both.fips, 
                  sector   = both.sector, 
                  scenario = proj.scenario, 
                  model    = proj.model)
                  
# This function addDrivers does XXXXXX -> adds driver data to projection DF
addDrivers <- function (projDF, driverDF) 
{
  #browser()
  projDF <- left_join(projDF, driverDF, by = c("fips", "scenario", "year")) %>% 
    mutate(driver = ifelse(sector == "dp", pop, 
                    ifelse(sector == "ic", inc,
                    ifelse(sector == "ir", acres, NA)))) %>% 
    select(-(pop:acres))
  return(projDF)
}

wd <- addDrivers(wd, drivers)
head(wd)

# This function addInitialValues does XXXXXXX 
#    -> adds initial driver value and wpu to projection DF
addInitalValues <- function (projDF, initDF) 
{
  #browser()
  init.drivers<-select(initDF, c('fips','year', 'pop', 'inc', 'acres'))  %>%
                 gather('sector', 'driver', pop:acres)          %>%
                   mutate(sector = ifelse(sector == "pop", "dp", 
                    ifelse(sector == "inc", "ic", 
                    ifelse(sector == "acres", "ir", NA))))

  projDF<- left_join(projDF, init.drivers, by = c("year", "fips","sector")) %>%
              mutate(driver=coalesce(driver.x, driver.y)) %>%
              select(-driver.x, -driver.y)
                     
  init.wpu<-select(initDF, "year", "fips", ends_with('wpu')) %>%
               gather('sector','wpu', -(1:2))              %>%
               mutate(sector=gsub('.wpu','',sector))
               
  projDF<-left_join(projDF, init.wpu)
  
  return(projDF)
}

wd<-addInitalValues (wd, init)
head(wd)


# This function calcWPU 
# -> inputs growth and decay rates from initial DF
# -> calculates withdrawals per unit trends for all sectors
calcWPU <- function (projDF, initDF) 
{
  #browser()
  init.rates<-select(initDF, 'fips','year',contains('growth'), contains('decay')) %>%
                  gather('sector','rate', -(1:2)) %>%
                  separate(sector, into=c('sector','sig')) %>%
                  spread(sig, rate)
  
  projDF <- left_join(projDF, init.rates) %>% 
        group_by(sector, scenario, model, fips) %>%
        mutate(fc = ifelse(year > min(year),
                   ((1 + growth[1] * (1 + decay[1])^(year - min(year)))^5), 1), 
           wpu = wpu[1] * cumprod(fc)) %>% 
    select(-fc, -growth, -decay)
  return(projDF)
}

wd<-calcWPU(wd, init)
head(wd)  

# This function calcWD does XXXXXX
# -> calculates wd from wpu and driver data
calcWD <- function (dF) 
{
  dF <- dF %>% mutate(wd = wpu * driver)
  return(dF)
} 

wd<-calcWD(wd)   
head(wd)  

ggplot(wd, aes(x=year, y=wd, color=scenario))+
  stat_summary(geom='line', fun.y='sum')+
  facet_wrap(~sector, scales='free') + expand_limits(y = 0)                                      