### Example script with input dataframe
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(ggmap)

# Input initial values dataframe
# population units in people
# income units in thousands
# wpu.dp is gallons per capita per day
init<-read.csv('rawdata/RPAWaterInputs.csv', na.strings=c('-'))
   names(init)<-tolower(names(init))
   names(init)<-gsub('perunit', 'wpu', names(init))
   names(init)<-sub('inc.in.millions','inc',names(init))
   names(init)<-sub('acresirrig','acres',names(init))

   init<-subset(init, year != 'NA')
   
   init$pop<-as.numeric(init$pop)
   init$inc<-as.numeric(init$inc)
   init$dp.wpu<-as.numeric(init$dp.wpu)
   
# Input drivers
pop<-read.csv("rawdata/Popdata.csv")
pop<-gather(pop, "year", "pop",-c(1:2) ) 
  pop$year<-as.numeric(substr(pop$year, 8,11))
# Travis re-did his input file to make population unit in people. Need to do this for the 
  # data file in general, though the scaling below might be sufficient.
  
inc<-read.csv("rawdata/Incdata.csv")
inc<-gather(inc, "year", "inc",-c(1:2) ) 
  inc$year<-as.numeric(substr(inc$year, 9,12))
# income is in per capita personal income for projections, but possibly 
  # total income for init data

# Travis: add file for acreage projections
    
  
drivers<-full_join(pop,inc); rm(pop,inc)
drivers$inc <- drivers$pop*drivers$inc
# but there might be a difference between personal and total income
# Travis sent a note to D Wear asking about this

#### I'm wondering if it would be best to have Ryan work directly with the data input files here.

# Match scales for initial and drivers data
subset(init, fips=='1001')[, c('pop','inc')]
subset(drivers, fips=='1001' & year==2015)

scaleI<-median(floor(log10(init$pop)),na.rm=T)
scaleD<-median(floor(log10(subset(drivers, year==2015)$pop)))
  init$pop<-init$pop*10^(scaleD-scaleI)

scaleI<-median(floor(log10(init$inc)),na.rm=T)
scaleD<-median(floor(log10(subset(drivers, year==2015)$inc)))
  init$inc<-init$inc*10^(scaleD-scaleI) ### SOMETHING IS AMISS HERE!!!
  # Travis is looking into it.. seems to be two different data sources
  


# Pull projection values from initial and drivers dataframes                           
   init.yr<-unique(init$year); stopifnot(length(init.yr)==1)

   proj.yrs<-sort(unique(drivers$year))
   proj.scenario<-unique(drivers$scenario)
   proj.model<-NA

   both.fips<-intersect(init$fips, drivers$fips)
   both.sector<-c('dp','ic')

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
                    ifelse(sector == "ic", inc, NA))) %>% 
    select(-(pop:inc))
  return(projDF)
}

wd <- addDrivers(wd, drivers)

# This function addInitialValues does XXXXXXX 
#    -> adds initial driver value and wpu to projection DF
addInitalValues <- function (projDF, initDF) 
{
  #browser()
  init.drivers<-select(initDF, c('fips','year', 'pop', 'inc'))  %>%
                 gather('sector', 'driver', pop:inc)          %>%
                   mutate(sector = ifelse(sector == "pop", "dp", 
                    ifelse(sector == "inc", "ic", NA)))

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

wd$wpu<as.numeric(wd$wpu)

# This function calcWPU 
# -> inputs growth and decay rates from initial DF
# -> calculates withdrawals per unit trends for all sectors
calcWPU <- function (projDF, initDF) 
{
  #browser()
  init.rates<-select(init, 'fips','year',contains('growth'), contains('decay')) %>%
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

# This function calcWD does XXXXXX
# -> calculates wd from wpu and driver data
calcWD <- function (dF) 
{
  dF <- dF %>% mutate(wd = wpu * driver)
  return(dF)
} 

wd<-calcWD(wd)                                             

write.csv(wd, "wd_projections") 

#### is there a way to make county-level plots of the data?

cnty <- maps_data("county")
head(cnty)
cnty_base <- ggplot(data=cnty, mapping=aes(x=long, y=lat, group=group)) +
                      coord_fixed(1.3) + 
                      geom_polygon(color="black", fill="gray")
                    cnty_base + theme_nothing()


