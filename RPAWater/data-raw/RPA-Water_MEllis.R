# clear memory
rm(list = ls())

#setwd("D:/5_RPA_Water/RPA Water Model")
save_to<-'C:\\Users\\martha.ellis\\Documents\\GitHub\\RPAWater\\RPAWater\\data\\'

# library(nlme)
library(dplyr)
library(tidyr)

# read in data

year <- c(2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060,
          2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100)

watershed <- c(1401,1402,1403,1501,1502,1503)

sector <- c('dp', 'ic', 'thermo', 'irrig', 'livestock')

scenario <- c('a1b', 'a2', 'b2')

model <- c("CGC.A1B", "CGC.A2","CGC.B2","CSIRO.A1B","CSIRO.A2","CSIRO.B2","MIROC.A1B","MIROC.A2","HAD.B2")

num_years <- length(year)
num_sheds <- length(watershed)
num_scenarios <- length(scenario)
num_models <- length(model)


source('RPA-PopData.R')

### Drivers ####################################################################
# Dataframe with population data
pop<-rbind(data.frame(ASR=as.numeric(row.names(popdataA1)),scenario='a1b', popdataA1), 
           data.frame(ASR=as.numeric(row.names(popdataA2)),scenario='a2', popdataA2), 
           data.frame(ASR=as.numeric(row.names(popdataB2)),scenario='b2', popdataB2))
pop<-gather(pop, "year", "pop",-c(1:2) ) 
  pop$year<-as.numeric(substr(pop$year, 2,5))

# Dataframe with income data
inc<-rbind(data.frame(ASR=as.numeric(row.names(incdataA1)),scenario='a1b', incdataA1), 
           data.frame(ASR=as.numeric(row.names(incdataA2)),scenario='a2', incdataA2), 
           data.frame(ASR=as.numeric(row.names(incdataB2)),scenario='b2', incdataB2))
inc<-gather(inc, "year", "inc",-c(1:2) ) 
  inc$year<-as.numeric(substr(inc$year, 2,5))
  
# Combining drivers across sectors into one dataframe
drivers<-full_join(pop,inc)
save(drivers, file=paste0(save_to,'drivers.rdata'))

### Initial water demand #######################################################
yr1<-2010  
wd2010<-data.frame(WRR=as.numeric(substr(row.names(wd0),1,2)), ASR=as.numeric(row.names(wd0)), wd0)
  wd2010<-gather(wd2010, 'sector', 'wd', -c(1:2) )
  wd2010$sector<-factor(wd2010$sector, levels=sector)  

# Empty dataframe for water demand projections  
wd<-expand.grid(year=year, ASR=watershed,
  sector=sector,scenario=scenario, model=model)
  wd<-left_join(wd,drivers, by=c('ASR','scenario','year')) %>% 
        mutate(driver=ifelse(sector=='dp', pop,
                      ifelse(sector=='ic', inc, NA))) %>%
        select(-(pop:inc))  
  wd<-left_join(wd,data.frame(year=yr1, wd2010), by=c('year','ASR','sector'))
    wd<- wd %>% mutate(WRR=floor(ASR*1e-2)) %>% select(year, WRR, ASR:wd)
  

### Growth and decay rates #####################################################
# Need to figure out how
# to estimate these based on historic data
g_eDP = -0.0066     
g_wDP = -0.0035
d_eDP = -0.03
d_wDP = -0.03

g_eIC = -0.0331
g_wIC = -0.0578
d_eIC = -0.0350
d_wIC = -0.0420

gd_rates<-data.frame(region=rep(c('East','West'),length.out=4),
                     sector=rep(c('dp','ic'), each=2),
                     g=c(g_eDP, g_wDP,g_eIC, g_wIC),
                     d=c(d_eDP, d_wDP,d_eIC, d_wIC))
save(gd_rates,file=paste0(save_to, 'gd_rates.rdata'))                     

# Notes on data:
# Units: water in MGD unless stated otherwise. Fresh water only unless stated otherwise
# (an exception, for example, is that total thermo kWhs are also listed, which includes salt water use).
# Surface water + ground water. For 1985-1995 the numbers differ from what you get when you sum across
# counties (assigning each county to a WRR). For year 2000 summation across counties was necessary,
# as estimates were not provided for watersheds. For earlier years, water use and some other items
# from Michelle's USE-ALL.WQ2, which gives numbers that in many cases do not agree with the USGS
# circulars (apparently they were taken from computer files, and sometimes aggregated from smaller units).
# Usually the difference is within 5%, but it can be considerably larger, especially for small amounts.
# Differs from USE-WRX8.XLS: has I&C from circulars (not from Guldin's spreadsheet).

# Beginning in 2000 with the Hutson et al. circular the USGS did not break down public supply
# withdrawals into separate categories for domestic, industrial and commercial, and thermoelectric use.
# To allow us to contiue to use the categories that were established in earlier circulars, we applied
# percentages based on the 1995 data to the USGS public supply estimates for 2000 and 2005.

# Note that in 2005 (the Kenny et al. circular) they did give the percent of total public supply
# withdrawal that was for domentic use, but they lumped the rest (commerical, industrial, public
# use and losses). I did not try to take advantage of this percentage, preferring to rely on the
# 1995 percentages, in part because the 2005 circular did not even provide an estimate of total public
# supply D&P (the provided only D, not P).

# The livestock estimates for 1990 and 1995 are from the circulars, rather than from the aggregated
# county or huc data, with the exception of WRR 9, which had some problematic zeros in the circular.
# I made this change on 13 Jan 10 (at this point, I'm not sure which source would be better).

##### Types of use #############
#DP: Domestic and Public
#IC: Industrial and Commercial
#TF: Thermoelectric
#IR: Irrigation
#AQ: Aquaculture

# The general formula is withdrawal = units * wpu
# where wpu is withdrawal per demand unit


# Initial values for 2010                                          
wd <- wd %>% group_by(sector, scenario, model, year, WRR) %>% 
  mutate(wpu=sum(wd)/sum(driver)*1e6 ) %>% ungroup()

# Calculate WPU by WRR  
wd <- wd %>% group_by(sector, scenario, model, ASR) %>%
  mutate(fc=ifelse(year>yr1,((1+g_wDP*(1+d_wDP)^(year-yr1))^5),1), 
  wpu=wpu[1]*cumprod(fc)) %>% select(-fc)
  
# Calculate water demand for each scenario  
wd<- wd %>%  mutate(wd=ifelse(year>yr1, wpu*driver/1e6, wd)) ##doesn't match a2,b2 due to initial pop in wpu calcs


### CLIMATE EFFECTS ############################################################
eta_precip <- 1.415
eta_et <- 0.778

# Dataframe with climate data
precipdata<-do.call('rbind',lapply(model, function(x) {
  tmp<-get(paste0('precipdata_',gsub('\\.','',x)))
  data.frame(model=x, ASR=as.numeric(row.names(tmp)), tmp)}))
  precipdata<-gather(precipdata, 'year','precipdata',-c(1:2))
  
etdata<-do.call('rbind',lapply(model, function(x) {
  tmp<-get(paste0('etdata_',gsub('\\.','',x)))
  data.frame(model=x, ASR=as.numeric(row.names(tmp)), tmp)}))
  etdata<-gather(etdata, 'year','etdata',-c(1:2))

# Join precipitation and et
climate<-full_join(precipdata, etdata)
  climate$year<-as.numeric(substr(climate$year,2,5))
  climate$sector<-factor('dp', levels=levels(wd$sector))
  climate$wpu_climate <- 0

# Calculate wpu under climate scenarios
climate <- climate %>% group_by(model, ASR) %>% 
    mutate(wpu_climate=ifelse(year==yr1, 0,
        eta_precip * 6 * ((precipdata[1]-precipdata)/6 + 
               0.016*((precipdata/6)^2-(precipdata/6)^2)) )) +
         eta_et * ((etdata-etdata[1])*365/10)
    rm(precipdata, etdata)

# Calculate total water demand with climate
wd <- left_join(wd, select(climate, -(precipdata:etdata))) %>%
  mutate(wd_tot=driver*(wpu+wpu_climate)/1e6)     ## Should this be divided by 10^6?             

wd
