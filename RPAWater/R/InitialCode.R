################################################################################
################################################################################

addDrivers<-function(dF){
  data(drivers)
  dF<-left_join(dF,drivers, by=c('ASR','scenario','year')) %>% 
        mutate(driver=ifelse(sector=='dp', pop,
                      ifelse(sector=='ic', inc, NA))) %>%
        select(-(pop:inc))
   return(dF) 
}  

################################################################################
################################################################################

addInitialValues<-function(dF, yr1=2010){
  if(yr1!=2010)
    stop('Only 2010 available as start year')
  data(wd2010)
  
  dF<-left_join(dF,data.frame(year=yr1, wd2010), by=c('year','ASR','sector'))
    dF<- dF %>% mutate(WRR=floor(ASR*1e-2)) %>% select(year, WRR, ASR:wd) %>% 
      group_by(sector, scenario, model, year, WRR) %>% 
    mutate(wpu=sum(wd)/sum(driver)*1e6 ) %>% ungroup()
  
  return(dF)
}

################################################################################
################################################################################

getGDrate<-function(ASR, sect){
  data(gd_rates)
  R<-ifelse(ASR %in% c(1401,1402,1403,1501,1502,1503), 'West','East')
  gd<-right_join(gd_rates, data.frame(region=R, sector=sect))[,c('g','d')]
  return(gd)
  }   
  
################################################################################
################################################################################

calcWPU<-function(dF, yr1=2010){
   data(gd_rates)
   dF<-dF %>% group_by(sector, scenario, model, ASR) %>%
    mutate(
      g=getGDrate(ASR, sector)[,2],
      d=getGDrate(ASR, sector)[,2],
      fc=ifelse(year>yr1,((1+g*(1+d)^(year-yr1))^5),1)), 
    wpu=wpu[1]*cumprod(fc)) %>% select(-fc)
    return(dF) 
}    

################################################################################
################################################################################

calcWD<-function(dF){
  dF<- dF %>%  mutate(wd=ifelse(year>yr1, wpu*driver/1e6, wd))
  return(dF)
}

################################################################################
################################################################################

projClimate<-function(dF){
  eta_precip <- 1.415
  eta_et <- 0.778
  
  data(climate)
  
  dF<- climate %>% group_by(model, ASR) %>% 
      mutate(wpu_climate=ifelse(year==yr1, 0,
        eta_precip * 6 * ((precipdata[1]-precipdata)/6 + 
               0.016*((precipdata/6)^2-(precipdata/6)^2)) )) +
         eta_et * ((etdata-etdata[1])*365/10)
  return(dF)
}

################################################################################
################################################################################

projectWater<-function(year=NULL, watershed=NULL, sector=NULL, scenario=NULL, model=NULL){
  if(is.null(year)) 
    year <- c(2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060,
            2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100)
  if(is.null(watershed))
    watershed <- c(1401,1402,1403,1501,1502,1503)  
  if(is.null(sector))
    sector <- c('dp', 'ic', 'thermo', 'irrig', 'livestock')  
  if(is.null(scenario))
    scenario <- c('a1b', 'a2', 'b2')  
  if(is.null(model))
    model <- c("CGC.A1B", "CGC.A2","CGC.B2","CSIRO.A1B","CSIRO.A2","CSIRO.B2","MIROC.A1B","MIROC.A2","HAD.B2")

  num_years <- length(year)
  num_sheds <- length(watershed)
  num_scenarios <- length(scenario)
  num_models <- length(model)

  wd<-expand.grid(year=year, ASR=watershed,
    sector=sector,scenario=scenario, model=model)
    
  wd<-addDrivers(wd)
  
  wd<-addInitialValues(wd, yr1=year[1])

  wd <-calcWPU(wd, yr1=year[1]) 

  wd<- calcWD(wd) 
  
  return(wd)
}

################################################################################
################################################################################