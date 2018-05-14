#### is there a way to make county-level plots of the data?

cnty <- maps_data("county")
head(cnty)
head(county.fips) ## from maps package

# Single map
map.data<- cnty %>% mutate(polyname=paste(region,subregion, sep=',')) %>%
        select(-region, -subregion) %>%
        left_join(county.fips) %>%
        left_join((
          filter(wd, sector=='dp' & scenario=='SSP1' & year %in% c(2010,2070)) %>%
          summarise(p_wd=(wd[year==2070]-wd[year==2010])/wd[year==2010]*100)%>%
          mutate(p_wd=cut(p_wd, breaks=c(-Inf,0,10,20,40,70,150,Inf)))%>%
          ungroup() %>% select(fips, p_wd)
          ))

cnty_base <- ggplot(data=map.data, mapping=aes(x=long, y=lat, group=group, fill=p_wd)) +
                      geom_polygon(color=grey(.1)) +
                      scale_fill_manual("Percent change 2010 to 2070",
                          values=c('lightblue', RColorBrewer::brewer.pal(6, 'Reds')))+
                      coord_map()  +ggthemes::theme_map() + 
                      theme(legend.position=c(0,0))
                      
# All scenarios
map.data<- cnty %>% mutate(polyname=paste(region,subregion, sep=',')) %>%
        select(-region, -subregion) %>%
        left_join(county.fips) %>%
        left_join((
          filter(wd, sector=='dp' & year %in% c(2010,2070)& !is.na(scenario)) %>%
          summarise(p_wd=(wd[year==2070]-wd[year==2010])/wd[year==2010]*100)%>%
          mutate(p_wd=cut(p_wd, breaks=c(-Inf,0,10,20,40,70,150,Inf)))%>%
          ungroup() %>% select(fips, scenario, p_wd)
          ))

cnty_panel <- ggplot(data=droplevels(filter(map.data,!is.na(scenario))),
               mapping=aes(x=long, y=lat, group=group, fill=p_wd)) +
                      geom_polygon(color=grey(.1)) +
                      scale_fill_manual("Percent change 2010 to 2070",
                          values=c('lightblue', RColorBrewer::brewer.pal(6, 'Reds')))+
                      coord_map()  +ggthemes::theme_map() + 
                      theme(legend.position=c(0.8,.1))  +facet_wrap(~scenario)
                      
pdf('CountyPlots.pdf',width=20,height=10)
print(cnty_base)
print(cnty_panel)
dev.off()                       