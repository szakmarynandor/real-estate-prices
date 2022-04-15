library(tidyverse)
library(sf)

dat <- readxl::read_excel("Lakásár_területi_panel.xlsx", sheet = "2020_Bpvel") # adatsor excel füle

dat <- dat %>% 
  mutate_at(-1, function(x) as.numeric(x))

dat <- dat[apply(dat, 1, function(x) sum(!is.na(x)) > 1), ] %>% 
  select(1, 4) %>% # adatsor oszlopa
  set_names(c('NAME', 'value'))
  
p <-  merge(dat, read_sf('kozighatarok/admin7.shp'), all.y = T) %>% 
  ggplot() +
  geom_sf(aes(fill = value, geometry = geometry, text = paste0(NAME, ':\n', value)), color ='black') + 
  geom_sf(data = merge(read_sf('kozighatarok/admin9.shp'), dat, all.x = T), 
          mapping = aes(fill = value, geometry = geometry, text = paste0(NAME, ':\n', value)), color = 'black') +
  theme_void() + 
  labs(fill = NULL, title = 'Átlagos használt lakóingatlan árak járásonként', subtitle = "2020")+
  scale_fill_continuous(low = "white", high = "#1B5E20", name = "Millió Ft", label = scales::comma,
                        guide = guide_colorbar(ticks.colour = 'black', frame.colour = 'black',
                                               frame.linewidth = 1.5, ticks.linewidth = 1))+
  theme(
    plot.title = element_text(hjust = 0.5, vjust=-3.5),
    plot.subtitle = element_text(hjust = 0.5, vjust = -6),
    text = element_text(family = "serif", size=28),
    legend.title = element_text(size=24, hjust = -1),
    legend.text = element_text(size=18),
    legend.position = c(0.95, 0.375),
    legend.key.size = unit(1.25, 'cm')
  )

p


library(plotly)

ggplotly(p, tooltip = c('text')) %>% 
  config(displayModeBar = F)
  

