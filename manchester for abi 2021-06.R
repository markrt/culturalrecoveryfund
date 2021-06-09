##### abi's asked for some crf data for manc
# in an email of 2021-06-03
# let's see if i can do it

# load packages

library(tidyverse)
library(readxl)
library(scales)
library(janitor)
library(ggrepel)
library(ggridges)
library(ggforce)
library(sf)
library(plotly)
library(colorspace)
library(patchwork)

# load data
source("load_data.R")

# look at it
# add it all together
money_population_geography %>% 
  filter(Local.Authority == "Manchester" | # just GM authorities
           Local.Authority == "Bolton" |
           Local.Authority == "Bury" |
           Local.Authority == "Oldham" |
           Local.Authority == "Rochdale" |
           Local.Authority == "Salford" |
           Local.Authority == "Stockport" |
           Local.Authority == "Tameside" |
           Local.Authority == "Trafford" |
           Local.Authority == "Wigan" 
  ) %>% 
  distinct() %>% 
  select(Local.Authority,
         geometry,
         money_per_head) %>% 
  group_by(Local.Authority) %>% 
  mutate(money_per_head = sum(money_per_head)) %>% 
  distinct() %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  scale_fill_viridis_c(labels = function(x) paste0("£", x)) +
  theme(legend.position = "bottom") +
  labs(fill = "",
       title = "Overall £ per head, GM local authorities")

# do it separately 
manc_data <- 
  money_population_geography %>% 
  filter(Local.Authority == "Manchester" | # just GM authorities
           Local.Authority == "Bolton" |
           Local.Authority == "Bury" |
           Local.Authority == "Oldham" |
           Local.Authority == "Rochdale" |
           Local.Authority == "Salford" |
           Local.Authority == "Stockport" |
           Local.Authority == "Tameside" |
           Local.Authority == "Trafford" |
           Local.Authority == "Wigan" 
  )

manc_01 <- 
  manc_data %>% 
  filter(Source == "ACE capital kickstart") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "ACE capital kickstart") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))

manc_02 <- 
  manc_data %>% 
  filter(Source == "ACE grants round 1") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "ACE grants round 1") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))

manc_03 <- 
  manc_data %>% 
  filter(Source == "ACE grants round 2") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "ACE grants round 2") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))
 
manc_04 <- 
  manc_data %>% 
  filter(Source == "BFI") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "BFI") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))

manc_05 <- 
  manc_data %>% 
  filter(Source == "NLHF") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "NLHF") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))

manc_01 + 
  manc_02 + 
  manc_03 + 
  manc_04 + 
  manc_05 + plot_annotation(title = "CRF money by GM local authority")

##### same thing again, but by region


nw_01 <- 
  money_population_geography %>% 
  filter(Region.ONS == "North West")%>% 
  filter(Source == "ACE capital kickstart") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "ACE capital kickstart") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))

nw_02 <- 
  money_population_geography %>% 
  filter(Region.ONS == "North West")%>% 
  filter(Source == "ACE grants round 1") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "ACE grants round 1") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))

nw_03 <- 
  money_population_geography %>% 
  filter(Region.ONS == "North West")%>% 
  filter(Source == "ACE grants round 2") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "ACE grants round 2") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))

nw_04 <- 
  money_population_geography %>% 
  filter(Region.ONS == "North West")%>% 
  filter(Source == "BFI") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "BFI") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))

nw_05 <- 
  money_population_geography %>% 
  filter(Region.ONS == "North West")%>% 
  filter(Source == "NLHF") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "NLHF") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))


nw_01 + 
  nw_02 + 
  nw_03 + 
  nw_04 + 
  nw_05 + plot_annotation(title = "CRF money for the North West local authorities")


#### north east


ne_01 <- 
  money_population_geography %>% 
  filter(Region.ONS == "North East")%>% 
  filter(Source == "ACE capital kickstart") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "ACE capital kickstart") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))

ne_02 <- 
  money_population_geography %>% 
  filter(Region.ONS == "North East")%>% 
  filter(Source == "ACE grants round 1") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "ACE grants round 1") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))

ne_03 <- 
  money_population_geography %>% 
  filter(Region.ONS == "North East")%>% 
  filter(Source == "ACE grants round 2") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "ACE grants round 2") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))

ne_04 <- 
  money_population_geography %>% 
  filter(Region.ONS == "North East")%>% 
  filter(Source == "BFI") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "BFI") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))

ne_05 <- 
  money_population_geography %>% 
  filter(Region.ONS == "North East")%>% 
  filter(Source == "NLHF") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "NLHF") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))


ne_01 + 
  ne_02 + 
  ne_03 + 
  ne_04 + 
  ne_05 + plot_annotation(title = "CRF money for the North East local authorities")

### yorkshire 


yh_01 <- 
  money_population_geography %>% 
  filter(Region.ONS == "Yorkshire and The Humber")%>% 
  filter(Source == "ACE capital kickstart") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "ACE capital kickstart") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))

yh_02 <- 
  money_population_geography %>% 
  filter(Region.ONS == "Yorkshire and The Humber")%>% 
  filter(Source == "ACE grants round 1") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "ACE grants round 1") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))

yh_03 <- 
  money_population_geography %>% 
  filter(Region.ONS == "Yorkshire and The Humber")%>% 
  filter(Source == "ACE grants round 2") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "ACE grants round 2") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))

yh_04 <- 
  money_population_geography %>% 
  filter(Region.ONS == "Yorkshire and The Humber")%>% 
  filter(Source == "BFI") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "BFI") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))

yh_05 <- 
  money_population_geography %>% 
  filter(Region.ONS == "Yorkshire and The Humber")%>% 
  filter(Source == "NLHF") %>% 
  ggplot() +
  aes(fill = money_per_head,
      geometry = geometry) +
  geom_sf() +
  theme_void() +
  labs(fill = "",
       subtitle = "NLHF") +
  theme(legend.position = "bottom") +
  scale_fill_viridis_c(labels = function(x) paste0("£", x))


yh_01 + 
  yh_02 + 
  yh_03 + 
  yh_04 + 
  yh_05 + plot_annotation(title = "CRF money for the Yorkshire & the Humber local authorities")


#### levelling up fund

level_up <- 
  read_excel("Levelling_Up_Fund_list_of_local_authorities_by_priority_category GM markup.xlsx")

names(level_up) <- c("level_up_country",
                     "Local.Authority",
                     "level_up_priority")


money_population_geography %>% 
  full_join(level_up) %>% 
  filter(level_up_country == "England") %>% 
  filter(Local.Authority != "City of London") %>% 
  group_by(Local.Authority,
           level_up_priority) %>% 
  summarise(total_money_per_head = 
           sum(money_per_head)) %>% 
  ggplot() +
  aes(y = fct_rev(as.factor(level_up_priority)),
      x = total_money_per_head,
      fill = as.factor(level_up_priority)) +
  geom_density_ridges() +
  scale_x_log10(breaks = c(0.1,
                           1,
                           10,
                           100),
                labels = c("10p",
                           "£1",
                           "£10",
                           "£100")) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  labs(x = "Total CRF money per head, per local authority",
       y = "Levelling up fund priority group") 

)



### imd cleanups
# involves doing stuff in imd first
# sorted in for_abi_june.Rmd

la_money_each %>% 
  full_join(population %>% 
              rename(Local.Authority = 
                       la_name)) %>% 
  mutate(money_per_head = 
           money/population) %>% 
  full_join(imd %>% 
              rename("Local.Authority" = 
                       "Local.Authority.District.name..2019.")) %>% 
  filter(Local.Authority != 
           "City of London") %>% 
  ggplot() +
  aes(x = money_per_head,
      y = IMD...Average.rank) +
  geom_point() +
  scale_x_log10(breaks = c(1,
                           10,
                           100),
                labels = c("£1",
                           "£10",
                           "£100")) +
  theme_minimal() +
  labs(x = "£ per head",
       y = "Average IMD rank per local authority")


la_money_each %>% 
  full_join(population %>% 
              rename(Local.Authority = 
                       la_name)) %>% 
  mutate(money_per_head = 
           money/population) %>% 
  full_join(imd %>% 
              rename("Local.Authority" = 
                       "Local.Authority.District.name..2019.")) %>% 
  filter(Local.Authority != 
           "City of London") %>% 
  select(money_per_head,
      IMD...Average.rank) %>% 
  mutate(log_money_per_head = 
           log(money_per_head)) %>% 
  na.omit %>% 
  cor()



nw_01 + 
  plot_spacer() +
  nw_02 + 
  plot_spacer() +
  nw_03 + 
  plot_spacer() +
  nw_04 + 
  plot_spacer() +
  nw_05 + plot_annotation(title = "CRF money for the North West local authorities")


nw_layout <- "
  AA#BB#CC
  AA#BB#CC
  #DD##EE#
  #DD##EE#
  "

nw_01 + 
  nw_02 + 
  nw_03 + 
  nw_04 + 
  nw_05 + plot_annotation(title = "CRF money for the North West local authorities") +
  plot_layout(design = nw_layout)
