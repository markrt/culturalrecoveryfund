### how does CRF money
# fit with IMD ranking
# and how does that fit
# with other ACE spend


# load packages
library(tidyverse)
library(readxl)
library(scales)
library(janitor)
library(ggrepel)
library(ggridges)
library(ggforce)

# crf
crf_money_round1 <- 
  read_excel("CRF investment data published 020421.xlsx", 2) %>% 
  select(-Strand) %>% 
  mutate(source = "Grants round 1")
crf_money_capitalkickstart <- 
  read_excel("CRF investment data published 020421.xlsx", 3) %>% 
  mutate(source = "Capital kickstart")
crf_money_round2 <- 
  read_excel("CRF investment data published 020421.xlsx", 4) %>% 
  mutate(source = "Grants round 2")


# bind crf sources
crf_money <- 
  bind_rows(crf_money_round1,
            crf_money_round2,
            crf_money_capitalkickstart)

# load npo data
npo_data <- 
  read_excel("NPO_2018_12082020_0.xlsx") %>% 
  mutate(Organisation = 
           `Applicant Name`)

# load imd data
imd <- 
  read_excel("File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx",
             2,
             .name_repair = "universal")



### for each local authority,
# get NPO and non-NPO money

npo_names <- 
  npo_data %>% 
  select(Organisation) %>% 
  pull()

# add flag to crf money
# of whether it's an npo or not

crf_money <- 
  crf_money %>% 
  mutate(is_npo = 
           if_else(Organisation %in% npo_names,
                   "NPO",
                   "Not NPO")) 

# get data frame with one row per LA
# with data about money to NPO and non-NPOs

la_money_each <- 
  crf_money %>% 
  group_by(`Local Authority`, is_npo) %>% 
  summarise(money = sum(`£ Awarded`)) %>% 
  pivot_wider(names_from = is_npo,
              values_from = money) %>% 
  mutate(`Not NPO` = 
           replace_na(`Not NPO`, 0),
         NPO = 
           replace_na(NPO, 0)) %>% 
  mutate(percent_not_npo = 
           (`Not NPO` / 
              (`Not NPO` + NPO))*100) 

# find stray errors
imd %>% 
  rename("Local Authority" = 
           "Local.Authority.District.name..2019.") %>% 
  full_join(la_money_each) %>% 
  filter(is.na(IMD.2019...Extent))
# Bucks is new as of 2020

# South Bucks is the most average of the old Bucks LAs
imd %>% 
  rename("Local Authority" = 
           "Local.Authority.District.name..2019.") %>% 
  full_join((la_money_each %>% 
               mutate(`Local Authority` = 
                        fct_recode(`Local Authority`,
                                   "South Bucks" = 
                                     "Buckinghamshire")))) %>% 
  filter(is.na(IMD.2019...Extent))

# fine

# try other way round

imd %>% 
  rename("Local Authority" = 
           "Local.Authority.District.name..2019.") %>% 
  full_join(la_money_each) %>% 
  filter(is.na(NPO))
# from before:
# Brentwood, Welwyn Hatfield, Gosport, Isles of Scilly all no £
# Northamptonshire has two new LAs:
# West Northants comprises the old Daventry, Northampton, South Northants,
# North Northants has Corby, East Northants, Kettering, Wellingborough

# needs fixing, but check at first


imd %>% 
  rename("Local Authority" = 
           "Local.Authority.District.name..2019.") %>% 
  right_join(la_money_each) %>% 
  ggplot() +
  aes(x = (`Not NPO` + NPO),
      y = IMD...Average.rank) +
  geom_point() +
  scale_x_log10()


imd %>% 
  rename("Local Authority" = 
           "Local.Authority.District.name..2019.") %>% 
  right_join(la_money_each) %>% 
  ggplot() +
  aes(x = percent_not_npo,
      y = IMD...Average.rank) +
  geom_point() +
  scale_x_log10()
