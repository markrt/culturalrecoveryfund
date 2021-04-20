#### we know that there's a v strong relationship
# between areas (constituencies, LAs, whatever)
# getting CRF funding
# and getting more ACE money more generally

# load packages
library(tidyverse)
library(readxl)
library(scales)
library(janitor)
library(ggrepel)
library(ggridges)
library(ggforce)


# loading data
# election
election_2019 <- 
  read_csv("HoC-GE2019-results-by-constituency-csv.csv")
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


# clean up election
# filter election to just england
election_2019 <- 
  election_2019 %>% 
  filter(country_name == "England")

# renaming column for merge
election_2019$Constituency <- 
  election_2019$constituency_name

# tweak inconsistencies in constituency names
# make those tweaks in the election_2019 data
election_2019$Constituency[election_2019$constituency_name == 
                             "Berwick-Upon-Tweed"] <- 
  "Berwick-upon-Tweed"
election_2019$Constituency[election_2019$constituency_name == 
                             "Cities Of London and Westminster"] <- 
  "Cities of London and Westminster"
election_2019$Constituency[election_2019$constituency_name == 
                             "Cities Of London and Westminster"] <- 
  "Cities of London and Westminster"
election_2019$Constituency[election_2019$constituency_name == 
                             "City Of Chester"] <- 
  "City of Chester"
election_2019$Constituency[election_2019$constituency_name == 
                             "City Of Durham"] <- 
  "City of Durham"
election_2019$Constituency[election_2019$constituency_name == 
                             "Isle Of Wight"] <- 
  "Isle of Wight"
election_2019$Constituency[election_2019$constituency_name == 
                             "Newcastle Upon Tyne Central"] <- 
  "Newcastle upon Tyne Central"
election_2019$Constituency[election_2019$constituency_name == 
                             "Newcastle Upon Tyne North"] <- 
  "Newcastle upon Tyne North"

election_2019$Constituency[election_2019$constituency_name == 
                             "Newcastle Upon Tyne East"] <- 
  "Newcastle upon Tyne East"
election_2019$Constituency[election_2019$constituency_name == 
                             "Newcastle-Under-Lyme"] <- 
  "Newcastle-under-Lyme"
election_2019$Constituency[election_2019$constituency_name == 
                             "Stoke-On-Trent Central"] <- 
  "Stoke-on-Trent Central"
election_2019$Constituency[election_2019$constituency_name == 
                             "Stoke-On-Trent South"] <- 
  "Stoke-on-Trent South"
election_2019$Constituency[election_2019$constituency_name == 
                             "Stratford-On-Avon"] <- 
  "Stratford-on-Avon"
# election_2019$Constituency[election_2019$constituency_name == 
#                              "Weston-Super-Mare"] <- 
#   "Weston-super-Mare"
election_2019$Constituency[election_2019$constituency_name == 
                             "Forest Of Dean"] <- 
  "Forest of Dean"
election_2019$Constituency[election_2019$constituency_name == 
                             "Ashton-Under-Lyne"] <- 
  "Ashton-under-Lyne"
election_2019$Constituency[election_2019$constituency_name == 
                             "Stoke-On-Trent North"] <- 
  "Stoke-on-Trent North"

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



# what are the non-npos getting big money?
crf_money %>% 
  filter(is_npo == "Not NPO") %>% 
  filter(`£ Awarded` > 1000000) %>% 
  select(Organisation, `£ Awarded`) %>% 
  print(n = 31)

# do money per constituency

crf_money %>% 
  filter(is_npo == "Not NPO") %>% 
  group_by(Constituency) %>% 
  summarise(total_crf_money = 
              sum(`£ Awarded`)) %>% 
  full_join(election_2019) %>% 
  mutate(total_crf_money = 
           replace_na(total_crf_money, 0)) %>% 
  group_by(first_party) %>% 
  summarise(money_by_party = 
              mean(total_crf_money))
# similar pattern for the whole pot,
# NPOs or not

# do it just for places that got money
crf_money %>% 
  filter(is_npo == "Not NPO") %>% 
  group_by(Constituency) %>% 
  summarise(total_crf_money = 
              sum(`£ Awarded`)) %>% 
  full_join(election_2019) %>% 
  na.omit %>% 
  # mutate(total_crf_money = 
  #          replace_na(total_crf_money, 0)) %>% 
  group_by(first_party) %>% 
  summarise(money_by_party = 
              mean(total_crf_money))
# similar pattern again

# what areas got the most money for each pot?

crf_money %>% 
  full_join(election_2019) %>% 
  group_by(is_npo, Constituency) %>% 
  summarise(total_money = 
              sum(`£ Awarded`)) %>% 
  pivot_wider(names_from = is_npo,
              values_from = total_money) %>% 
  mutate(NPO = replace_na(NPO, 0),
         `Not NPO` = replace_na(`Not NPO`, 0)) %>% 
  ggplot() +
  aes(x = NPO/1000,
      y = `Not NPO`/1000) + 
  geom_point() +
  geom_abline(slope = 1,
              intercept = 0) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) 
