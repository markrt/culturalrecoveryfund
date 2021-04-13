#### 2021-04-13
# Abi's asked for money per local authority
# i can do that

# load packages
library(tidyverse)
library(readxl)
library(scales)
library(janitor)
library(ggrepel)
library(ggridges)
library(ggforce)

# load data
# nb i can't find the repayable finance round 1
# ace says it's here
# https://www.gov.uk/government/news/more-than-165-million-in-repayable-finance-announced-to-support-major-arts-and-heritage-institutions-as-culture-fund-marks-1-billion-milestone 
# ref from here 
# https://www.artscouncil.org.uk/publication/culture-recovery-fund-data
# the second round is fine, but the first round is an issue

# crf money: from the ace site
crf_money_round1 <- 
  read_excel("CRF investment data published 020421.xlsx", 2) %>% 
  select(-Strand) %>% 
  mutate(source = "ACE grants round 1")
crf_money_capitalkickstart <- 
  read_excel("CRF investment data published 020421.xlsx", 3) %>% 
  mutate(source = "ACE capital kickstart")
crf_money_round2 <- 
  read_excel("CRF investment data published 020421.xlsx", 4) %>% 
  mutate(source = "ACE grants round 2")

# crf money: from dcms
# https://www.gov.uk/government/news/400-million-to-help-more-than-2700-arts-culture-heritage-organisations-and-independent-cinemas-survive-and-thrive
# sheet 1 seems to be the same, so steering clear for the moment
# bfi: weird missingness, trying to deal by replacing NAs with "per cinema"
bfi_money <- 
  read_excel("CRF_2-Awards_read-only.xlsx", 2, 
             skip = 1,
             na = "-") %>% 
  mutate(`Award (£)` = 
           coalesce(`Award (£)`, 
                    `Award per Cinema (£)`))
# nlhf: looks ok
nlhf_money <- 
  read_excel("CRF_2-Awards_read-only.xlsx", 3, 
           skip = 1)

# repayable finance from dcms - seems fine
repayable_finance <- 
  read_excel("CRF_2-Awards_read-only.xlsx", 4, 
           skip = 1) %>% 
  mutate(`Local authority` = fct_recode(`Local authority`,
                              "York" = 
                                "City of York"))


# population data from 2019
# has updated local authority codes
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
population <- 
  read_excel("ukmidyearestimates20192020ladcodes.xls", 
           6,
           skip = 4
           ) %>% 
  select(Code:`All ages`) %>% 
  rename(la_name = 
           Name,
         population = 
           `All ages`) %>% 
  filter(Geography1 != "Country" &
           Geography1 != "Region" &
           Geography1 != "Metropolitan County" &
           Geography1 != "County"
  ) %>% 
  mutate(country = substr(Code, 
                          1,
                          1)) %>% 
  filter(country == "E") %>% 
  select(-country)



# combine crf money
crf_money <- 
  bind_rows(crf_money_round1,
            crf_money_round2,
            crf_money_capitalkickstart)




# ok, time to get summary data for each data frame
bfi_money_per_la <- 
  bfi_money %>% 
  rename(la_name = 
           `Local authority`,
         money = 
           `Award (£)`) %>% 
  group_by(la_name) %>% 
  summarise(money_per_la = 
              sum(money)) %>% 
  mutate(source = "bfi")
  
nlhf_money_per_la <- 
  nlhf_money %>% 
  rename(la_name = 
           `Local authority`,
         money = 
           `Award (£)`) %>% 
  group_by(la_name) %>% 
  summarise(money_per_la = 
              sum(money)) %>% 
  mutate(source = "nlhf")

repayable_money_per_la <- 
  repayable_finance %>% 
  rename(la_name = 
           `Local authority`,
         money = 
           `Offer (£)`) %>% 
  group_by(la_name) %>% 
  summarise(money_per_la = 
              sum(money)) %>% 
  mutate(source = "repayable_finance")

crf_money_per_la <- 
  crf_money %>% 
  rename(la_name = 
           `Local Authority`,
         money = 
           `£ Awarded`) %>% 
  group_by(source, la_name) %>% 
  summarise(money_per_la = 
              sum(money))

# bring the summary by constituency together
all_sources_money_per_la <- 
  bind_rows(bfi_money_per_la,
          crf_money_per_la,
          nlhf_money_per_la,
          repayable_money_per_la)

# check those that are missing
# first: in LA dataset, not £ datasets
full_join(population,
          all_sources_money_per_la) %>% 
  filter(is.na(source)) 
# four: Brentwood, Welwyn Hatfield, Gosport, Isles of Scilly

# second: in £ dataset, not LA datasets
full_join(population,
          all_sources_money_per_la) %>% 
  filter(is.na(Code)) 
# all Wales or Scotland

# join, write
full_join(population,
          all_sources_money_per_la) %>% 
  mutate(money_per_head = 
           round(money_per_la / 
           population, 2)) %>% 
  arrange(la_name) %>% 
  arrange(source) %>% 
  select(-Code, Geography1) %>% 
  write_csv("crf_money_local_authority.csv")


