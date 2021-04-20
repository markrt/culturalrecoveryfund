# already looked at money per constituency
# and per la
# time to do per art form

# load packages
library(tidyverse)
library(readxl)
library(scales)
library(janitor)
library(ggrepel)
library(ggridges)
library(ggforce)


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


# combine crf money: ace
crf_money <- 
  bind_rows(crf_money_round1,
            crf_money_round2,
            crf_money_capitalkickstart)

# crf money per discipline: ace
crf_money %>% 
  group_by(Discipline) %>% 
  summarise(total_money = 
              sum(`£ Awarded`)) %>% 
  arrange(-total_money)

# crf money per discipline once you add bfi
bfi_money %>% 
  rename("Organisation" = 
           "Applicant Name",
         "£ Awarded" = 
           "Award (£)",
         "Region ONS" = 
           "Region",
         "Local Authority" =
           "Local authority") %>% 
  select(-c("Cinema",
            "Award per Cinema (£)")) %>% 
  bind_rows((crf_money %>% 
               select(-c(`Applicant ACE Area`,
                         source)))) %>% 
  group_by(Discipline) %>% 
  summarise(total_money = 
              sum(`£ Awarded`)) %>% 
  arrange(-total_money)

# attempt to add nlhf as well, a bit shakier this one
bfi_money %>% 
  rename("Organisation" = 
           "Applicant Name",
         "£ Awarded" = 
           "Award (£)",
         "Region ONS" = 
           "Region",
         "Local Authority" =
           "Local authority") %>% 
  select(-c("Cinema",
            "Award per Cinema (£)")) %>% 
  bind_rows((crf_money %>% 
               select(-c(`Applicant ACE Area`,
                         source)))) %>% 
  bind_rows(nlhf_money %>% 
  rename("Organisation" = 
           "Applicant",
         "Discipline" = 
           "Heritage Area",
         "Local Authority" = 
           "Local authority",
         "Region ONS" = 
           "Region",
         "£ Awarded" =  
          "Award (£)") ) %>% 
  group_by(Discipline) %>% 
  summarise(total_money = 
              sum(`£ Awarded`)) %>% 
  arrange(-total_money)
