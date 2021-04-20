### loading packages
library(tidyverse)
library(readxl)
library(scales)
library(janitor)
library(ggrepel)
library(ggridges)
library(ggforce)

# loading data
election_2019 <- 
  read_csv("HoC-GE2019-results-by-constituency-csv.csv")
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

# joining
election_crf <- 
  full_join(election_2019,
            crf_money,
            by = "Constituency")

# check merge
election_crf %>% 
  filter(is.na(other_winner)) %>% 
  select(Constituency) %>% 
  table()
# Scottish and Welsh cases only, fine

# get cases where no money at all
election_crf %>% 
  group_by(constituency_name) %>% 
  summarise(total_money = sum(`£ Awarded`)) %>% 
  filter(is.na(total_money)) %>% 
  print(n = 38)

# how many in labour/tory constituencies?
election_crf %>% 
  group_by(constituency_name) %>% 
  summarise(total_money = sum(`£ Awarded`),
            first_party = first_party) %>% 
  filter(is.na(total_money)) %>% 
  tabyl(first_party)
# 22 con, 16 lab

# how many in both?
inner_join(crf_money_round1,
           crf_money_round2,
           by = "Organisation")
# 1058

### average money by political party
election_crf %>% 
  mutate(`£ Awarded` = 
           replace_na(`£ Awarded`, 0)) %>% 
  group_by(Constituency, 
           first_party) %>% 
  summarise(total_money = 
              sum(`£ Awarded`)) %>%
  group_by(first_party) %>% 
  summarise(mean_money = 
              mean(total_money))

# is that just a long tail issue?
election_crf %>% 
  mutate(`£ Awarded` = 
           replace_na(`£ Awarded`, 1)) %>% 
  filter(first_party == "Lab" |
           first_party == "Con") %>% 
  group_by(Constituency) %>% 
  summarise(total_money = sum(`£ Awarded`),
            first_party = first_party) %>% 
  distinct() %>% 
  ggplot() +
  aes(x = total_money,
      y = first_party) +
  geom_density_ridges() +
  scale_x_log10(labels = comma)

# yes
election_crf %>% 
  mutate(`£ Awarded` = 
           replace_na(`£ Awarded`, 0)) %>% 
  group_by(Constituency, 
           first_party) %>% 
  summarise(total_money = 
              sum(`£ Awarded`)) %>% 
  arrange(-total_money)



### load in other ace data
# nb just npos for the moment bc of constituency data
npo_data <- 
  read_excel("NPO_2018_12082020_0.xlsx")

names(npo_data)

# same thing, NPOs
npo_money <- 
  npo_data %>% 
  group_by(Constituency) %>% 
  summarise(npo_money = sum(`TOTAL Portfolio grant 18/22 - £`))

# clean up CRF money
crf_money <- 
  crf_money %>% 
  select(`£ Awarded`,
         Constituency) %>% 
  rename(crf_money = 
           `£ Awarded`) %>% 
  group_by(Constituency) %>% 
  summarise(crf_money = 
              sum(crf_money)) 

# handful of cases where there's an NPO
# but no CRF money
full_join(crf_money,
          npo_money) %>% 
  filter(is.na(crf_money))
# something up with West Thurrock?
# is ROH anyway, so not too bothered

# plot relationship between crf money
# and npo money
full_join(crf_money,
          npo_money) %>% 
  mutate(npo_money = 
           replace_na(npo_money, 1)) %>% 
  mutate(crf_money = 
           replace_na(crf_money, 1)) %>% 
  ggplot() +
  aes(x = npo_money/1000,
      y = crf_money/1000) +
  geom_point(aes()) +
  theme_light() + 
  scale_x_log10() +
  scale_y_log10() +
  facet_zoom(x = npo_money > 40000,
             y = crf_money > 40000)  + 
  # geom_label_repel(data = (full_join(ace_money_overall,
  #                                   election_2019) %>% 
  #                            filter(Constituency == "Stratford-on-Avon")),
  #                  aes(label = Constituency)) +
  theme_light() + 
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  facet_zoom(x = npo_money > 40000,
             y = crf_money > 40000)  +
  theme(legend.position = "bottom") +
  scale_colour_manual(values = c("royalblue",
                                 "firebrick")) +
  labs(x = "Total NPO income 2018-2022 per constituency (£000s, log scale)",
       y = "CRF grants per constituency (£000s, log scale)",
       colour = "")


# start putting something together for merge
ace_money_overall <- 
  full_join(crf_money,
          npo_money) %>% 
  mutate(npo_money = 
           replace_na(npo_money, 1)) %>% 
  mutate(crf_money = 
           replace_na(crf_money, 1)) 

ggplot() +
  aes(x = npo_money/1000,
      y = crf_money/1000,
      colour = first_party) +
  geom_point(data = (full_join(ace_money_overall,
                              election_2019) %>% 
                       filter(first_party == "Con" |
                                first_party == "Lab") %>% 
                       na.omit),
             alpha = .3) +
  geom_smooth(data = (full_join(ace_money_overall,
                                election_2019) %>% 
                        filter(first_party == "Con" |
                                 first_party == "Lab") %>% 
                        na.omit %>% 
                        filter(npo_money > 40000 &
                                 crf_money > 40000)),
              method = "lm") + 
  # geom_label_repel(data = (full_join(ace_money_overall,
  #                                   election_2019) %>% 
  #                            filter(Constituency == "Stratford-on-Avon")),
  #                  aes(label = Constituency)) +
  theme_light() + 
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  facet_zoom(x = npo_money > 40000,
             y = crf_money > 40000)  +
  theme(legend.position = "bottom") +
  scale_colour_manual(values = c("royalblue",
                                 "firebrick")) +
  labs(x = "Total NPO income 2018-2022 per constituency (£000s, log scale)",
       y = "CRF grants per constituency (£000s, log scale)",
       colour = "")


crf_money_round1 %>% 
  filter(`Local Authority` == "Sheffield") %>% 
  summarise(money = sum(`£ Awarded`))

crf_money_round1 %>% 
  filter(`Local Authority` == "Stratford-on-Avon") %>% 
  summarise(money = sum(`£ Awarded`))
