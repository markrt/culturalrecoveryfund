

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
library(DT)
library(kableExtra)


# crf money: from the ace site
crf_money_round1 <- 
  read_excel("CRF investment data published 020421.xlsx", 
             2,
             .name_repair = "universal") %>% 
  select(-Strand) %>% 
  mutate(source = "ACE grants round 1")
crf_money_capitalkickstart <- 
  read_excel("CRF investment data published 020421.xlsx", 
             3,
             .name_repair = "universal") %>% 
  mutate(source = "ACE capital kickstart")
crf_money_round2 <- 
  read_excel("CRF investment data published 020421.xlsx", 
             4,
             .name_repair = "universal") %>% 
  mutate(source = "ACE grants round 2")

# crf money: from dcms
# https://www.gov.uk/government/news/400-million-to-help-more-than-2700-arts-culture-heritage-organisations-and-independent-cinemas-survive-and-thrive
# sheet 1 seems to be the same, so steering clear for the moment
# bfi: weird missingness, trying to deal by replacing NAs with "per cinema"
bfi_money <- 
  read_excel("CRF_2-Awards_read-only.xlsx", 
             2, 
             skip = 1,
             na = "-",
             .name_repair = "universal") %>% 
  mutate(Award.... = 
           coalesce(Award...., 
                    Award.per.Cinema....))
# nlhf: looks ok
nlhf_money <- 
  read_excel("CRF_2-Awards_read-only.xlsx", 3, 
             skip = 1,
             .name_repair = "universal")

# repayable finance from dcms - seems fine
repayable_finance <- 
  read_excel("CRF_2-Awards_read-only.xlsx", 4, 
             skip = 1,
             .name_repair = "universal") %>% 
  mutate(Local.authority = fct_recode(Local.authority,
                                      "York" = 
                                        "City of York"))


# population data from 2019
# has updated local authority codes
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
population <- 
  read_excel("ukmidyearestimates20192020ladcodes.xls", 
             6,
             skip = 4,
             .name_repair = "universal") %>% 
  select(Code:All.ages) %>% 
  rename(la_name = 
           Name,
         population = 
           All.ages) %>% 
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


npo_data <- 
  read_excel("NPO_2018_12082020_0.xlsx") %>% 
  mutate(Organisation = 
           `Applicant Name`)

npo_names <- 
  npo_data %>% 
  select(Organisation) %>% 
  pull()

npo_names

# add flag to crf money
# of whether it's an npo or not

crf_money_capitalkickstart <- 
  crf_money_capitalkickstart %>% 
  mutate(is_npo = 
           if_else(Organisation %in% npo_names,
                   "NPO",
                   "Not NPO")) 

crf_money_round1 <- 
  crf_money_round1 %>% 
  mutate(is_npo = 
           if_else(Organisation %in% npo_names,
                   "NPO",
                   "Not NPO")) 
crf_money_round2 <- 
  crf_money_round2 %>% 
  mutate(is_npo = 
           if_else(Organisation %in% npo_names,
                   "NPO",
                   "Not NPO")) 


all_money_sources <- 
  crf_money %>% 
  group_by(Local.Authority,
           source) %>% 
  summarise(money = 
              sum(..Awarded)) %>% 
  pivot_wider(names_from = "source",
              values_from = "money") %>% 
  full_join(bfi_money %>% 
              rename(Local.Authority = 
                       Local.authority) %>% 
              group_by(Local.Authority) %>% 
              summarise(BFI  = 
                          sum(Award....))) %>% 
  full_join(nlhf_money %>% 
              rename(Local.Authority = 
                       Local.authority) %>% 
              group_by(Local.Authority) %>% 
              summarise(NLHF  = 
                          sum(Award....))) %>% 
  pivot_longer(-Local.Authority) %>% 
  mutate(value = replace_na(value, 0)) %>% 
  rename(Source = name,
         Money = value)

# combine with population data
all_money_population <- 
  all_money_sources %>% 
  full_join((population %>% 
               rename(Local.Authority = 
                        la_name))) %>% 
  mutate(money_per_head = 
           round((Money / 
                    population), 
                 2))

# add active lives data
active_lives <- 
  read_csv("active_lives.csv")

# add constituency type data
hoc <- 
  read_csv("pcon-classification-csv.csv")

# single row for each constituency
# just England
hoc_use <- 
  hoc %>% 
  group_by(constituency_name) %>% 
  mutate(population = sum(population)) %>% 
  top_n(1, percent_of_constituency) %>% 
  filter(str_sub(constituency_code,
                 1, 1) == "E")


crf_constituency <- 
  crf_money %>% 
  group_by(Constituency) %>% 
  summarise(total_money = sum(..Awarded)) %>% 
  rename(constituency_name = 
           Constituency)

money_active_lives <- 
  (all_money_population %>% 
  group_by(Local.Authority) %>% 
  summarise(total_money_per_head = 
              sum(money_per_head)) %>% 
  full_join(active_lives %>% 
               rename(Local.Authority = 
                        LA_name) %>% 
              mutate(Local.Authority = 
                       fct_recode(Local.Authority,
                                  "King's Lynn and West Norfolk" = 
                                    "Kings Lynn and West Norfolk"))) %>% 
  filter(Local.Authority != "City of London") %>% 
  ggplot() +
  aes(x = percent_culture,
      y = total_money_per_head,
      text = paste(Local.Authority, 
                   ": \n£", 
                   total_money_per_head, 
                   "per head \n ",
                   round(percent_culture*100),
                   "% adults culturally active \n (Active Lives)")) +
  geom_point() +
  scale_y_log10() +
    theme_minimal() +
    scale_x_continuous(labels = percent) +
    labs(x = "% attending or participating in culture (Active Lives)",
         y = "Total CRF £ per head (all sources)"))

ggplotly(doodad,
         tooltip = "text")

hoc_use <- 
  hoc %>% 
  group_by(constituency_name) %>% 
  mutate(population = sum(population)) %>% 
  top_n(1, percent_of_constituency) %>% 
  filter(str_sub(constituency_code,
                 1, 1) == "E")

crf_constituency <- 
  crf_money %>% 
  group_by(Constituency) %>% 
  summarise(total_money = sum(..Awarded)) %>% 
  rename(constituency_name = 
           Constituency)


hoc_use %>% 
  full_join(crf_constituency) %>% 
  mutate(total_money = replace_na(total_money, 0)) %>% 
  group_by(classification) %>% 
  summarise(total_population = sum(population),
            total_money = sum(total_money)) %>% 
  mutate(money_per_head = 
           total_money / 
           total_population) %>% 
  na.omit %>% 
  mutate(classification = fct_relevel(classification,
                                      "Core City (London)",
                                      "Core City (outside London)",
                                      "Other City",
                                      "Large Town",
                                      "Medium Town",
                                      "Small Town",
                                      "Village or smaller")) %>% 
  ggplot() +
  aes(x = money_per_head,
      y = fct_rev(classification),
      label = paste0("£", round(money_per_head, 2))) +
  geom_col() +
  geom_text(hjust = 0,
            nudge_x = .5) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank()) +
  labs(x = "",
       y = "")



crf_constituency_source <- 
  crf_money %>% 
  group_by(source, Constituency) %>% 
  summarise(total_money = sum(..Awarded)) %>% 
  rename(constituency_name = 
           Constituency)

# ugly
hoc_use %>% 
  full_join((crf_money_round1 %>% 
               rename(constituency_name = 
                        Constituency))) %>% 
  mutate(..Awarded = 
               replace_na(..Awarded, 0)) %>% 
  group_by(classification,
           source) %>% 
  summarise(total_money = sum(..Awarded)) %>% 
  na.omit %>% 
  bind_rows(hoc_use %>% 
              full_join((crf_money_round2 %>% 
                           rename(constituency_name = 
                                    Constituency))) %>% 
              mutate(..Awarded = 
                       replace_na(..Awarded, 0)) %>% 
              group_by(classification,
                       source) %>% 
              summarise(total_money = sum(..Awarded)) %>% 
              na.omit) %>% 
  bind_rows(hoc_use %>% 
              full_join((crf_money_capitalkickstart %>% 
                           rename(constituency_name = 
                                    Constituency))) %>% 
              mutate(..Awarded = 
                       replace_na(..Awarded, 0)) %>% 
              group_by(classification,
                       source) %>% 
              summarise(total_money = sum(..Awarded)) %>% 
              na.omit) %>% 
  full_join(hoc_use %>% 
  group_by(classification) %>% 
  summarise(population = sum(population))) %>% 
  mutate(money_per_head = 
           total_money / 
           population) %>% 
  mutate(source = fct_relevel(source,
                              "ACE grants round 1",
                              "ACE grants round 2",
                              "ACE capital kickstart")) %>% 
  ggplot() +
  aes(x = money_per_head,
      y = fct_rev(fct_relevel(classification,
                      "Core City (London)",
                      "Core City (outside London)",
                      "Other City",
                      "Large Town",
                      "Medium Town",
                      "Small Town",
                      "Village or smaller")),
      label = paste0("£", round(money_per_head, 2))) +
  geom_col() +
  geom_text(hjust = 0,
            nudge_x = .5) +
  facet_wrap(~ source) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank()) +
  theme(panel.spacing = unit(1, "lines")) +
  labs(x = "",
       y = "") + coord_cartesian(clip = "off")





npo_data <- 
  read_excel("NPO_2018_12082020_0.xlsx") %>% 
  mutate(Organisation = 
           `Applicant Name`)

npo_names <- 
  npo_data %>% 
  select(Organisation) %>% 
  pull()

# add flag to crf money
# of whether it's an npo or not

crf_money_capitalkickstart <- 
  crf_money_capitalkickstart %>% 
  mutate(is_npo = 
           if_else(Organisation %in% npo_names,
                   "NPO",
                   "Not NPO")) 

crf_money_round1 <- 
  crf_money_round1 %>% 
  mutate(is_npo = 
           if_else(Organisation %in% npo_names,
                   "NPO",
                   "Not NPO")) 
crf_money_round2 <- 
  crf_money_round2 %>% 
  mutate(is_npo = 
           if_else(Organisation %in% npo_names,
                   "NPO",
                   "Not NPO")) 




hoc_use %>% 
  full_join((crf_money_round1 %>% 
               rename(constituency_name = 
                        Constituency))) %>% 
  mutate(..Awarded = 
           replace_na(..Awarded, 0)) %>% 
  group_by(classification,
           source,
           is_npo) %>% 
  summarise(total_money = sum(..Awarded)) %>% 
  na.omit %>% 
  bind_rows(hoc_use %>% 
              full_join((crf_money_round2 %>% 
                           rename(constituency_name = 
                                    Constituency))) %>% 
              mutate(..Awarded = 
                       replace_na(..Awarded, 0)) %>% 
              group_by(classification,
                       source,
                       is_npo) %>% 
              summarise(total_money = sum(..Awarded)) %>% 
              na.omit) %>% 
  bind_rows(hoc_use %>% 
              full_join((crf_money_capitalkickstart %>% 
                           rename(constituency_name = 
                                    Constituency))) %>% 
              mutate(..Awarded = 
                       replace_na(..Awarded, 0)) %>% 
              group_by(classification,
                       source,
                       is_npo) %>% 
              summarise(total_money = sum(..Awarded)) %>% 
              na.omit) %>% 
  full_join(hoc_use %>% 
              group_by(classification) %>% 
              summarise(population = sum(population))) %>% 
  mutate(money_per_head = 
           total_money / 
           population) %>% 
  mutate(source = fct_relevel(source,
                              "ACE grants round 1",
                              "ACE grants round 2",
                              "ACE capital kickstart")) %>% 
  ggplot() +
  aes(x = money_per_head,
      y = fct_rev(fct_relevel(classification,
                              "Core City (London)",
                              "Core City (outside London)",
                              "Other City",
                              "Large Town",
                              "Medium Town",
                              "Small Town",
                              "Village or smaller")),
      label = paste0("£", round(money_per_head, 2))) +
  geom_col() +
  geom_text(hjust = 0,
            nudge_x = .5) +
  facet_grid(is_npo ~ source) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank()) +
  theme(panel.spacing = unit(1, "lines")) +
  labs(x = "",
       y = "") + 
  coord_cartesian(clip = "off")

hoc_use %>% 
  tabyl(classification)

hoc_use %>% 
  group_by(classification) %>% 
  summarise(population = sum(population))

21000000/4457655

crf_money_capitalkickstart %>% 
  filter(is_npo == "Not NPO") %>% 
  select(Organisation, ..Awarded) %>% 
  arrange(-..Awarded)

crf_money_round1%>% 
  filter(is_npo == "Not NPO") %>% 
  select(Organisation, ..Awarded) %>% 
  arrange(-..Awarded)

npo_names
