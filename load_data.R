
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

# load shapefile
local_authorities <- 
  read_sf(dsn = "shapefiles/Local_Authority_Districts_(December_2020)_UK_BUC", 
          layer = "Local_Authority_Districts_(December_2020)_UK_BUC")

# ok, need a data frame with a column for all sources of money
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

# add region variable
all_money_sources <- 
  crf_money %>% 
  select(Local.Authority,
         Region.ONS) %>% 
  full_join(all_money_sources)

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

# combine with shapefile, limit to england
money_population_geography <- 
  full_join(all_money_population, 
            (local_authorities %>% 
               rename(Code = 
                        LAD20CD)) )%>% 
  mutate(country = substr(Code, 
                          1,
                          1)) %>% 
  filter(country == "E") %>% 
  select(-country)
