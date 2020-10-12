### loading packages
library(tidyverse)
library(readxl)
library(scales)

# loading data
election_2019 <- 
  read_csv("HoC-GE2019-results-by-constituency-csv.csv")
crf_money <- 
  read_excel("CRF_Round_one_under_1_Million_awards_offered_121020.xlsx",
             2)

# filter election to just england
election_2019 <- 
  election_2019 %>% 
  filter(country_name == "England")

# renaming column for merge
election_2019$Constituency <- 
  election_2019$constituency_name

# joining
election_crf <- 
  full_join(election_2019,
             crf_money,
             by = "Constituency")

# get missing cases where the id isn't consistent
election_crf %>% 
  filter(is.na(other_winner)) %>% 
  select(Constituency) %>% 
  table()

# mostly upper/lower case issues
# though will need to cut Edinburgh East & Wrexham

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
election_2019$Constituency[election_2019$constituency_name == 
                             "Weston-Super-Mare"] <- 
  "Weston-super-Mare"

# attempt join again

election_crf <- 
  full_join(election_2019,
            crf_money,
            by = "Constituency") %>% 
  filter(Constituency != "Edinburgh East" &
           Constituency != "Wrexham")

# look at total money by constituency
money_per_constituency <- 
  election_crf %>% 
  group_by(Constituency) %>% 
  mutate(award_value = 
           replace_na(`£ Award offered`, 0)) %>% 
  mutate(money_per_constituency = sum(award_value)) %>% 
  select(money_per_constituency,
         first_party,
         region_name,
         Constituency) %>% 
  distinct()

# where's the money gone?
money_per_constituency %>%
  ggplot()  +
  aes(x = money_per_constituency,
      y = fct_reorder(Constituency, 
                     money_per_constituency))  +
  geom_col(aes(fill = first_party),
           width = .75) +
  geom_text(aes(label = comma(money_per_constituency)),
            hjust = 0,
            size = 2,
            position = position_nudge(x = 100000)) +
  geom_text(aes(label = Constituency,
                colour = first_party,
                x = 0),
            hjust = 1,
            size = 2,
            position = position_nudge(x = -100000)) +
  theme_void() +
  labs(x = "",
       y = "") +
  theme(legend.position = "none") +
  theme(panel.grid = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  scale_fill_manual(values = c("darkblue",
                               "green4",
                               "firebrick",
                               "orange",
                               "black")) +
  scale_colour_manual(values = c("darkblue",
                               "green4",
                               "firebrick",
                               "orange",
                               "black")) +
  scale_x_continuous(limits = c(-3000000,
                                15000000))
ggsave("money_per_constituency.png",
       width = 8,
       height = 60,
       limitsize = FALSE)
  
