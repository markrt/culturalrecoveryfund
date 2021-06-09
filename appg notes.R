# run the chunks in initial_sweep_for_comments.Rmd first


nlhf_money %>% 
  bind_rows(bfi_money) %>% 
  bind_rows(crf_money) %>% 
  mutate(north = if_else((Region.ONS == "North East" |
                            Region.ONS == "North West" |
                            Region.ONS == "Yorkshire and The Humber"),
                         "The North",
                         "Not The North")) %>% 
  
  group_by(north) %>% 
  summarise(Money = sum(Money)) %>% 
  mutate(proportion = Money / sum(Money)) 
# total: 26.6% in the North

# just the ACE stuff
bind_rows(crf_money) %>% 
  mutate(north = if_else((Region.ONS == "North East" |
                            Region.ONS == "North West" |
                            Region.ONS == "Yorkshire and The Humber"),
                         "The North",
                         "Not The North")) %>% 
  
  group_by(north) %>% 
  summarise(Money = sum(Money)) %>% 
  mutate(proportion = Money / sum(Money)) 
# 26.8% of all ACE stuff

# not including capital kickstart
crf_money_round1 %>% 
  bind_rows(crf_money_round2) %>%
  mutate(north = if_else((Region.ONS == "North East" |
                            Region.ONS == "North West" |
                            Region.ONS == "Yorkshire and The Humber"),
                         "The North",
                         "Not The North")) %>% 
  
  group_by(north) %>% 
  summarise(Money = sum(..Awarded)) %>% 
  mutate(proportion = Money / sum(Money)) 
# 24.3% The North

# just round one
crf_money_round1 %>%
  mutate(north = if_else((Region.ONS == "North East" |
                            Region.ONS == "North West" |
                            Region.ONS == "Yorkshire and The Humber"),
                         "The North",
                         "Not The North")) %>% 
  
  group_by(north) %>% 
  summarise(Money = sum(..Awarded)) %>% 
  mutate(proportion = Money / sum(Money)) 

# just capital kickstart

crf_money_capitalkickstart %>%
  mutate(north = if_else((Region.ONS == "North East" |
                            Region.ONS == "North West" |
                            Region.ONS == "Yorkshire and The Humber"),
                         "The North",
                         "Not The North")) %>% 
  
  group_by(north) %>% 
  summarise(Money = sum(..Awarded)) %>% 
  mutate(proportion = Money / sum(Money)) 
