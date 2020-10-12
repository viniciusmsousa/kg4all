library(dplyr)
library(tidyr)

# import data -------------------------------------------------------------
triplets <- read.csv(
  file = 'dev\\datasets\\2020-09-22-triplets.csv',
  stringsAsFactors = F
) %>% 
  as_tibble() %>%
  mutate(
    same_ent=if_else(head_cui==tail_cui,T,F)
  ) %>% 
  filter(same_ent==F) %>% 
  select(-same_ent) 


definitons <- read.csv(
  file = 'dev\\datasets\\SRDEF_definitions.csv',
  stringsAsFactors = F,
  header = F,
  sep = '|'
) %>% 
  as_tibble()

relations <- read.csv(
  file = 'dev\\datasets\\SRSTRE1_relations.csv',
  stringsAsFactors = F,
  header = F,
  sep = '|',
  col.names = c('head_tui','relation_tui','tail_tui','n')
) %>% 
  as_tibble() %>% 
  select(-n)

metadata <- read.csv(
  file = 'dev\\datasets\\metadata.csv',
  stringsAsFactors = F,
  header = T,
  sep = ','
) %>% 
  as_tibble() %>% 
  select(sha,title, publish_time)

# transform data ----------------------------------------------------------
triplets %>% 
  left_join(relations,by=c('head_tui','tail_tui')) %>% 
  drop_na(relation_tui) %>% 
  left_join(select(definitons,V2,V3),by=c('head_tui'='V2')) %>% 
  rename(head_type=V3) %>% 
  left_join(select(definitons,V2,V3),by=c('tail_tui'='V2')) %>%
  rename(tail_type=V3) %>% 
  left_join(select(definitons,V2,V3),by=c('relation_tui'='V2')) %>%
  rename(relation_type=V3) %>% 
  select(
    head_sha,contains('head'),contains('relation'), contains('tail')
  ) %>% 
  group_by(head_cui,relation_tui,tail_cui) %>% 
  add_count() %>% 
  ungroup() %>% 
  rename(relation_count=n) %>% 
  left_join(metadata,by=c('head_sha'='sha')) %>% 
  select(head_sha,title, publish_time,contains('head'),contains('relation'), contains('tail')) %>% 
  select(-contains('tui')) -> kg



# saving processed --------------------------------------------------------
usethis::use_data(kg)





