
# session setup -----------------------------------------------------------


library(dplyr)
library(tidyr)
library(ggplot2)
library(igraph)
library(visNetwork)
library(ggraph)


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



# adding relation tui to triplets -----------------------------------------


## plots
triplets %>% 
  left_join(relations,by=c('head_tui','tail_tui')) %>% 
  drop_na(relation_tui) %>% 
  left_join(select(definitons,V2,V3),by=c('head_tui'='V2')) %>% 
  rename(head_type=V3) %>% 
  left_join(select(definitons,V2,V3),by=c('tail_tui'='V2')) %>%
  rename(tail_type=V3) %>% 
  left_join(select(definitons,V2,V3),by=c('relation_tui'='V2')) %>%
  rename(relation_type=V3) -> kg

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
  rename(relation_count=n) -> kg2

View(kg2)
# nodes data frame --------------------------------------------------------

kg %>% 
  select(
    head_cui,head_name, head_type
  ) %>%
  rename(
    cui=head_cui,
    name=head_name,
    type=head_type
  ) %>% 
  bind_rows(
    kg %>% 
      select(tail_cui,tail_name,tail_type) %>% 
      rename(
        cui=tail_cui,
        name=tail_name,
        type=tail_type
      )
  ) %>% 
    distinct() %>%
    rename(
      id=cui
    ) %>% 
    # mutate(
    #   id=row_number()
    # ) %>% 
    select(id,name,type) -> nodes

# edges dataframe ---------------------------------------------------------

  
kg %>% 
  select(head_cui,tail_cui,relation_type) %>% 
  count(head_cui,tail_cui,relation_type) %>% 
  arrange(desc(n)) %>%
  distinct() %>% 
  rename(
    from=head_cui,
    to=tail_cui,
    type=relation_type,
    count=n
  ) -> edges




# subset graph ------------------------------------------------------------
edges %>% 
  count(from)

(
  edges %>% 
    filter(from%in%c(
      # 'C0001175',
      'C0001311'
    )) %>%
    mutate(count=count/100) %>% 
    rename(value=count,label=type)-> arestas
)


(
  nos <- nodes %>% 
    filter(id%in%c(unique(c(arestas$from,arestas$to)))) %>% 
    rename(group=type,label=name)
)


visNetwork(
  nos, arestas
) %>% 
  visGroups(groupname = "Disease or Syndrome", color = "darkblue", shape = "square", 
            shadow = list(enabled = TRUE)) %>% 
  visLegend() %>% 
  visInteraction(dragNodes = FALSE, 
                 dragView = FALSE, 
                 zoomView = FALSE)


visNetwork(nos, arestas, height = "500px") %>%
  visIgraphLayout(layout = "layout_in_circle") %>%
  visNodes(size = 10) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), 
             nodesIdSelection = T)



visNetwork(nos, arestas) %>% 
  visEdges(arrows = "from") %>% 
  # visHierarchicalLayout(direction = "LR", levelSeparation = 500) %>% 
  visOptions(highlightNearest = list(enabled = T, hover = T), 
             nodesIdSelection = T) %>% 
  visPhysics(stabilization = FALSE)
  # Se dois nós estão ligados 
# Menor caminho entre dois nós




# dimensional -------------------------------------------------------------
library(threejs)
library(htmlwidgets)
net <- graph_from_data_frame(d = edges, vertices = nodes, directed = T)

V(net.js)

net.js <- net
graph_attr(net.js, "layout") <- NULL 


gjs.an <- graphjs(net.js, bg="gray10", showLabels=T, stroke=F, 
                  layout=list(layout_randomly(net.js, dim=3),
                              layout_with_fr(net.js,  dim=3),
                              layout_with_drl(net.js, dim=3),  
                              layout_on_sphere(net.js)))
print(gjs.an)


saveWidget(gjs.an, file="Media-Network-gjs-an.html")
browseURL("Media-Network-gjs-an.html")



# nenetworkD3 -------------------------------------------------------------

library(networkD3)



# Load data
data(MisLinks)
data(MisNodes)

# Plot
forceNetwork(Links = as.data.frame(arestas), Nodes = as.data.frame(nos %>% rename(a=label)),
             Source = "from", Target = "to",
             Value = "value", NodeID = "a",
             Group = "group", opacity = 0.8)

arestas %>% 
  rename(
    source=from,
    target=to
  ) %>% 
  select(-label) -> MisLinks


colnames(MisNodes)
nos %>% 
  rename(name=label) %>% 
  select(-id) -> MisNodes


forceNetwork(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8)

# rascunho ----------------------------------------------------------------



edges %>% 
  filter(to=='C0000936')
