
# wip ---------------------------------------------------------------------


library(dplyr)
library(stringr)
file_name <- 'D:\\Documentos\\Mestrado CI\\Dissertação\\masters-sand-box\\notebooks\\2020-09-21-triplets.csv'


df <- read.csv(file_name,sep = '|',stringsAsFactors = F) %>% 
  as_tibble() %>% 
  select(-X)

for(i in 1:nrow(df)){
  sha <- paste0(
    str_extract_all(
      string = df$head_sha[i],
      pattern = '[:alnum:]',
      simplify = T
    ),
    collapse = ''
  )
  df$head_sha[i] <- sha
  print(paste0('linha: ',i))
}


write.csv(
  x = df,
  file = 'datasets\2020-09-22-triplets.csv',
  row.names = F,
  sep = '|'
)







library('visNetwork') 
nodes <- read.csv("https://github.com/cengel/R-data-viz/raw/master/demo-data/network/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("https://github.com/cengel/R-data-viz/raw/master/demo-data/network/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)
nodes$shape <- "dot"  
nodes$shadow <- TRUE # Nodes will drop shadow
nodes$title <- nodes$media # Text on click
nodes$label <- nodes$type.label # Node label
nodes$size <- nodes$audience.size # Node size
nodes$borderWidth <- 2 # Node border width

nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$media.type]
nodes$color.border <- "black"
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"

visNetwork(nodes, links) %>%
  visOptions(highlightNearest = TRUE, 
             selectedBy = "type.label")



nodes %>% as_tibble()



library(dplyr)


df <- read.csv('dev\\datasets\\2020-09-22-triplets.csv') %>% 
  as_tibble()

df %>%
  group_by(head_sha) %>% 
  summarise(size=n()) %>% 
  mutate(type.label='Paper') %>% 
  rename(id=head_sha) -> documents

df %>%
  select(head_cui) %>% 
  distinct() %>% 
  mutate(type.label='Entity',size=1) %>% 
  rename(id=head_cui) -> head_cui


df %>%
  select(tail_cui) %>% 
  distinct() %>% 
  mutate(type.label='Entity',size=1) %>% 
  rename(id=tail_cui) %>% 
  distinct(id)-> tail_cui






bind_rows(
  documents,
  head_cui,
  tail_cui
) %>% 
  distinct(id) -> nodes



df %>% 
  group_by(head_cui,tail_cui) %>% 
  add_count() %>% 
  ungroup() %>% 
  rename(from=head_cui,to=tail_cui) -> ligacoes





library('visNetwork') 
nodes$size <- 1
nodes$shape <- "dot"  
nodes$shadow <- TRUE # Nodes will drop shadow
nodes$borderWidth <- 2 # Node border width
nodes$color.border <- "black"
nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"

visNetwork(nodes[1:10000,], ligacoes[1:10000,]) %>%
  visOptions(highlightNearest = TRUE, 
             selectedBy = "type.label")


# 2020-09-23 draft --------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(igraph)
library(visNetwork)



df <- read.csv(
  file = 'dev\\datasets\\2020-09-22-triplets.csv',
  stringsAsFactors = F
) %>% 
  as_tibble()


df %>% 
  mutate(
    same_ent=if_else(head_cui==tail_cui,T,F)
  ) %>% 
  filter(same_ent==F) %>% 
  select(-same_ent) -> df_unique



df_unique %>% 
  group_by(head_cui,tail_cui) %>% 
  summarise(weight=n()) %>%
  ungroup() %>% 
  right_join(df_unique,by=c('head_cui','tail_cui')) %>%
  select(head_cui,tail_cui,weight,head_name,tail_name) %>% 
  rename(
    from=head_cui,
    to=tail_cui
  ) %>% 
  distinct() -> edges




df_unique %>% 
  select(head_cui,head_name) %>% 
  rename(id=head_cui,label=head_name) %>%
  bind_rows(
    df_unique %>% 
      select(tail_cui,tail_name) %>% 
      rename(id=tail_cui,label=tail_name)
  ) %>% 
  distinct() -> nodes




edges_filtered <- filter(edges,head_name=='hepatitis B virus') %>%
  arrange(desc(weight)) %>%
  distinct()



nodes_filtered <- nodes %>% 
  filter(id%in%unique(c(edges_filtered$from,edges_filtered$to)))

routes_igraph <- graph_from_data_frame(
  d = edges_filtered,
  vertices = nodes_filtered, 
  directed = F
)

igraph::plot.igraph(
  routes_igraph
)


library(tidygraph)
library(ggraph)

ggraph(routes_igraph, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  #labs(edge_width = "Letters") +
  theme_graph()




# 2020-04-10 --------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(igraph)
library(visNetwork)
library(ggraph)


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

df <- read.csv(
  file = 'dev\\datasets\\2020-09-22-triplets.csv',
  stringsAsFactors = F
) %>% 
  as_tibble()


df %>%
  mutate(
    same_ent=if_else(head_cui==tail_cui,T,F)
  ) %>% 
  filter(same_ent==F) %>% 
  select(-same_ent) -> df_unique

df_unique %>% 
left_join(relations,by=c('head_tui','tail_tui')) %>% 
  drop_na(relation_tui) -> df_unique


df_unique %>% 
  group_by(head_cui,relation_tui,tail_cui) %>% 
  summarise(weight=n()) %>%
  ungroup() %>% 
  right_join(df_unique,by=c('head_cui','relation_tui','tail_cui')) %>%
  select(head_cui,relation_tui,tail_cui,weight,head_name,tail_name,head_sha) %>% 
  rename(
    from=head_cui,
    to=tail_cui,
    relation=relation_tui,
    document=head_sha
  ) %>% 
  distinct() -> edges




df_unique %>% 
  select(head_cui,head_name) %>% 
  rename(id=head_cui,label=head_name) %>%
  bind_rows(
    df_unique %>% 
      select(tail_cui,tail_name) %>% 
      rename(id=tail_cui,label=tail_name)
  ) %>% 
  distinct() -> nodes




edges_filtered <- filter(edges,from%in%c('C0042769',
                                         # 'C0001483',
                                         # 'C4742567',
                                         'C0521095')) %>%
  arrange(desc(weight)) %>%
  distinct()



nodes_filtered <- nodes %>% 
  filter(id%in%unique(c(edges_filtered$from,edges_filtered$to)))
nodes_filtered %>% distinct()


#--
g <- graph_from_data_frame(
  d = edges_filtered,
  vertices = nodes_filtered, 
  directed = T
)

# Explore the set of nodes
V(g)

# Print the number of nodes
vcount(g)

# Explore the set of ties
E(g)



# Give the name "Madrid network" to the network and print the network `name` attribute
g$name <- "Covid- Research Graph"
g$name

# Add node attribute id and print the node `id` attribute
V(g)$id <- seq_len(vcount(g))
V(g)$id

# Print the tie `weight` attribute
E(g)$weight

# Print the network and spot attributes
g


ggraph(g, layout = "with_kk") +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point()  +
  # Add a node text geometry, mapping label to id and repelling
  geom_node_text(aes(label = name), repel = TRUE)


# Visualize the network in a circular layout
ggraph(g, layout = "in_circle") + 
  # Map tie transparency to its weight
  geom_edge_link(aes(alpha = weight)) + 
  geom_node_point()+
  geom_node_text(aes(label = label), repel = TRUE)



# Change the layout so points are on a grid
ggraph(g, layout = "on_grid") + 
  geom_edge_link(aes(alpha = weight)) + 
  geom_node_point()+
  geom_node_text(aes(label = label), repel = TRUE)

# centrality
##
nodes_with_centrality <- nodes_filtered %>%
  mutate(
    degree = degree(g),
    # Add a column containing the strength of each node
    strength = strength(g)
  ) %>%
  # Arrange rows by descending strength
  arrange(desc(strength))

# See the result
nodes_with_centrality



## tie betweness
# Calculate the reciprocal of the tie weights
dist_weight <- 1 / E(g)$weight

ties_with_betweenness <- edges_filtered %>%
  # Add an edge betweenness column weighted by dist_weight
  mutate(betweennesss = edge_betweenness(g, weights = dist_weight))

# Review updated ties
ties_with_betweenness


# From previous step
ties_selected <- ties_with_betweenness %>% 
  left_join(nodes, by = c("from" = "id")) %>% 
  left_join(nodes, by = c("to" = "id")) %>% 
  select(from, to, name_from = label.x, name_to = label.y, betweennesss)

ties_selected %>%
  # Arrange rows by descending betweenness
  arrange(desc(betweenness))



## plot centrality
# Update the previous plot, mapping node size to strength
g2 <- graph_from_data_frame(
  d = ties_with_betweenness,
  vertices = nodes_with_centrality,
  directed = F
)

ggraph(g2, layout = "with_kk") + 
  geom_edge_link(aes(alpha = weight)) + 
  geom_node_point(aes(size = strength))



ggraph(g2, layout = "with_kk") + 
  geom_edge_link(aes(alpha = betweennesss)) +
  # Add a node point geom, mapping size to degree
  geom_node_point(aes(size = degree))



# Calculate the median betweenness
median_betweenness = median(E(g2)$betweennesss)

ggraph(g2, layout = "with_kk") + 
  # Filter ties for betweenness greater than the median
  geom_edge_link(aes(alpha = betweennesss, filter = betweennesss > median_betweenness)) + 
  theme(legend.position="none")



## weak strong ties
tie_counts_by_weight <- ties_with_betweenness %>% 
  # Count the number of rows with each weight
  count(weight) %>%
  # Add a column of the percentage of rows with each weight
  mutate(percentage = 100 * n / nrow(ties_with_betweenness)) 

# See the result
tie_counts_by_weight

# Make is_weak TRUE whenever the tie is weak
is_weak <- E(g)$weight == 1

# Check that the number of weak ties is the same as before
sum(is_weak)

ggraph(g, layout = "with_kk") +
  # Add an edge link geom, mapping color to is_weak
  geom_edge_link(aes(color = is_weak))


# graph -------------------------------------------------------------------


library('visNetwork') 
visNetwork(nodes_filtered, edges_filtered, width="100%", height="400px")


# We'll start by adding new node and edge attributes to our dataframes. 
vis.nodes <- nodes_filtered
vis.links <- edges_filtered

vis.nodes$shape  <- "dot"  
vis.nodes$shadow <- TRUE # Nodes will drop shadow
# vis.nodes$title  <- vis.nodes$ # Text on click
vis.nodes$label  <- vis.nodes$label # Node label
# vis.nodes$size   <- vis.nodes$audience.size # Node size
vis.nodes$borderWidth <- 2 # Node border width


# colocar aqui se é documento ou entidade médica
# vis.nodes$color.background <- c("slategrey", "tomato", "gold")[nodes$media.type]
vis.nodes$color.border <- "black"
vis.nodes$color.highlight.background <- "orange"
vis.nodes$color.highlight.border <- "darkred"

visNetwork(vis.nodes, vis.links)

vis.links$width <- vis.links$weight/8 # line width
vis.links$color <- "gray"    # line color  
vis.links$arrows <- "middle" # arrows: 'from', 'to', or 'middle'
vis.links$smooth <- FALSE    # should the edges be curved?
vis.links$shadow <- FALSE    # edge shadow

visnet <- visNetwork(vis.nodes, vis.links, highlightNearest = TRUE)
visnet

# 2020-10-05 --------------------------------------------------------------


df <- read.csv(
  file = 'dev\\datasets\\2020-09-22-triplets.csv',
  stringsAsFactors = F
) %>% 
  as_tibble()

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
  as_tibble()


df %>% 
  left_join(relations,by=c('head_tui','tail_tui')) %>% 
  drop_na(relation_tui) %>% 
  left_join(definitons,by=c('relation_tui'))
  
