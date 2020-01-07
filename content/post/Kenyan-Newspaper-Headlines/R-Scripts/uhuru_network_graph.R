uhuru_entities1 = uhuru_entities
uhuru_entities1 = uhuru_entities1[year(uhuru_entities1$date) == 2016,]
uhuru_entities1$entities = gsub(", ", ",",  uhuru_entities1$entities)
uhuru_entities1$entities = gsub(" ", "_",  uhuru_entities1$entities)
uhuru_entities1$entities = gsub(",", " ",  uhuru_entities1$entities)

corpus = VCorpus(VectorSource(uhuru_entities1$entities))
corpus = tm_map(corpus, content_transformer(tolower))
tdm = TermDocumentMatrix(corpus, control = list(removePunctuation = FALSE, stopwords = FALSE))
entities_matrix = as.matrix(tdm)
entities_matrix[entities_matrix >= 1] = 1
adjacency_matrix = entities_matrix %*% t(entities_matrix)
graph_object = graph.adjacency(adjacency_matrix, weighted = TRUE, mode = "undirect")
graph_object = simplify(graph_object)
bipartite_object = get.data.frame(graph_object) 
edge_list = bipartite_object %>% select(to, from)
nodes = data.frame(table(unlist(strsplit(uhuru_entities1$entities, " ")))) %>%
  rowid_to_column("id")
colnames(nodes) = c("id", "label", "weight")

edges = bipartite_object 
edges = edges %>%
  left_join(nodes %>% select(-weight), by = c("from" = "label")) %>%
  select(-from) %>%
  rename("from" = id) %>%
  left_join(nodes %>% select(-weight), by = c("to" = "label")) %>%
  select(-to) %>%
  rename("to" = id)

nodes_d3 = nodes %>%
  mutate(id = id - 1)

edges_d3 = edges %>%
  mutate(from = from - 1, to = to - 1)

forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
             NodeID = "label", Nodesize = "weight", Group = "id", Value = "weight", 
             opacity = 1, fontSize = 16, zoom = TRUE)

graph_object = graph.data.frame(bipartite_object)
V(graph_object)$label = V(graph_object)$name
V(graph_object)$degree = degree(graph_object)
yearly_entities$to = gsub(" ", "_",yearly_entities$to)
nodes_data = yearly_entities %>%
  filter(to %in% V(graph_object)$label) %>%
  arrange(to)
tidy_graph_object = as_tbl_graph(graph_object) 
tidy_graph_object = tidy_graph_object %>%
  arrange(label) %>%
  filter(V(graph_object)$label %in% nodes_data$to)
V(tidy_graph_object)$counts = nodes_data$number_appearances
g = ggraph(tidy_graph_object, layout = "fr") + geom_edge_link(col = "blue") +
  #geom_node_point() +
  geom_node_point(aes(size = V(tidy_graph_object)$counts), col = "red") + theme_graph() +
  geom_node_text(aes(label = label), repel = TRUE) + scale_color_continuous(low = "#132B43", high = "#FF0000") +
  scale_size(range = c(0.5, 15)) + theme(legend.position = "none")