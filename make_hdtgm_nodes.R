# make_hdtgm_nodes.R


# Author: J. Peralta
# October 3, 2023


# IMDB data downloaded October 2, 2023: https://datasets.imdbws.com/
# Movies covered on pod: https://www.imdb.com/list/ls099196868/?sort=list_order,asc&st_dt=&mode=detail&page=1

# This network is current up to Episode 329: Bats LIVE! but only includes movies that share at least one principal with another movie

# =================== 0 Packages ========================================

# R version 4.2.2

library(tidyverse) # v2.0.0
library(igraph) # v1.5.1
library(visNetwork) # v2.1.2
library(htmlwidgets) # v1.6.2

# =================== 1 Load data ======================================

# All HDTGM movies
nodes = read_csv("./data/hdtgm_movies.csv", col_names = TRUE, show_col_types = FALSE) # Node list
ref.all = nodes$id

# Import IMDB db
imdb = read_tsv("./data/title.principals.trim.tsv", col_names = TRUE, show_col_types = FALSE)
imdb_small = subset(imdb[, 1:2], id %in% ref.all) # Subset to movies that have been on the pod
pairs = combn(ref.all, 2)
rownames(pairs) = c("to", "from")

i = 1
width = vector()
for(i in 1:ncol(pairs)){ # Get edge values
  to = pairs[1, i]
  from = pairs[2, i]
  x = length(intersect(imdb_small$n[imdb_small$id == to], imdb_small$n[imdb_small$id == from]))
  width = append(width, x)
  i = i+1
}
edges.df = rbind(pairs, width)
edges.df = as.data.frame(t(edges.df)) # Note: Still the IMDB id's

get_decade = function(y){ # 'Bin' the movies by decade
  return(y - y %% 10)
}
decade = lapply(nodes$year, get_decade)
decade = unlist(decade)
decade = paste(decade, "s", sep="")
newtitle = paste(nodes$title, " (", nodes$year, ")", sep = "")
nodes.df = as.data.frame(cbind(nodes$id, newtitle, decade, nodes$rating))
colnames(nodes.df) = c("id", "label", "group", "value")

# =================== 2 Format data for connected movies ==============

edges.some = edges.df[edges.df$width != 0, ] # Remove isolated movies from edges
edges.some = edges.some[order(edges.some$to), ]
rownames(edges.some) = NULL

ref.some = sort(unique(append(edges.some$to, edges.some$from))) # Named vector to get compatible id's
newnames = as.character(seq(0, length(ref.some)-1))
names(ref.some) = newnames

nodes.some = filter(nodes.df, id %in% ref.some) # Remove isolated movies from nodes
nodes.some$id = names(ref.some)[match(nodes.some$id, ref.some)] # Get nodes df for plotting
nodes.some = nodes.some[order(nodes.some$id), ]
rownames(nodes.some) = NULL
nodes.some = nodes.some %>% mutate(id = as.numeric(id), value = as.numeric(value))
nodes.some$value = nodes.some$value^1.4

edges.some$to = names(ref.some)[match(edges.some$to, ref.some)]
edges.some$from = names(ref.some)[match(edges.some$from, ref.some)]
edges.some = edges.some %>% mutate(to = as.numeric(to), from = as.numeric(from), width = as.numeric(width))

# =================== 3 Plotting network =============================

plot = visNetwork(nodes.some, edges.some, width = "100%", height = "1080px") %>%
  visEdges(color = list(color = "#bebebe", highlight = "#6c6c6c")) %>%
  visNodes(shape = "dot", font = list(size = 16)) %>%
  visGroups(groupname = "1970s", color = "#FFFFD9") %>%
  visGroups(groupname = "1980s", color = "#D6EFB2") %>%
  visGroups(groupname = "1990s", color = "#72C8BC") %>%
  visGroups(groupname = "2000s", color = "#2498C0") %>%
  visGroups(groupname = "2010s", color = "#234DA0") %>%
  visGroups(groupname = "2020s", color = "#081D58") %>%
  visLayout(randomSeed = 222) %>%
  visOptions(highlightNearest = list(enabled = T, hover = T), nodesIdSelection = T) %>%
  visPhysics(stabilization = FALSE)

saveNetwork(plot, "hdtgm_nodes.html")
