library(igraph)
library(qgraph)
library(dogesr)
library(dplyr)

colleganza <- read.csv("../data/colleganza-pairs.csv", header=F)
colleganza.sn <- graph.data.frame(data.frame(colleganza$V1,colleganza$V2),directed=F)
E(colleganza.sn)$weight <- 1
colleganza.sn <- simplify(colleganza.sn, edge.attr.comb=list(weight="sum"))
V(colleganza.sn)$degree <- degree(colleganza.sn)
V(colleganza.sn)$betweenness <- betweenness(colleganza.sn)

components <- igraph::clusters(colleganza.sn, mode="weak")
biggest_cluster_id <- which.max(components$csize)
vert_ids <- V(colleganza.sn)[components$membership == biggest_cluster_id]

colleganza.sn.connected <- igraph::induced_subgraph(colleganza.sn, vert_ids)

# Traza para a4
rojo.dogo <- rgb(0.7,0,0,0.8)
V(colleganza.sn.connected)$color=rgb(0,0,1,0.5)

doge.families <- data.doges %>% count(Family.doge,sort=T)
V(colleganza.sn.connected)[ V(colleganza.sn.connected)$name %in% doge.families$Family.doge ]$color <- rojo.dogo
edges <- get.edgelist(colleganza.sn.connected,names=FALSE)
colleganza.layout  <- qgraph.layout.fruchtermanreingold(edges,vcount=vcount(colleganza.sn.connected))
plot(colleganza.sn.connected,
     vertex.size=V(colleganza.sn.connected)$degree/2,
     layout=colleganza.layout,
     vertex.label.cex=1+V(colleganza.sn.connected)$betweenness/2000,
     vertex.label.dist=0.5,
     vertex.label.color=V(colleganza.sn.connected)$color,
     edge.width=3*E(colleganza.sn.connected)$weight)

# Elimina colgantes
main_ids <- V(colleganza.sn.connected)[degree(colleganza.sn.connected) > 1 ]
colleganza.main.sn <- igraph::induced_subgraph(colleganza.sn.connected, main_ids)

for (i in 1:3) {
  main_ids <- V(colleganza.main.sn)[degree(colleganza.main.sn) > 1 ]
  colleganza.main.sn <- igraph::induced_subgraph(colleganza.main.sn, main_ids)
}

plot(colleganza.main.sn,
     vertex.size=V(colleganza.main.sn)$degree/2,
     layout=layout_with_fr,
     vertex.label.cex=V(colleganza.main.sn)$betweenness/400,
     vertex.label.dist=1,
     edge.width=3*E(colleganza.main.sn)$weight)
