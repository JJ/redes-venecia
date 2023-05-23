library(igraph)

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
