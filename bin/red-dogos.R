library(dogesr)
library(igraph)

doges.sn <- marriage.graph()
E(doges.sn)$weight <- 1
doges.sn <- simplify(doges.sn, edge.attr.comb=list(weight="sum"))
V(doges.sn)$degree <- degree(doges.sn)
V(doges.sn)$EV <- unname(eigen_centrality(doges.sn)$vector)
components <- igraph::clusters(doges.sn, mode="weak")
biggest_cluster_id <- which.max(components$csize)
vert_ids <- V(doges.sn)[components$membership == biggest_cluster_id]

doges.sn.connected <- igraph::induced_subgraph(doges.sn, vert_ids)

doges.layout <- layout_with_fr(doges.sn.connected, niter=1000)
plot(doges.sn.connected,
     vertex.size=V(doges.sn.connected)$degree,
     layout=doges.layout,
     vertex.label.cex=1+V(doges.sn.connected)$EV*3,
     vertex.label.color=rgb(0.7,0,0,0.6),
     vertex.label.dist=1,
     edge.width=3*E(doges.sn.connected)$weight)

main_ids <- V(doges.sn.connected)[degree(doges.sn.connected) > 1 ]
doges.main.sn <- igraph::induced_subgraph(doges.sn.connected, main_ids)

for (i in 1:3) {
  main_ids <- V(doges.main.sn)[degree(doges.main.sn) > 1 ]
  doges.main.sn <- igraph::induced_subgraph(doges.main.sn, main_ids)
}

plot(doges.main.sn,
     vertex.size=V(doges.main.sn)$degree,
     layout=layout_nicely,
     vertex.label.cex=V(doges.main.sn)$EV*2,
     vertex.label.dist=1,
     edge.width=3*E(doges.main.sn)$weight)
