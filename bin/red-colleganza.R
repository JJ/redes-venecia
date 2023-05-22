library(igraph)

source("../R/utils.R")

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
