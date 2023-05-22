library(dogesr)
library(igraph)
doges.sn <- marriage.graph()
E(doges.sn)$weight <- 1
doges.sn <- simplify(doges.sn, edge.attr.comb=list(weight="sum"))
V(doges.sn)$degree <- degree(doges.sn)
V(doges.sn)$EV <- unname(eigen_centrality(doges.sn)$vector) # unname extrae solo el vector de valores; los pesos se usan implícitamente
components <- igraph::clusters(doges.sn, mode="weak")
biggest_cluster_id <- which.max(components$csize)
vert_ids <- V(doges.sn)[components$membership == biggest_cluster_id]

doges.sn.connected <- igraph::induced_subgraph(doges.sn, vert_ids)
