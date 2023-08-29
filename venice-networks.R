## ----setup, echo=F, message=F----------------------------------------------------------------------------
source("R/utils.R") # Creates doges.sn, doges.sn.connected
library(ggplot2)
library(igraph)
## ----colleganza,  fig.pos="h!tbp", echo=FALSE, message=F, warning=F, fig.height=4, fig.cap="The \\protect\\emph{Biggest component of the colleganza} network. Family names are sized according to the pagerank centrality, nodes according to degree, edges to edge betweenness. In green, nodes for the \"popular\" party (opposed to the Serrata) families; red for the \"aristocratic\" party (supporting the Serrata) families.\\protect\\label{fig:colleganza}"----
colleganza <- read.csv("data/colleganza-pairs.csv", header=F)
colleganza.sn <- graph.data.frame(data.frame(colleganza$V1,colleganza$V2),directed=F)
E(colleganza.sn)$weight <- 1
colleganza.sn <- simplify(colleganza.sn, edge.attr.comb=list(weight="sum"))
V(colleganza.sn)$degree <- degree(colleganza.sn)
V(colleganza.sn)$pagerank <- unname(unlist(page_rank(colleganza.sn)$vector))

components <- igraph::clusters(colleganza.sn, mode="weak")
biggest_cluster_id <- which.max(components$csize)
vert_ids <- V(colleganza.sn)[components$membership == biggest_cluster_id]

colleganza.sn.connected <- igraph::induced_subgraph(colleganza.sn, vert_ids)
par(mar=c(1,0,1,0)+.1)

png("preso/img/fig1-colleganza.png",width=2400,height=1600)
plot(colleganza.sn.connected,
     vertex.size=V(colleganza.sn.connected)$degree/2,
     layout=layout_with_fr,
     vertex.label.cex=0.5+V(colleganza.sn.connected)$pagerank*60,
     vertex.label.dist=0.5,
     vertex.frame.width=0,
     edge.width=3*E(colleganza.sn.connected)$weight)
dev.off()

# Main component
V(colleganza.sn.connected)$color=rgb(0,0,1,0.5)
rojo.dogo <- rgb(0.7,0,0,0.8)

hanging <- which(degree(colleganza.sn.connected)==1)

colleganza.sn.main <- delete.vertices(colleganza.sn.connected,hanging)

V(colleganza.sn.main)[ V(colleganza.sn.main)$name %in% c("Contarini","Foscari", "Gradenigo","Giustiniani","Steno", "Ziani","Morosini","Moro","Grimani","Memmo") ]$color <- rojo.dogo
V(colleganza.sn.main)[ V(colleganza.sn.main)$name %in% c("Querini","Tiepolo", "Badoer","Dauro","Barozzi", "Lombardo","Pedoni") ]$color <- rgb(0,1,0,0.5)
V(colleganza.sn.main)$shape <- "circle"
V(colleganza.sn.main)$font.size <- V(colleganza.sn.main)$pagerank*2000

png("preso/img/fig1-colleganza-main.png",width=2400,height=1600)
plot(colleganza.sn.main,
     vertex.size=V(colleganza.sn.main)$degree/2,
     layout=layout_with_fr,
     vertex.label.cex=0.5+V(colleganza.sn.main)$pagerank*60,
     vertex.label.dist=0.5,
     vertex.frame.width=0,
     edge.width=3*E(colleganza.sn.main)$weight)
dev.off()

library(visNetwork)
library(htmlwidgets)
V(colleganza.sn.main)$shape <- "circle"
V(colleganza.sn.main)$font.size <- V(colleganza.sn.main)$pagerank*1000
E(colleganza.sn.main)$width <- E(colleganza.sn.main)$edge_betweenness/5
saveWidget(visIgraph(colleganza.sn.main) %>% visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE), file = "colleganza-2.html")

## ----longevity, echo=F, message=F, warning=FALSE, fig.height=3, fig.cap="A timeline of the time every doge spent in office. Time in office is represented as segment height and width, as well as with color for highlighting.\\protect\\label{fig:terms}"----
library(ggthemes)
library(viridis)
library(devtools)
library(dogesr)
data("doges.years")
doges.years$idx <- as.numeric(row.names(doges.years))
ggplot(doges.years, aes(x=Start,y=idx))+geom_segment(aes(xend=Start+Years,yend=idx,color=Years,linewidth=1+Years))+ scale_color_viridis(option = "D")+xlab("Year")+ylab("")+geom_vline(xintercept=1300, color="red",size=4,alpha=0.4)+theme_economist()+theme(axis.ticks.y=element_blank(),axis.text.y=element_blank())
ggsave("preso/img/fig2-years.png", width=12, height=8)


## ----doges, echo=F, fig.height=4, fig.cap="Doge's marital network. Red marks again the \"aristocratic\" party, green the \"popular\"; edge width corresponds to edge betweenness, name size to pagerank, and node size to degree.\\protect\\label{fig:doges}"----
E(doges.sn.connected)$edge_betweenness <- edge_betweenness(doges.sn.connected) # Comes from utils
V(doges.sn.connected)$pagerank <- unname(unlist(page_rank(doges.sn.connected)$vector))

rojo.dogo <- rgb(0.7,0,0,0.8)
V(doges.sn.connected)$color=rgb(1,0.78125,0.0390625,0.8)
V(doges.sn.connected)[ V(doges.sn.connected)$name %in% c("Contarini","Foscari", "Gradenigo","Giustiniani","Steno", "Ziani","Morosini","Moro","Grimani","Memmo") ]$color <- rojo.dogo
V(doges.sn.connected)[ V(doges.sn.connected)$name %in% c("Querini","Tiepolo", "Badoer","Dauro","Barozzi", "Lombardo","Pedoni") ]$color <- rgb(0,1,0,0.5)

max.EW <- max( E(doges.sn.connected)$edge_betweenness)
E(doges.sn.connected)$color <- "gray"
E(doges.sn.connected)[ E(doges.sn.connected)$edge_betweenness == max.EW ]$color <- "blue"
par(mar=c(1,0,1,0)+.1)


V(doges.sn.connected)$shape <- "circle"
V(doges.sn.connected)$font.size <- V(doges.sn.connected)$pagerank*1000
E(doges.sn.connected)$width <- E(doges.sn.connected)$edge_betweenness/5
saveWidget(visIgraph(doges.sn.connected) %>% visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE), file = "doges-marriages.html")

plot(doges.sn.connected,
     vertex.size=V(doges.sn.connected)$degree,
     layout=layout_as_tree(doges.sn.connected,circular=T),
     vertex.frame.color=V(doges.sn.connected)$color,
     vertex.label.cex=V(doges.sn.connected)$pagerank*60,
     vertex.label.dist=1,
     edge.color=E(doges.sn.connected)$color,
     edge.width=E(doges.sn.connected)$edge_betweenness/50)

