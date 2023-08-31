## ----setup, echo=F,message=F-----------------------------------------------------------------------------
library(igraph)
library(knitr)
library(stringr)
library(kableExtra)
tablify <- function( graph, column ) {
  m.table <- data.frame( Family=V(graph)$name, degree=vertex_attr(graph,column) )
  names(m.table)[names(m.table) == "degree"] <- column
  return( m.table[ order(m.table[[column]],decreasing = T),] )
}


load("data/venice-marriages.Rda")
marriages <- marriages.raw[ marriages.raw$wife_familyname_std != '',] # Eliminates those that are not noble

# Use self-loops
all.marriages <- marriages                                    # Keep marriages with intra-family ties
all.marriages.sn <- graph.data.frame(data.frame(all.marriages$husband_familyname_std,all.marriages$wife_familyname_std),directed=F)
V(all.marriages.sn)$eigen <-  unname(unlist(eigen_centrality(all.marriages.sn)$vector))
all.m.eigen <- tablify( all.marriages.sn, "eigen")

marriages.plot <- marriages[ marriages$wife_familyname_std == marriages$wife_familyname_std,]
plot.marriages.sn <- graph.data.frame(data.frame(marriages.plot$husband_familyname_std,marriages.plot$wife_familyname_std),directed=F)
V(plot.marriages.sn)$eigen <-  unname(unlist(eigen_centrality(plot.marriages.sn)$vector))
hanging <- which(degree(plot.marriages.sn)==1)
marriages.sn.main <- delete.vertices(marriages.sn,hanging)


plot(marriages.sn.main,
     vertex.size=V(marriages.sn.main)$eigen*6,
     vertex.label.cex=V(marriages.sn.main)$eigen*2,
     vertex.label.dist=1)
top.ev.all.eigen <- all.m.eigen %>% head(.,10)

# Eliminate self-loops
marriages <- marriages[ marriages$husband_familyname_std != marriages$wife_familyname_std,]
marriages.sn <- graph.data.frame(data.frame(marriages$husband_familyname_std,marriages$wife_familyname_std),directed=F)
V(marriages.sn)$eigen <-  unname(unlist(eigen_centrality(marriages.sn)$vector))
m.eigen <- tablify( marriages.sn, "eigen")
top.ev.eigen <- m.eigen %>% head(.,10)

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ggthemes)

marriages.self <- marriages.raw[ marriages.raw$husband_familyname_std == marriages.raw$wife_familyname_std,]
sorted.marriages.self <- marriages.self %>% count(wife_familyname_std, sort=T)
intra.marriages <- unique(marriages.self$wife_familyname_std)

top.intra.marriages <-  sorted.marriages.self %>% head(.,10)

kable(top.intra.marriages)

tabled.top.eigen <- data.frame(Family = top.ev.eigen$Family, EV = top.ev.eigen$eigen, Family.Self=top.ev.all.eigen$Family, EV.self = top.ev.all.eigen$eigen)

kable(top.intra.marriages)

## ----charts, echo=F, fig.show="hold", out.width="50%",message=F, fig.cap="(Left) EV centrality considering self-loops (x axis) or not (y axis). The size of the plot and color signal the number of intra-family marriages. (Right) Number of intra-family marriages vs. percent increment of EV when self-loops are not computed. \\protect\\label{fig:plots}"----
all.names <- unique( c( marriages.raw$wife_familyname_std, marriages.raw$husband_familyname_std))
all.marriages <- marriages.raw %>% select(husband_familyname_std,wife_familyname_std) %>% rowid_to_column() %>% pivot_longer(-rowid) %>% group_by(value) %>% summarise(n = n_distinct(rowid)) %>% arrange(desc(n))
marriages.table <- left_join(all.marriages,sorted.marriages.self,by=c("value" = "wife_familyname_std"))
marriages.table$intra.marriages.rate <- marriages.table$n.y / marriages.table$n.x

marriages.summary <- data.frame(family=intra.marriages, EV.with.self=sapply( intra.marriages, function(x) V(all.marriages.sn)[[x]]$eigen ), EV.no.self=sapply( intra.marriages, function(x) V(marriages.sn)[[x]]$eigen ), intra.marriages=sapply( intra.marriages, function(x) sorted.marriages.self[sorted.marriages.self$wife_familyname_std ==x,]$n ))
ggplot(marriages.summary, aes(x=EV.with.self,y=EV.no.self, color=intra.marriages, size=intra.marriages))+geom_point()+geom_abline(intercept=0,slope=1,linetype="dashed",color="red")+scale_x_log10()+scale_y_log10()
ggsave("preso/img/fig1-ev-self-vs-no.png", width=8, height=4.5)

marriages.summary$diff <- (marriages.summary$EV.no.self - marriages.summary$EV.with.self)/marriages.summary$EV.no.self
ggplot(marriages.summary, aes(x=intra.marriages,y=diff))+geom_point()+theme_fivethirtyeight()
ggsave("preso/img/fig1-ev-change-vs-marriages.png", width=8, height=4.5)

