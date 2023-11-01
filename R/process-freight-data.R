library(dplyr)
freight.traffic <- read.csv("data/freight-US.csv", header = TRUE, sep = ";")

freight.traffic$freight <- as.numeric(gsub(",", ".", freight.traffic$Valores.de.medidas))
freight.traffic %>% 
  group_by(Origin.State, Dest.State, Year) %>% 
  summarise(sum = sum(freight)) -> freight.traffic.agg
save(freight.traffic.agg, file = "data/freight-traffic-agg.RData")   
  