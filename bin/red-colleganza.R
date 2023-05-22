library(igraph)

marriages <- read.csv("../data/venice_colleganza_puga_treffler_families.csv")
family.names <- unique( c(marriages$husband_familyname_std,marriages$wife_familyname_std))
