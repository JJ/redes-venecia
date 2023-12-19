library(dplyr)
library(ggplot2)
library(trend)
library(anomalize)
library(tibble)
library(DescTools)

load("data/venice-marriages.Rda")

marriages <- marriages.raw[ !is.na(marriages.raw$year),] # Eliminates those that are not noble
marriages <- marriages[ marriages$year >= 1398,]
marriages <- marriages[ marriages$year <= 1797,]
marriages %>% group_by(year) %>% summarise(n = n(),non.patrician.wife = sum( wife_familyname_std ==""), self.marriages = sum( wife_familyname_std == husband_familyname_std)) -> marriages.by.year

marriages.by.year$percent.non.patrician <- marriages.by.year$non.patrician.wife/marriages.by.year$n

ggplot(marriages.by.year, aes(x=year, y=n)) + geom_line() + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Marriages by year")
ggplot(marriages.by.year, aes(x=year, y=percent.non.patrician)) + geom_line() + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Marriages by year")

cp.estimate <- lanzante.test(marriages.by.year$n)
change.year <- marriages.by.year[as.integer(cp.estimate$estimate),]$year

cp.estimate.2 <- pettitt.test(marriages.by.year$n)
change.year.2 <- marriages.by.year[as.integer(cp.estimate.2$estimate),]$year

marriages.before <- marriages.by.year[ marriages.by.year$year < change.year,]
marriages.after <- marriages.by.year[ marriages.by.year$year >= change.year,]

cp.non.patrician <- lanzante.test(marriages.by.year$non.patrician.wife)
change.year.non.patrician <- marriages.by.year[as.integer(cp.non.patrician$estimate),]$year

np.marriages.before <- marriages.by.year[ marriages.by.year$year < change.year.non.patrician,]
np.marriages.after <- marriages.by.year[ marriages.by.year$year >= change.year.non.patrician,]

cp.non.patrician.pc <- lanzante.test(marriages.by.year$percent.non.patrician)
change.year.non.patrician.pc <- marriages.by.year[as.integer(cp.non.patrician.pc$estimate),]$year

np.pc.marriages.before <- marriages.by.year[ marriages.by.year$year < change.year.non.patrician.pc,]
np.pc.marriages.after <- marriages.by.year[ marriages.by.year$year >= change.year.non.patrician.pc,]



# Anomaly and time series
marriage.t <- tibble( date = as.Date(as.POSIXct(paste0(marriages.by.year$year, "-12-31"))), value = marriages.by.year$n)
marriage.t <- marriage.t %>% tibbletime::as_tbl_time(index = date)

marriage.t %>% time_decompose(value) %>% anomalize(remainder) %>% time_recompose() -> marriage.decomposed

marriage.decomposed %>% plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5) + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Marriages by year")

marriages %>% group_by(year) %>% summarise( families = length(unique( c(unique(husband_familyname_std,unique(wife_familyname_std)) ) ) )) -> married.families.by.year

marriages %>% group_by(year) %>% summarise( entropy = Entropy(table(c(husband_familyname_std,wife_familyname_std)))/log2(length(c(husband_familyname_std,wife_familyname_std))) ) -> family.entropy.by.year

cp.norm.entropy <- lanzante.test(family.entropy.by.year$entropy)
change.year.norm.entropy <- family.entropy.by.year[as.integer(cp.norm.entropy$estimate),]$year

ggplot(family.entropy.by.year, aes(x=year, y=entropy)) + geom_line() + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Entropy by year")

marriage.data.by.year <- merge(married.families.by.year,marriages.by.year, by="year")
marriage.data.by.year <- merge(marriage.data.by.year,family.entropy.by.year, by="year")

z.marriages <- matrix(c(marriage.data.by.year$n, marriage.data.by.year$non.patrician.wife, marriage.data.by.year$entropy), ncol = 3)
library(ecp)
multiple.cp <- e.divisive(z.marriages)

# print the years of the change points contained in multiple.cp$ordered
for (i in 1:length(multiple.cp$order.found)) {
  print(marriage.data.by.year[multiple.cp$order.found[i],]$year)
}

# draw entropy as point color using a color and families as point size
ggplot(marriage.data.by.year, aes(x=year, y=n)) + geom_line() + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Marriages by year") + geom_point(aes(color=entropy, size=families))+scale_color_gradientn(colors=c("black","blue","green","gray","red","gold"))


cp.lanzante.entropy <- lanzante.test(family.entropy.by.year$entropy)
change.year.entropy <- family.entropy.by.year[as.integer(cp.lanzante.entropy$estimate),]$year

cp.lanzante.families <- lanzante.test(married.families.by.year$families)
change.year.families <- married.families.by.year[as.integer(cp.lanzante.families$estimate),]$year

married.families.before <- married.families.by.year[ married.families.by.year$year < change.year.families,]
married.families.after <- married.families.by.year[ married.families.by.year$year >= change.year.families,]
