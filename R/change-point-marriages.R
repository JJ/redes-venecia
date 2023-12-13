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
marriages %>% group_by(year) %>% summarise(n = n()) -> marriages.by.year

ggplot(marriages.by.year, aes(x=year, y=n)) + geom_line() + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Marriages by year")

cp.estimate <- lanzante.test(marriages.by.year$n)
change.year <- marriages.by.year[as.integer(cp.estimate$estimate),]$year

cp.estimate.2 <- pettitt.test(marriages.by.year$n)
change.year.2 <- marriages.by.year[as.integer(cp.estimate.2$estimate),]$year

marriages.before <- marriages.by.year[ marriages.by.year$year < change.year,]
marriages.after <- marriages.by.year[ marriages.by.year$year >= change.year,]

marriage.t <- tibble( date = as.Date(as.POSIXct(paste0(marriages.by.year$year, "-12-31"))), value = marriages.by.year$n)
marriage.t <- marriage.t %>% tibbletime::as_tbl_time(index = date)

marriage.t %>% time_decompose(value) %>% anomalize(remainder) %>% time_recompose() -> marriage.decomposed

marriage.decomposed %>% plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5) + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Marriages by year")

marriages %>% group_by(year) %>% summarise( families = length(unique( c(unique(husband_familyname_std,unique(wife_familyname_std)) ) ) )) -> married.families.by.year

marriages %>% group_by(year) %>% summarise( norm.entropy = (unique( c(unique(husband_familyname_std,unique(wife_familyname_std)) ) ) )) -> married.families.by.year

# merge married.families.by.year and marriages.by.year
marriage.data.by.year <- merge(married.families.by.year,marriages.by.year,by="year")
