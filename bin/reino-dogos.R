library(dogesr)
library(ggplot2)
library(ggthemes)

data("doges")
doges.years <- unique( data.frame(doge=data.doges$Doge,start=data.doges$Start,years=data.doges$Years))

library(trend)
cp.estimate <- lanzante.test(doges.years$years)
change.doge <- doges.years[as.integer(cp.estimate$estimate),]$doge
change.year <- doges.years[as.integer(cp.estimate$estimate),]$start
index.cp <- as.integer(cp.estimate$estimate)
after.dandolo <- tail(doges.years,-index.cp+1)
before.dandolo <- head(doges.years,index.cp-1)
avg.before.dandolo <- mean(before.dandolo$years)
avg.after.dandolo <- mean(after.dandolo$years)

ggplot(doges.years, aes(x=start,y=years))+geom_bar(stat="identity")+theme_tufte()+geom_segment(y=avg.before.dandolo, yend=avg.before.dandolo,x=doges.years$start[1],xend=change.year, linetype="dashed", color = "red",size=2)+geom_segment(y=avg.after.dandolo, yend=avg.after.dandolo,x=change.year,xend=doges.years$start[length(doges.years$start)],linetype="dotdash", color = "blue",size=2)
