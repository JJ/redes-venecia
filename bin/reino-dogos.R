library(dogesr)
library(ggplot2)
library(ggthemes)

data("doges")
doges.years <- unique( data.frame(doge=data.doges$Doge,start=data.doges$Start,years=data.doges$Years))

library(trend)
cp.estimate <- lanzante.test(doges.years$years)
change.doge <- doges.years[as.integer(cp.estimate$estimate),]$doge
change.year <- doges.years[as.integer(cp.estimate$estimate),]$start
max.years <-max(doges.years$years)
index.cp <- as.integer(cp.estimate$estimate)
after.dandolo <- tail(doges.years,-index.cp+1)
before.dandolo <- head(doges.years,index.cp-1)
avg.before.dandolo <- mean(before.dandolo$years)
lm.before <- lm( formula= years ~ start, data=before.dandolo) # No significativo
avg.after.dandolo <- mean(after.dandolo$years)

ggplot(doges.years, aes(x=start,y=years,fill=factor(ifelse(years==max.years,"Francesco Foscari","Other"))))+geom_bar(stat="identity")+ scale_fill_manual(name = "", values=c(rgb(0.7,0,0),"grey50"))+theme_tufte()+geom_segment(y=avg.before.dandolo, yend=avg.before.dandolo,x=doges.years$start[1],xend=change.year, linetype="dashed", color = "red",size=2)+geom_segment(y=avg.after.dandolo, yend=avg.after.dandolo,x=change.year,xend=doges.years$start[length(doges.years$start)],linetype="dotdash", color = "blue",size=2)+geom_segment(color="green",aes(x = 1400, y = 25, xend = change.year, yend = (avg.before.dandolo - avg.after.dandolo)/2+avg.after.dandolo),arrow = arrow(length = unit(0.5, "cm")))+annotate(geom="text", x=1350, y=26, label="Serrata", color="darkgreen", size=6)+theme(legend.position=c(0.5,0.9))+xlab("year")+ylab("Duration")
