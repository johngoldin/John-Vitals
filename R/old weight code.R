tmp<-date.mdy(weight_data$d)
months<-c(31,28,31,30,31,30,31,31,30,31,30,31)
cum_month <- rep(0,12)
month_seq <- seq(1,12,1)
month_name <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
# why 143 in the next line? 12 * 12 - 1 ?
yvalue <- rep(143,12)
xvalue <- rep(1,12)
for(i in seq(2,12))  {for(j in seq(1,i-1)) xvalue[i]<-cum_month[i]}
note1 <- data.frame(xvalue,yvalue,month_name)
for(i in seq(2,12))  {for(j in seq(1,i-1)) cum_month[i]<-cum_month[i]+months[j]}
daycnt <- cum_month[tmp$month]+tmp$day
weight_data$days <- daycnt

weight_data <- subset(weight_data,!((tmp$month==2)&(tmp$day==29)))
weight_data$adjusted<-ifelse(weight_data$adjusted>1000,weight_data$adjusted/10,weight_data$adjusted)

# this version does one line per year
p <- qplot(days, adjusted, data=weight_data, geom=c("smooth"),  
           colour=year,main="Weight by Year",
           xlab="days",ylab="pounds")

# this version does a single line, but a different color and smoother for each year
fp <- qplot(d, adjusted, data=weight_data,geom=c("point"), size=0.5,main="Weight by Day",
            xlab="date",ylab="pounds", colour=year)
#fp <- fp + stat_smooth(method=loess,span=0.058, legend=FALSE)
fp <- fp + stat_smooth(method=loess,span=0.75, legend=FALSE)
