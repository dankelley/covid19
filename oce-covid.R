## https://cran.r-project.org/web/packages/COVID19/readme/README.html
library(COVID19)
ds <- world("state")
dc <- world("country")
par(mfrow=c(2, 1))

place <- "Nova Scotia"
d <- ds[ds$state == place, ]
oce::oce.plot.ts(d$date, d$confirmed, drawTimeRange=FALSE, ylab="Cases", type="p", pch=20)
mtext(place, adj=0)
mtext(paste(tail(d$confirmed,1), "cases as of", tail(d$date,1)), adj=1)


place <- "Canada"
d <- dc[dc$country == place, ]
oce::oce.plot.ts(d$date, d$confirmed, drawTimeRange=FALSE, ylab="Cases", type="p", pch=20)
mtext(place, adj=0)
mtext(paste(tail(d$confirmed,1), "cases as of", tail(d$date,1)), adj=1)


