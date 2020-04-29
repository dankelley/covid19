library(COVID19)
library(oce)
if (!exists("d")) { # cache to save server load during code development
    d <- covid19(end=Sys.Date()-1)
    d$time <- lubridate::with_tz(as.POSIXct(d$date), "UTC")
    d$testsPerPop <- d$tests / d$pop
}
germany <- subset(d, d$country=="Germany")
canada <- subset(d, d$country=="Canada")
usa <- subset(d, d$country=="United States")
uk <- subset(d, d$country=="United Kingdom")
italy <- subset(d, d$country=="Italy")
now <- lubridate::with_tz(Sys.time(), "UTC")
tlim <- c(as.POSIXct("2020-01-15", format="%Y-%m-%d", tz="UTC"), now)
ylim <- 100 * range(c(usa$testsPerPop, canada$testsPerPop, germany$testsPerPop, uk$testsPerPop))

if (!interactive()) png("tests_per_pop.png", height=5, width=7, pointsize=9, unit="in", res=120)
pch <- 20
cex <- 3/4
oce.plot.ts(canada$time, 100*germany$testsPerPop, type="o", xaxs="i",
            cex=cex, pch=pch, grid=TRUE,
            ylab="Tests Per Population [%]", xlim=tlim, ylim=ylim, drawTimeRange=FALSE)
lines(canada$time, 100*canada$testsPerPop, col=2, type="o", pch=pch, cex=cex)
lines(usa$time, 100*usa$testsPerPop, col=3, type="o", pch=pch, cex=cex)
lines(uk$time, 100*uk$testsPerPop, col=4, type="o", pch=pch, cex=cex)
lines(italy$time, 100*italy$testsPerPop, col=5, type="o", pch=pch, cex=cex)
legend("topleft", col=1:4, pch=pch, cex=cex,
       legend=c("Germany", "Canada", "USA", "UK", "Italy"), lwd=par("lwd"), bg="white")

if (!interactive()) dev.off()

