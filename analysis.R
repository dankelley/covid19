library(COVID19)
library(oce)
if (!exists("ds")) { # cache to save server load during code development
    ds <- covid19(end=Sys.Date()-1)
    ds$time <- lubridate::with_tz(as.POSIXct(ds$date), "UTC")
    ds$testsPerPop <- ds$tests / ds$pop
}
canada <- subset(ds, ds$country=="Canada")
usa <- subset(ds, ds$country=="United States")
tlim <- range(c(usa$time, canada$time))
ylim <- 100 * range(c(usa$testsPerPop, canada$testsPerPop))

if (!interactive()) pdf("analysis_01.pdf", height=4, width=7, pointsize=10)

oce.plot.ts(canada$time, 100*canada$testsPerPop, type="s", xaxs="i",
            grid=TRUE,
            ylab="Tests/population [%]", xlim=tlim, ylim=ylim, drawTimeRange=FALSE)
lines(usa$time, 100*usa$testsPerPop, col=2, type="s")
legend("topleft", col=c(1,2), legend=c("Canada", "USA"), lwd=par("lwd"), bg="white")

if (!interactive()) dev.off()

