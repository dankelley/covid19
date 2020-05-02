library(COVID19)
library(oce)
if (!exists("d")) { # cache to save server load during code development
    d <- covid19(end=Sys.Date()-1)
    d$time <- lubridate::with_tz(as.POSIXct(d$date), "UTC")
    d$testsPerPop <- d$tests / d$pop
}
now <- lubridate::with_tz(Sys.time(), "UTC")
places <- c("Canada", "Germany", "Italy", "United Kingdom", "United States")
cols <- seq_along(places)
ylim <- c(0, 100*max(sapply(places, function(p) max(subset(d, d$country==p)$testsPerPop))))
if (!interactive()) png("tests_per_pop.png", height=5, width=7, pointsize=9, unit="in", res=120)
pch <- 20
for (i in seq_along(places)) {
    sub <- subset(d, d$country == places[i])
    if (i == 1) {
        oce.plot.ts(sub$time, 100*sub$testsPerPop, type="p", xaxs="i",
                    pch=pch, grid=TRUE, col=cols[i],
                    ylab="Tests Per Population [%]", ylim=ylim, drawTimeRange=FALSE)
    } else {
        points(sub$time, 100*sub$testsPerPop, col=cols[i], pch=pch)
    }
}
legend("topleft", col=cols, pch=pch, cex=par("cex"), pt.cex=par("cex"), legend=places, bg="white")

if (!interactive()) dev.off()

