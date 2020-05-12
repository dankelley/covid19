library(COVID19)
library(oce)
if (!exists("d")) { # cache to save server load during code development
    d <- covid19(end=Sys.Date()-1)
    d$time <- lubridate::with_tz(as.POSIXct(d$date), "UTC")
    d$testsPerPop <- d$tests / d$pop
}
## These are the R4 colours
now <- lubridate::with_tz(Sys.time(), "UTC")
places <- c("Canada", "France", "Germany", "Italy", "Korea, South", "United Kingdom", "United States")
R4colors <- c("#000000", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#CD0BBC",
              "#F5C710", "#9E9E9E")
cols <- R4colors[seq_along(places)%%length(R4colors)]
ylim <- c(0, 100*max(sapply(places, function(p) max(subset(d, d$country==p)$testsPerPop))))
if (!interactive()) png("tests_per_pop.png", height=5, width=7, pointsize=9, unit="in", res=120)
lwd <- 2
pch <- 20
for (i in seq_along(places)) {
    sub <- subset(d, d$country == places[i])
    if (i == 1) {
        oce.plot.ts(sub$time, 100*sub$testsPerPop, type="l", lwd=lwd, xaxs="i",
                    pch=pch, grid=TRUE, col=cols[i],
                    ylab="Tests Per Population [%]", ylim=ylim, drawTimeRange=FALSE)
    } else {
        lines(sub$time, 100*sub$testsPerPop, col=cols[i], pch=pch, lwd=lwd)
    }
}
legend("topleft", col=cols, lwd=lwd, legend=places, bg="white")
mtext(format(tail(sub$time,1), "%Y %b %d"))
if (!interactive()) dev.off()

