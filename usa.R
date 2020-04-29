library(oce)
source("get_data.R")


recentNumberOfDays <- 10
now <- Sys.time()
mar <- c(2, 3, 1.5, 1.5)

# select 12 regions, shown in a 3x4 array of panels.
regions <- c("California",
             "Georgia",
             "Louisiana",
             "Maine",
             "Massachusetts",
             "Montana",
             "New Hampshire",
             "New York",
             "Ohio",
             "Tennessee",
             "Vermont",
             "Washington")

width <- 7
height <- 5
res <- 150
pointsize <- 9

if (!interactive())
    png("usa_linear.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
par(mfrow=c(4, 3))
tlim <- c(as.POSIXct("2020-01-15", format="%Y-%m-%d", tz="UTC"), now)
## Ignore the territories (few data) and also repatriated travellers (oddly broken
## up into two groups, presumably because of poor data handling).

for (region in regions) {
    message("Handling linear plot for ", region)
    if (region == "-") {
        plot(0:1, 0:1, xlab="", ylab="", type="n", axes=FALSE)
        text(0.5, 0.5, "Suggest a state")
        box()
        next
    }
    sub <- getData("United States", region)
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    oce.plot.ts(sub$time, sub$cases,
                ylab="Cases & Deaths", xlim=tlim,
                type="p", pch=20, col=ifelse(recent, "black", "gray"), cex=par("cex"),
                mar=mar,
                drawTimeRange=FALSE)
    points(sub$time, sub$cases, pch=20, col=ifelse(recent, "black", "gray"), cex=ifelse(recent, 1, 0.7))
    points(sub$time, sub$deaths,
           pch=20,
           col=ifelse(recent, "red", "pink"),
           cex=par("cex"))

    mtext(paste0(" ", region, " / ",
                 format(tail(sub$time,1), "%b %d")),
          cex=par("cex"), adj=0, line=-1)
    mtext(sprintf(" Confirmed: %d (%.4f%%)",
                  tail(sub$cases,1), 100*tail(sub$cases,1)/sub$pop[1]),
          line=-2, cex=par("cex"), adj=0)
    mtext(sprintf(" Deaths: %d (%.4f%%)",
                  tail(sub$deaths,1), 100*tail(sub$deaths,1)/sub$pop[1]),
          line=-3, cex=par("cex"), adj=0)
}
if (!interactive())
    dev.off()

if (!interactive())
    png("usa_log.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
par(mfrow=c(4, 3))
## Uniform scale for all log plots, to make
## it easier to see slope differences.
for (region in regions) {
    message("Handling log plot for ", region)
    if (region == "-") {
        plot(0:1, 0:1, xlab="", ylab="", type="n", axes=FALSE)
        text(0.5, 0.5, "Suggest a state")
        box()
        next
    }
    sub <- getData("United States", region)
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    if (any(sub$cases > 0)) {
        positive <- sub$cases > 0
        oce.plot.ts(sub$time[positive], sub$cases[positive],
                    mar=c(2, 3, 1, 1),
                    ylab="Cases & Deaths", xlim=tlim,
                    type="p", pch=20, col=ifelse(recent, "black", "gray"),
                    cex=ifelse(recent, 1, 0.7),
                    log="y", logStyle="decade",
                    drawTimeRange=FALSE)
        positive <- sub$deaths > 0
        points(sub$time[positive], sub$deaths[positive], pch=20,
               col=ifelse(recent[positive], "pink", "red"),
               cex=par("cex")*ifelse(recent[positive], 1, 0.7))
        if (any(sub$cases[is.finite(sub$cases)] > 0))
            points(sub$time, sub$cases, pch=20, col=ifelse(recent, "black", "gray"), cex=ifelse(recent, 1, 0.7))
        y <- (sub$cases)[recent]
        ok <- y > 0
        x <- (as.numeric(sub$time)[recent])[ok]
        y <- log10(y[ok])
        canFit <- length(x) > 3
        if (canFit) {
            m <- lm(y ~ x)
            xx <- seq(par("usr")[1], par("usr")[2], length.out=100)
            lines(xx, 10^predict(m, list(x=xx)))
            growthRate <- coef(m)[2] * 86400 # in days
            doubleTime <- log10(2) / growthRate
            if (doubleTime > 0)
                mtext(if (doubleTime < 100) sprintf(" Doubling time: %.1f days", doubleTime) else " Doubling time > 100 days",
                      side=3, adj=0, line=-1, cex=par("cex"))
        }
    } else {
        plot(0:1, 0:1, xlab="", ylab="", type="n")
        text(0.5, 0.5, "No counts")
    }
    mtext(region, cex=par("cex"), adj=0)
    mtext(paste(format(tail(sub$time,1), "%b %d")), adj=1, cex=par("cex"))
}
if (!interactive())
    dev.off()

if (!interactive())
    png("usa_change.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
par(mfrow=c(4, 3))
for (region in regions) {
    message("Handling change-plot for ", region)
    if (region == "-") {
        plot(0:1, 0:1, xlab="", ylab="", type="n", axes=FALSE)
        text(0.5, 0.5, "Suggest a state")
        box()
    } else {
        sub <- getData("United States", region)
        y <- c(0, diff(sub$cases))
        ok <- y > 0
        y <- y[ok]
        sub$time <- sub$time[ok]
        sub$cases <- sub$cases[ok]
        sub$deaths <- sub$deaths[ok]
        oce.plot.ts(sub$time, y, drawTimeRange=FALSE, ylab="Daily Cases", type="p",
                    mar=c(2, 3, 1, 1),
                    xlim=tlim, col="darkgray", pch=20, cex=par("cex"))# * ifelse(y==0, 0.25, 1))
        ## spline with df proportional to data length (the 7 is arbitrary)
        ok <- is.finite(y)
        lines(smooth.spline(sub$time[ok], y[ok], df=length(y)/7), col="magenta")
        recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
        points(sub$time[recent], y[recent], pch=20, cex=par("cex"))
        mtext(paste0(" ", region, " / ",
                     format(tail(sub$time,1), "%b %d")),
              cex=par("cex"), adj=0, line=-1)
    }
}

if (!interactive())
    dev.off()
