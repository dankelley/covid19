library(oce)
library(COVID19)

recentNumberOfDays <- 10
now <- Sys.time()
## Cache for speed during code development
if (!exists("d")) {
    message("downloading")
    d <- covid19(ISO="USA", level=2)
    d$time <- as.POSIXct(d$date, format="%d-%m-%Y", tz="UTC")
    d$num <- d$confirmed
} else {
    message("using downloaded data")
}
# select 10 regions
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
## Problem: "Repatriated travellers" and "Repatriated Travellers" both exist.
tlim <- range(d$time)
## Ignore the territories (few data) and also repatriated travellers (oddly broken
## up into two groups, presumably because of poor data handling).

message("linear plots")
for (region in regions) {
    message("Handling ", region)
    if (region == "-") {
        plot(0:1, 0:1, xlab="", ylab="", type="n", axes=FALSE)
        text(0.5, 0.5, "Suggest a state")
        box()
        next
    }
    sub <- subset(d, tolower(d$state)==tolower(region))
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    oce.plot.ts(sub$time, sub$num,
                mar=c(2, 3, 1, 1),
                ylab="Cases & Deaths", xlim=tlim,
                type="p", pch=20, col=ifelse(recent, "black", "gray"),
                drawTimeRange=FALSE)
    points(sub$time, sub$num, pch=20, col=ifelse(recent, "red", "pink"), cex=ifelse(recent, 1, 0.7))
    mtext(paste0(" ", region, " / ",
                 format(tail(sub$time,1), "%b %d")),
          cex=par("cex"), adj=0, line=-1)
    mtext(sprintf(" Confirmed: %d (%.4f%%)",
                  tail(sub$num,1), 100*tail(sub$num,1)/sub$pop[1]),
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
message("log plots")
## Uniform scale for all log plots, to make
## it easier to see slope differences.
ylim <- c(1, 2*max(d$num, na.rm=TRUE))
for (region in regions) {
    message("Handling ", region)
    if (region == "-") {
        plot(0:1, 0:1, xlab="", ylab="", type="n", axes=FALSE)
        text(0.5, 0.5, "Suggest a state")
        box()
        next
    }
    sub <- subset(d, tolower(d$state)==tolower(region))
    sub <- sub[sub$num > 0, ]
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    lastDuplicated <- 0 == diff(tail(sub$num, 2))
    if (lastDuplicated) {
        sub <- head(sub, -1)
        message("NB. removed final point, because it duplicated its predecessor")
    }
    if (any(sub$num > 0)) {
        positive <- sub$num > 0
        oce.plot.ts(sub$time[positive], sub$num[positive],
                    mar=c(2, 3, 1, 1),
                    ylab="Cases & Deaths", xlim=tlim,
                    type="p", pch=20, col=ifelse(recent, "black", "gray"),
                    cex=ifelse(recent, 1, 0.7),
                    ylim=ylim, log="y", logStyle="decade",
                    drawTimeRange=FALSE)
        if (any(sub$num[is.finite(sub$num)] > 0))
            points(sub$time, sub$num, pch=20, col=ifelse(recent, "red", "pink"), cex=ifelse(recent, 1, 0.7))
        y <- (sub$num)[recent]
        ok <- y > 0
        x <- (as.numeric(sub$time)[recent])[ok]
        y <- log10(y[ok])
        canFit <- length(x) > 3 && tolower(region) != "repatriated travellers"
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
    message("Handling ", region)
    if (region == "-") {
        plot(0:1, 0:1, xlab="", ylab="", type="n", axes=FALSE)
        text(0.5, 0.5, "Suggest a state")
        box()
        next
    }
    sub <- subset(d, tolower(d$state)==tolower(region))
    y <- c(0, diff(sub$num))
    ok <- y > 0
    y <- y[ok]
    sub <- subset(sub, ok)
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

if (!interactive())
    dev.off()
