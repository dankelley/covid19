library(oce)
url <- "https://health-infobase.canada.ca/src/data/summary_current.csv"
now <- Sys.time()
## Cache for speed during code development
if (!exists("d")) {
    message("downloading from ", url)
    d <- read.csv(url)
    d$time <- as.POSIXct(d$date, format="%d-%m-%Y")
} else {
    message("using downloaded data")
}
regions <- c("Canada", "Alberta", "British Columbia" , "Manitoba", "New Brunswick",
             "Newfoundland and Labrador", "Nova Scotia", "Ontario",
             "Prince Edward Island", "Quebec", "Saskatchewan",
             "Repatriated travellers")


if (!interactive())
    png("canada_linear.png", width=7, height=5, unit="in", res=150, pointsize=11)
par(mfrow=c(3, 4))
## Problem: "Repatriated travellers" and "Repatriated Travellers" both exist.
tlim <- range(d$time)
## Ignore the territories (few data) and also repatriated travellers (oddly broken
## up into two groups, presumably because of poor data handling).

message("linear plots")
for (province in regions) {
    message(province)
    sub <- subset(d, tolower(prname)==tolower(province))
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= 7 * 86400
    oce.plot.ts(sub$time, sub$numconf + sub$numprob,
                mar=c(2, 3, 1, 1),
                ylab="Cases", xlim=tlim,
                type="p", pch=20, col=ifelse(recent, "black", "gray"),
                drawTimeRange=FALSE)
    points(sub$time, sub$numdeaths, pch=20, col=ifelse(recent, "red", "pink"), cex=ifelse(recent, 1, 0.7))
    mtext(province, cex=par("cex"))
}
if (!interactive())
    dev.off()
if (!interactive())
    png("canada_log.png", width=7, height=5, unit="in", res=150, pointsize=11)
par(mfrow=c(3, 4))
message("log plots")
for (province in regions) {
    message(province)
    sub <- subset(d, tolower(prname)==tolower(province))
    ok <- sub$numconf + sub$numprob > 0
    sub <- sub[ok, ]
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) < 7 * 86400
    y <- sub$numconf + sub$numprob
    if (any(y > 0)) {
        ylim <- c(1, 2*max(y, na.rm=TRUE))
        oce.plot.ts(sub$time, y,
                    mar=c(2, 3, 1, 1),
                    ylab="Cases", xlim=tlim,
                    type="p", pch=20, col=ifelse(recent, "black", "gray"),
                    cex=ifelse(recent, 1, 0.7),
                    ylim=ylim, log="y", logStyle="decade",
                    drawTimeRange=FALSE)
        if (any(sub$numdeaths) > 0)
            points(sub$time, sub$numdeaths, pch=20, col=ifelse(recent, "red", "pink"), cex=ifelse(recent, 1, 0.7))
        y <- (sub$numconf + sub$numprob)[recent]
        ok <- y > 0
        x <- (as.numeric(sub$time)[recent])[ok]
        y <- log10(y[ok])
        canFit <- length(x) > 3 && tolower(province) != "repatriated travellers"
        if (canFit) {
            m <- lm(y ~ x)
            xx <- seq(par("usr")[1], par("usr")[2], length.out=100)
            lines(xx, 10^predict(m, list(x=xx)))
            growthRate <- coef(m)[2] * 86400 # in days
            doubleTime <- log10(2) / growthRate
            mtext(sprintf(" Doubling time: %.1fd", doubleTime), side=3, adj=0, line=-1, cex=par("cex"))
        }
    } else {
        plot(0:1, 0:1, xlab="", ylab="", type="n")
        text(0.5, 0.5, "No counts")
    }
    mtext(province, cex=par("cex"), adj=0)
}
if (!interactive())
    dev.off()
