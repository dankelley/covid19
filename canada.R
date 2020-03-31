library(oce)
url <- "https://health-infobase.canada.ca/src/data/summary_current.csv"
recentNumberOfDays <- 10
now <- Sys.time()
## Cache for speed during code development
if (!exists("d")) {
    message("downloading from ", url)
    d <- read.csv(url)
    d$time <- as.POSIXct(d$date, format="%d-%m-%Y")
    d$num <- d$numconf + d$numprob
} else {
    message("using downloaded data")
}
regions <- c("Canada", "Alberta", "British Columbia" , "Manitoba", "New Brunswick",
             "Newfoundland and Labrador", "Nova Scotia", "Ontario",
             "Prince Edward Island", "Quebec", "Saskatchewan",
             "Repatriated travellers")

if (!interactive())
    png("canada_linear.png", width=7, height=6, unit="in", res=150, pointsize=11)
par(mfrow=c(3, 4))
## Problem: "Repatriated travellers" and "Repatriated Travellers" both exist.
tlim <- range(d$time)
## Ignore the territories (few data) and also repatriated travellers (oddly broken
## up into two groups, presumably because of poor data handling).

message("linear plots")
for (province in regions) {
    message(province)
    sub <- subset(d, tolower(prname)==tolower(province))
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    oce.plot.ts(sub$time, sub$num,
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
    png("canada_log.png", width=7, height=6, unit="in", res=150, pointsize=11)
par(mfrow=c(3, 4))
message("log plots")
## Uniform scale for all log plots, to make
## it easier to see slope differences.
ylim <- c(1, 2*max(d$num, na.rm=TRUE))
for (province in regions) {
    message(province)
    sub <- subset(d, tolower(prname)==tolower(province))
    sub <- sub[sub$num > 0, ]
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    lastDuplicated <- 0 == diff(tail(sub$numconf+sub$numprob, 2))
    if (lastDuplicated) {
        sub <- head(sub, -1)
        message("NB. removed final point, because it duplicated its predecessor")
    }
    if (any(sub$numconf + sub$numprob > 0)) {
        positive <- sub$num > 0
        oce.plot.ts(sub$time[positive], sub$num[positive],
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
