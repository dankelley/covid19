library(oce)
## The name of the Canadian data file changed sometime near the start of
## April, from the commented-out line to the line after it.
#url <- "https://health-infobase.canada.ca/src/data/summary_current.csv"
url <- "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv"

fixLastDuplicated <- function(x)
{
    if (0 == diff(tail(x$num, 2))) {
        x <- head(x, -1)
        message("NB. removed final point, because it duplicated its predecessor")
    }
    x
}

abbreviateRegion <- function(r, wide=TRUE)
{
    if (wide)
        return(r)
    if (r == "Newfoundland and Labrador")
        return("NFLD & LAB")
    else if (r == "Prince Edward Island")
        return("PEI")
    else if (r == "British Columbia")
        return("BC")
    else if (r == "New Brunswick")
        return("NB")
    else if (r == "Saskatchewan")
        return("Sask.")
    r
}

# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000901
# Q1, 2020
population <- function(region)
{
    switch(region,
           "Canada"=37894799,
           "Ontario"=14446515,
           "Quebec"=8433301,
           "British Columbia"=5020302,
           "Alberta"=4345737,
           "Manitoba"=1360396,
           "Saskatchewan"=1168423,
           "Nova Scotia"=965382,
           "New Brunswick"=772094,
           "Newfoundland and Labrador"=523790,
           "Prince Edward Island"=154748,
           "Repatriated travellers"=NA)
}



recentNumberOfDays <- 10
now <- Sys.time()
## Cache for speed during code development
if (!exists("d")) {
    message("downloading from ", url)
    d <- read.csv(url)
    d$time <- as.POSIXct(d$date, format="%d-%m-%Y")
    d$num <- d$numconf + d$numprob
    d$deaths <- d$numdeaths
} else {
    message("using downloaded data")
}
regions <- c("Canada", "Alberta", "British Columbia" , "Manitoba", "New Brunswick",
             "Newfoundland and Labrador", "Nova Scotia", "Ontario",
             "Prince Edward Island", "Quebec", "Saskatchewan",
             "Repatriated travellers")

width <- 8
height <- 5
res <- 200
pointsize <- 9

if (!interactive())
    png("canada_linear.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
par(mfrow=c(4, 3))
## Problem: "Repatriated travellers" and "Repatriated Travellers" both exist.
tlim <- range(d$time)
## Ignore the territories (few data) and also repatriated travellers (oddly broken
## up into two groups, presumably because of poor data handling).

message("linear plots")
for (region in regions) {
    message("Handling ", region)
    sub <- subset(d, tolower(prname)==tolower(region))
    sub <- fixLastDuplicated(sub)
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    oce.plot.ts(sub$time, sub$num,
                mar=c(2, 3, 1, 1),
                ylab="Cases & Deaths", xlim=tlim,
                type="p", pch=20, col=ifelse(recent, "black", "gray"),
                drawTimeRange=FALSE)
    points(sub$time, sub$numdeaths, pch=20, col=ifelse(recent, "red", "pink"), cex=ifelse(recent, 1, 0.7))
    mtext(sprintf(" Confirmed: %d (%.4f%%)",
                  tail(sub$num,1), 100*tail(sub$num,1)/population(region)),
          line=-1, cex=par("cex"), adj=0)
    mtext(sprintf(" Deaths: %d (%.4f%%)",
                  tail(sub$deaths,1), 100*tail(sub$deaths,1)/population(region)),
          line=-2, cex=par("cex"), adj=0)
    mtext(region, cex=par("cex"), adj=0)
    mtext(paste(format(tail(sub$time,1), "%b %d")), adj=1, cex=par("cex"))
}
if (!interactive())
    dev.off()
if (!interactive())
    png("canada_log.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
par(mfrow=c(4, 3))
message("log plots")
## Uniform scale for all log plots, to make
## it easier to see slope differences.
ylim <- c(1, 2*max(d$num, na.rm=TRUE))
for (region in regions) {
    message("Handling ", region)
    sub <- subset(d, tolower(prname)==tolower(region))
    sub <- fixLastDuplicated(sub)
    sub <- sub[sub$num > 0, ]
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    lastDuplicated <- 0 == diff(tail(sub$num, 2))
    if (lastDuplicated) {
        sub <- head(sub, -1)
        message("NB. removed final point, because it duplicated its predecessor")
    }
    if (any(sub$numconf + sub$numprob > 0)) {
        positive <- sub$num > 0
        oce.plot.ts(sub$time[positive], sub$num[positive],
                    mar=c(2, 3, 1, 1),
                    ylab="Cases & Deaths", xlim=tlim,
                    type="p", pch=20, col=ifelse(recent, "black", "gray"),
                    cex=ifelse(recent, 1, 0.7),
                    ylim=ylim, log="y", logStyle="decade",
                    drawTimeRange=FALSE)
        if (any(sub$numdeaths[is.finite(sub$numdeaths)]) > 0)
            points(sub$time, sub$numdeaths, pch=20, col=ifelse(recent, "red", "pink"), cex=ifelse(recent, 1, 0.7))
        ## Case doubling time
        y <- (sub$numconf + sub$numprob)[recent]
        ok <- y > 0
        x <- (as.numeric(sub$time)[recent])[ok]
        y <- log10(y[ok])
        canFit <- length(x) > 3 && tolower(region) != "repatriated travellers"
        if (canFit) {
            m <- lm(y ~ x)
            xx <- seq(par("usr")[1], par("usr")[2], length.out=100)
            growthRate <- coef(m)[2] * 86400 # in days
            t2 <- log10(2) / growthRate
            if (0 < t2 && t2 < 100) {
                lines(xx, 10^predict(m, list(x=xx)), lty="dotted")
                mtext(sprintf(" cases double in %.0fd", t2), side=3, adj=0, line=-1, cex=par("cex"))
            }
        }
        ## Death doubling time
        y <- sub$deaths[recent]
        ok <- y > 0
        x <- (as.numeric(sub$time)[recent])[ok]
        y <- log10(y[ok])
        canFit <- length(x) > 3 && tolower(region) != "repatriated travellers"
        if (canFit) {
            m <- lm(y ~ x)
            xx <- seq(par("usr")[1], par("usr")[2], length.out=100)
            growthRate <- coef(m)[2] * 86400 # in days
            t2 <- log10(2) / growthRate
            if (0 < t2 && t2 < 100) {
                lines(xx, 10^predict(m, list(x=xx)), col="red", lty="dotted")
                mtext(sprintf(" deaths double in %.0fd", t2), side=3, adj=0, line=-2, col="red", cex=par("cex"))
            }
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
    png("canada_change.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
par(mfrow=c(4, 3))
for (region in regions) {
    message("Handling ", region)
    sub <- subset(d, tolower(prname)==tolower(region))
    sub <- fixLastDuplicated(sub)
    y <- diff(sub$num)
    oce.plot.ts(sub$time[-1], y, drawTimeRange=FALSE, ylab="Daily Cases", type="p",
                mar=c(2, 3, 1, 1),
                xlim=tlim, col="darkgray", pch=20, cex=par("cex"))# * ifelse(y==0, 0.25, 1))
    ## spline with df proportional to data length (the 7 is arbitrary)
    ok <- is.finite(y)
    lines(smooth.spline(sub$time[-1][ok], y[ok], df=length(y)/7), col="magenta")
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    points(sub$time[-1][recent], y[recent], pch=20, cex=par("cex"))
    mtext(region, cex=par("cex"), adj=0)
    mtext(paste(format(tail(sub$time,1), "%b %d")), adj=1, cex=par("cex"))
}

if (!interactive())
    dev.off()
