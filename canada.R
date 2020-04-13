library(COVID19)
library(oce)
width <- 6
height <- 4.5
res <- 150
pointsize <- 8
mar <- c(1.8, 3, 0.75, 1.5)

ds <- world("state")
tlim <- range(as.POSIXct(ds$date))

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

## The name of the Canadian data file changed sometime near the start of
## April, from the commented-out line to the line after it.
#url <- "https://health-infobase.canada.ca/src/data/summary_current.csv"
##> url <- "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv"
##> fixLastDuplicated <- function(x)
##> {
##>     if (0 == diff(tail(x$num, 2))) {
##>         x <- head(x, -1)
##>         message("NB. removed final point, because it duplicated its predecessor")
##>     }
##>     x
##> }

recentNumberOfDays <- 10
now <- Sys.time()
regions <- c("Alberta", "British Columbia" , "Manitoba", "New Brunswick",
             "Newfoundland and Labrador", "Nova Scotia", "Ontario",
             "Prince Edward Island", "Quebec", "Saskatchewan")

if (!interactive())
    png("canada_linear.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)

message("# linear plots")
par(mfrow=c(5, 2))
for (region in regions) {
    message("  ", region)
    sub <- ds[ds$state == region, ]
    sub$time <- lubridate::with_tz(as.POSIXct(sub$date), tz="UTC")
    time <- sub$time
    num <- sub$confirmed
    deaths <- sub$deaths
    recent <- abs(as.numeric(now) - as.numeric(time)) <= recentNumberOfDays * 86400
    oce.plot.ts(time, num,
                mar=mar,
                ylab="Cases & Deaths", xlim=tlim,
                type="p", pch=20, col=ifelse(recent, "black", "gray"),
                drawTimeRange=FALSE)
    points(time, deaths, pch=20, col=ifelse(recent, "red", "pink"), cex=ifelse(recent, 1, 0.7))
    mtext(paste0(" ", abbreviateRegion(region), " / ",
                 format(tail(time,1), "%b %d")),
          cex=par("cex"), adj=0, line=-1)
}
if (!interactive())
    dev.off()
if (!interactive())
    png("canada_log.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)

message("#log plots")
par(mfrow=c(5, 2))
## Uniform scale for all log plots, to make
## it easier to see slope differences.
ylim <- c(1, 0)
for (region in regions) {
    sub <- ds[ds$state == region, ]
    ylim[2] <- max(ylim[2], 1.5 * max(sub$confirmed))
}
for (region in regions) {
    message("  ", region)
    sub <- ds[ds$state == region, ]
    sub$time <- lubridate::with_tz(as.POSIXct(sub$date), tz="UTC")
    time <- sub$time
    num <- sub$confirmed
    deaths <- sub$deaths
    recent <- abs(as.numeric(now) - as.numeric(time)) <= recentNumberOfDays * 86400
    if (any(num > 0)) {
        positive <- is.finite(num) & num > 0
        oce.plot.ts(time[positive], num[positive],
                    mar=mar,
                    ylab="Cases & Deaths", xlim=tlim,
                    type="p", pch=20, col=ifelse(recent[positive], "black", "gray"),
                    cex=ifelse(recent, 1, 0.7),
                    ylim=ylim, log="y", logStyle="decade",
                    drawTimeRange=FALSE)
        if (any(deaths[is.finite(deaths)] > 0))
            points(time, deaths, pch=20, col=ifelse(recent, "red", "pink"), cex=ifelse(recent, 1, 0.7))
        y <- num[recent]
        ok <- y > 0
        x <- (as.numeric(time)[recent])[ok]
        y <- log10(num[recent][ok])
        canFit <- length(x) > 3
        if (canFit) {
            m <- lm(y ~ x)
            xx <- seq(par("usr")[1], par("usr")[2], length.out=100)
            lines(xx, 10^predict(m, list(x=xx)))
            growthRate <- coef(m)[2] * 86400 # in days
            doubleTime <- log10(2) / growthRate
            mtext(sprintf(" Doubling time: %.1fd", doubleTime), side=3, adj=0, line=-2, cex=par("cex"))
        }
    } else {
        plot(0:1, 0:1, xlab="", ylab="", type="n")
        text(0.5, 0.5, "No counts")
    }
    mtext(paste0(" ", abbreviateRegion(region), " / ",
                 format(tail(time,1), "%b %d")),
          cex=par("cex"), adj=0, line=-1)
}
if (!interactive())
    dev.off()

if (!interactive())
    png("canada_change.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)

message("# daily change")
par(mfrow=c(5, 2))
for (region in regions) {
    message("  ", region)
    sub <- ds[ds$state == region, ]
    sub$time <- lubridate::with_tz(as.POSIXct(sub$date), tz="UTC")
    time <- sub$time
    y <- sub$confirmed_new
    oce.plot.ts(time, y,
                xlim=tlim, type="p", drawTimeRange=FALSE, col="gray",
                mar=mar,
                pch=20, cex=par("cex") * ifelse(y==0, 0.25, 1),
                xlab="Time", ylab="Daily Change")
    ## spline with df proportional to data length (the 7 is arbitrary)
    ok <- is.finite(y)
    lines(smooth.spline(time[ok], y[ok], df=length(y)/7), col="magenta")
    recent <- abs(as.numeric(now) - as.numeric(time)) <= recentNumberOfDays * 86400
    points(time[recent], y[recent], pch=20, cex=par("cex"))
    mtext(paste0(" ", abbreviateRegion(region), " / ",
                 format(tail(time,1), "%b %d")),
          cex=par("cex"), adj=0, line=-1)
}

if (!interactive())
    dev.off()
