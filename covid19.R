library(COVID19)
library(oce)

recentNumberOfDays <- 10
## can specify region in the commandline
args <- commandArgs(trailingOnly=TRUE)
regions <- if (length(args)) args else "Canada"
#regions <- if (length(args)) args else "China"

if (!exists("ds")) # cache to save server load during code development
    ds <- world("country")
#sink("names.md");names(ds);sink()
# [1] "id"             "date"           "country"        "state"          "city"           "lat"            "lng"
# [8] "deaths"         "confirmed"      "tests"          "deaths_new"     "confirmed_new"  "tests_new"      "pop"
#[15] "pop_14"         "pop_15_64"      "pop_65"         "pop_age"        "pop_density"    "pop_death_rate"

trimZeros <- function(x)
{
    x[x==0] <- NA
    x
}

## Construct world
A <- split(ds, ds$date)
dateWorld <- names(lapply(A, function(x) x$date[[1]]))
tlim <- range(as.POSIXct(dateWorld, tz="UTC"))
confirmedWorld <- unlist(lapply(A, function(x) sum(x$confirmed)))
deathsWorld <- unlist(lapply(A, function(x) sum(x$deaths)))
now <- lubridate::with_tz(Sys.time(), "UTC")
mar <- c(2, 3, 1.5, 1.5)

for (region in regions) {
    message("handling ", region)
    if (region == "World") {
        sub <- list(date=dateWorld,
                    confirmed=confirmedWorld,
                    confirmed_new=c(0, diff(confirmedWorld)),
                    deaths=deathsWorld,
                    pop=7776617876)
    } else {
        sub <- ds[ds$country == region, ]
    }
    sub$time <- lubridate::with_tz(as.POSIXct(sub$date), "UTC")
    lastTime <- tail(sub$time, 1)
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    if (!sum(recent))
        next

    if (!interactive()) png(paste0("covid19_", region, ".png"),
                            width=7, height=5, unit="in", res=120, pointsize=11)
    par(mfrow=c(2,2))

    ## Cases, linear axis
    oce::oce.plot.ts(sub$date, sub$confirmed,
                     xlim=tlim,
                     type="p",
                     pch=20,
                     col=ifelse(recent, "black", "gray"),
                     cex=par("cex"),
                     xlab="Time",
                     ylab="Cumulative Case Count",
                     mar=mar,
                     drawTimeRange=FALSE)
    points(sub$time, sub$confirmed,
           pch=20,
           col=ifelse(recent, "black", "gray"),
           cex=par("cex"))
    points(sub$time, sub$deaths,
           pch=20,
           col=ifelse(recent, "red", "pink"),
           cex=par("cex"))
    legend("topleft", pt.cex=1, cex=0.8, pch=20,
           col=c("black", "red"),
           legend=c("Confirmed", "Deaths"),
           title=region)
    mtext(sprintf("Confirmed: %d (%5.3g%%); deaths: %d (%5.3g%%)",
                  tail(sub$confirmed, 1),
                  100*tail(sub$confirmed,1)/sub$pop[1],
                  tail(sub$deaths, 1),
                  100*tail(sub$deaths, 1)/sub$pop[1]),
                  side=3,
          cex=0.9*par("cex"))

    ## Cases, log axis
    ylim <- c(1, 2*max(sub$confirmed, na.rm=TRUE))
    positive <- sub$confirmed > 0
    oce::oce.plot.ts(sub$time[positive], sub$confirmed[positive], log="y", logStyle="decade",
                     xlim=tlim,
                     ylim=ylim,
                     type="p",
                     pch=20,
                     col=ifelse(recent, "black", "gray"),
                     cex=par("cex"),
                     xlab="Time",
                     ylab="Cumulative Case Count",
                     mar=mar,
                     drawTimeRange=FALSE)
    mtext(paste(format(now, "%Y %b %d")), adj=1, cex=0.9*par("cex"))
    points(sub$time[positive], sub$confirmed[positive],
           pch=20,
           col=ifelse(recent[positive], "black", "gray"),
           cex=par("cex"))
    x <- as.numeric(sub$time[recent])
    y <- log10(sub$confirmed[recent])
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]
    y <- y[ok]
    canFit <- length(x) > 3
    if (canFit) {
        m <- lm(y ~ x)
        abline(m)
        growthRate <- coef(m)[2] * 86400 # in days
        doubleTime <- log10(2) / growthRate
        mtext(sprintf("Doubling time: %.1fd", doubleTime), side=3, line=-1, cex=0.9*par("cex"))
    }
    points(sub$time, sub$deaths,
           pch=20,
           col=ifelse(recent, "red", "pink"),
           cex=par("cex"))

    ## Daily change
    y <- sub$confirmed_new
    ylim <- c(0, max(y))
    oce::oce.plot.ts(sub$time, y,
                     xlim=tlim,
                     type="p",
                     pch=20,
                     col=ifelse(recent, "black", "gray"),
                     cex=par("cex"),
                     xlab="Time",
                     ylab="Daily Case Count",
                     mar=mar,
                     drawTimeRange=FALSE)
    ## spline with df proportional to data length (the 7 is arbitrary)
    points(sub$time, y,
           pch=20,
           col=ifelse(recent, "black", "gray"),
           cex=par("cex"))
    splineModel <- smooth.spline(sub$time, y, df=length(y)/7)
    lines(splineModel, col="magenta")

    oce::oce.plot.ts(sub$date, y+1, log="y", logStyle="decade",
                     xlim=tlim,
                     type="p",
                     pch=20,
                     col=ifelse(recent, "black", "gray"),
                     cex=par("cex"),
                     xlab="Time",
                     ylab="1 + Daily Case Count",
                     mar=mar,
                     drawTimeRange=FALSE)
    lines(splineModel, col="magenta")

    if (!interactive()) dev.off()
}
