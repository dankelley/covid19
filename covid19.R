library(COVID19)
library(oce)

recentNumberOfDays <- 10
now <- Sys.time()
## can specify region in the commandline
args <- commandArgs(trailingOnly=TRUE)
regions <- if (length(args)) args else "World"

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
    sub$time <- as.POSIXct(sub$date)
    lastTime <- tail(sub$time, 1)
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    if (sum(recent) < 3)
        stop("error: under 3 recent data. Is the data stream broken?")
    ##print(recent)
    deaths <- sub$deaths
    ## recovered <- acquireCovid19(paste0(base, "/time_series_19-covid-Recovered.csv"), region=region)

    if (!interactive()) png(paste0("covid19_", region, ".png"),
                            width=5, height=5, unit="in", res=120, pointsize=11)

    par(mfrow=c(3,1), pch=20, lwd=0.9)

    ##tlim <- range(sub$time)
    oce::oce.plot.ts(sub$date, sub$confirmed, xlim=tlim, type="l", col="gray",
                     drawTimeRange=FALSE, xlab="Time", ylab="Case Count", mar=c(2,3,1,1.5))
    points(sub$time, sub$confirmed,
           pch=20,
           col=ifelse(recent, "black", "gray"),
           cex=par("cex") * ifelse(sub$deaths==0, 0.25, 1))
     mtext(region, adj=0, cex=par("cex"))
    now <- lubridate::with_tz(Sys.time(), "UTC")
    mtext(paste(format(now, "%Y %b %d")), adj=1, cex=par("cex"))
    points(sub$time, sub$deaths,
           pch=20,
           col=ifelse(recent, "red", "pink"),
           cex=par("cex") * ifelse(sub$deaths==0, 0.25, 1))
    legend("topleft", pt.cex=1.4, cex=0.9, pch=20, bg="white",
           col=c("black", "red"),
           legend=c("Confirmed", "Deaths"))
    mtext(sprintf("Confirmed: %d (%.2g%%); deaths: %d (%.2g%%)",
                  tail(sub$confirmed, 1),
                  100*tail(sub$confirmed,1)/sub$pop[1],
                  tail(sub$deaths, 1),
                  100*tail(sub$deaths, 1)/sub$pop[1]),
                  side=3,
          cex=par("cex"))
    ## Log axis
    ylim <- c(1, 2*max(sub$confirmed, na.rm=TRUE))
    positive <- sub$confirmed > 0
    oce::oce.plot.ts(sub$time[positive], sub$confirmed[positive],
                     xlim=tlim, ylim=ylim, type="o",
                     log="y", logStyle="decade",
                     pch=20,
                     col=ifelse(recent[positive], "black", "gray"),
                     xlab="Time", ylab="Case Count", mar=c(2, 3, 1, 1.5),
                     drawTimeRange=FALSE)
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
        mtext(sprintf("Doubling time: %.1fd", doubleTime), side=3, adj=1, cex=par("cex"))
    }
    points(sub$time, sub$deaths, pch=20, col=ifelse(recent, "red", "pink"), type="o")

    ## Daily change
    y <- sub$confirmed_new
    ylim <- c(0, max(y))
    oce::oce.plot.ts(sub$time, y,
                     xlim=tlim, type="o", drawTimeRange=FALSE, col="gray",
                     pch=20, cex=par("cex") * ifelse(y==0, 0.25, 1),
                     xlab="Time", ylab="Daily Change", mar=c(2,3,1,1.5))
    ## spline with df proportional to data length (the 7 is arbitrary)
    lines(smooth.spline(sub$time, y, df=length(y)/7), col="magenta")
    points(sub$time[recent], sub$confirmed_new[recent],
           pch=20, cex=par("cex") * ifelse(y==0, 0.25, 1))
    if (!interactive()) dev.off()
}

