# CHANGES:
##
## 2020-04-28
## World, USA and Canada plots no longer us the COVID19 package.  That package
## is now used only for the test/population plot.  The reason for this change is
## to increase processing speed, and to reduce the effort involved in keeping
## up with changes to the COVID19 package.  Rather than sift through my old
## code that was written before my trial with the COVID19 package, I
## started from scratch, writing get_data.R for the setup work.
##
## 2020-04-15
## Changes to the COVID19 package dictate the following changes:
##     1. The code now examines the most recent 'confirmed' count, to be sure that
##        it is not very different from the previous day.  (I reported
##        this as https://github.com/emanuele-guidotti/COVID19/issues/4)
##     2. The confirmed_new column has disappeared, so we now compute that.
##     3. We use covid19() instead of world().
##     4. The following changes to country names had to be made (meaning changes
##        in the Makefile and the index.html file).
##         * Burma -> Myanmar
##         * Cabo Verde -> Cape Verde
##         * Congo (Brazzaville) -> Congo
##         * Congo (Kinshasa) -> Congo, the Democratic Republic of the
##         * Czechia -> Czech Republic
##         * Eswatini -> removed, since I could not guess a new name
##         * North Macedonia -> Macedonia
##         * US -> United States
##         * West Bank and Gaza -> removed, since I could not guess a new name


library(oce)
source("get_data.R")

recentNumberOfDays <- 10
## can specify region in the commandline
args <- commandArgs(trailingOnly=TRUE)
regions <- if (length(args)) args else "World"
regions <- if (length(args)) args else "United States"
regions <- if (length(args)) args else "Canada"
regions <- if (length(args)) args else "Congo (Kinshasa)"
regions <- if (length(args)) args else "US"
regions <- if (length(args)) args else "World"

now <- lubridate::with_tz(Sys.time(), "UTC")
mar <- c(2, 3, 1.5, 1.5)
tlim <- c(as.POSIXct("2020-01-15", format="%Y-%m-%d", tz="UTC"), now)
colDeath <- "red"

for (region in regions) {
    message("handling ", region)
    if (region == "World") {
        message("FIXME: rewrite this")
        sub <- getData(region)
        sub$cases_new <- c(0, diff(sub$cases))
    } else {
        sub <- getData(region)
        sub$cases_new <- c(0, diff(sub$cases))
    }
    sub$cases_new[sub$cases_new < 0] <- NA
    n <- length(sub$cases)
    if (n < 2) {
        cat("Under 2 data points for", region, "so it is not plotted\n")
        next
    }
    subOrig <- sub
    SD <- sd(tail(head(sub$cases,-1), 7))
    if (abs(sub$cases[n] - sub$cases[n-1]) > 2 * SD) {
        message("dropping most recent point (",
               sub$cases[n], ") since it differs from previous by ",
                round(abs(sub$cases[n] - sub$cases[n-1])),
                ", more than 2* previous recent std-dev of ", round(SD))
        sub$time <- sub$time[seq(1, n-1)]
        sub$cases <- sub$cases[seq(1, n-1)]
        sub$cases_new <- sub$cases_new[seq(1, n-1)]
        sub$deaths <- sub$deaths[seq(1, n-1)]
    }
    lastTime <- tail(sub$time, 1)
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    if (!sum(recent))
        next

    if (!interactive()) png(paste0("covid19_", region, ".png"),
                            width=7, height=5, unit="in", res=120, pointsize=11)
    if (!any(sub$cases > 0)) {
        par(mfrow=c(1,1))
        plot(c(0, 1), c(0, 1), xlab="", ylab="", axes=FALSE, type="n")
        box()
        text(0.5, 0.5, paste0("No data are available for '", region, "';\nplease report this to dan.kelley@dal.ca"))
        next
    }
    par(mfrow=c(2,2))

    ## Cases, linear axis
    oce::oce.plot.ts(sub$time, sub$cases,
                     xaxs="i", xlim=tlim,
                     type="p",
                     pch=20,
                     col=ifelse(recent, "black", "gray"),
                     cex=par("cex"),
                     xlab="Time",
                     ylab="Cumulative Case Count",
                     mar=mar,
                     drawTimeRange=FALSE)
    points(sub$time, sub$cases,
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
    mtext(sprintf("Confirmed: %d (%.4f%%); deaths: %d (%.4f%%)",
                  tail(sub$cases, 1),
                  100*tail(sub$cases,1)/sub$pop[1],
                  tail(sub$deaths, 1),
                  100*tail(sub$deaths, 1)/sub$pop[1]),
                  side=3,
          cex=0.9*par("cex"))

    ## Cases, log axis
    ylim <- c(1, 2*max(sub$cases, na.rm=TRUE))
    positive <- sub$cases > 0
    oce::oce.plot.ts(sub$time[positive], sub$cases[positive], log="y", logStyle="decade",
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
    mtext(paste(format(tail(sub$time,1), "%b %d")), adj=1, cex=0.9*par("cex"))
    points(sub$time[positive], sub$cases[positive],
           pch=20,
           col=ifelse(recent[positive], "black", "gray"),
           cex=par("cex"))
    points(sub$time, sub$deaths, pch=20, col=ifelse(recent, "red", "pink"), cex=par("cex"))
    ## Case doubling time
    x <- as.numeric(sub$time[recent])
    y <- log10(sub$cases[recent])
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]
    y <- y[ok]
    canFit <- length(x) > 3
    if (canFit) {
        m <- lm(y ~ x)
        growthRate <- coef(m)[2] * 86400 # in days
        t2 <- log10(2) / growthRate
        if (0 < t2 && t2 < 100) {
            abline(m, lty="dotted")
            mtext(sprintf(" cases double in %.0fd", t2), adj=0, side=3, line=-1, cex=par("cex"))
        }
    }
    ## Death doubling time
    x <- as.numeric(sub$time[recent])
    y <- log10(sub$deaths[recent])
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]
    y <- y[ok]
    canFit <- length(x) > 3
    if (canFit) {
        m <- lm(y ~ x)
        growthRate <- coef(m)[2] * 86400 # in days
        t2 <- log10(2) / growthRate
        if (0 < t2 && t2 < 100) {
            abline(m, col=colDeath, lty="dotted")
            mtext(sprintf(" deaths double in %.0fd", t2), adj=0, side=3, line=-2, col=colDeath, cex=par("cex"))
        }
    }


    ## Daily change
    y <- sub$cases_new
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
    canSpline <- is.finite(y)
    splineModel <- smooth.spline(sub$time[canSpline], y[canSpline], df=length(y)/7)
    lines(splineModel, col="magenta")

    positive <- y > 0
    oce::oce.plot.ts(sub$time[positive], y[positive], log="y", logStyle="decade",
                     xlim=tlim,
                     type="p",
                     pch=20,
                     col=ifelse(recent[positive], "black", "gray"),
                     cex=par("cex"),
                     xlab="Time",
                     ylab="Daily Case Count",
                     mar=mar,
                     drawTimeRange=FALSE)
    lines(splineModel$x[positive], splineModel$y[positive], col="magenta")

    if (!interactive()) dev.off()
}
