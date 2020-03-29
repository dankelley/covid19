requireNamespace("oce")
requireNamespace("curl")

now <- Sys.time()
## can specify region in the commandline
args <- commandArgs(trailingOnly=TRUE)
regions <- if (length(args)) args else "Canada"

maybeDownload <- function(url, file, hours=1)
{
    if (!file.exists(file) || file.info(file)$mtime < Sys.time() - hours * 3600) {
        message("downloading url ", url)
        curl::curl_download(url, file)
    }
}

acquireCovid19 <- function(url, hours=1, region="World")
{
    file <- gsub(".*/", "", url)
    maybeDownload(url, file, hour=hours)
    d <- read.csv(file, header=TRUE)
    ## Convert from idiotic US date of month/day/(year-2000) which becomes month.day.(year-2000)
    t <- gsub("X", "", tail(names(d), -4))
    t <- gsub("([0-9]*).([0-9]*).([0-9]*)", "20\\3-\\1-\\2", t)
    t <- as.POSIXct(t, tz="UTC")
    regionAllowed <- sort(unique(as.character(d$Country.Region)))
    if (region == "World") {
        data <- as.vector(mapply(sum, d[, seq(5, dim(d)[2])]))
    } else if (region %in% regionAllowed) {
        d <- subset(d, Country.Region == region)
        data <- as.vector(mapply(sum, d[, seq(5, dim(d)[2])]))
    } else {
        cat("only works for region=\"World\" or \"", paste(regionAllowed, collapse="\", \""), "\"\n", sep="")
        stop("invalid region \"", region, "\"")
    }
    list(url=url, time=t, data=data, region=region)
}

trimZeros <- function(x) {
    x[x==0] <- NA
    x
}

base <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

for (region in regions) {
    message("handling ", region)
    confirmed <- acquireCovid19(paste0(base, "/time_series_covid19_confirmed_global.csv"), region=region)
    lastTime <- tail(confirmed$time, 1)
    recent <- abs(as.numeric(now) - as.numeric(confirmed$time)) < 7 * 86400
    ##print(recent)
    deaths <- acquireCovid19(paste0(base, "/time_series_covid19_deaths_global.csv"), region=region)
    ## recovered <- acquireCovid19(paste0(base, "/time_series_19-covid-Recovered.csv"), region=region)

    if (!interactive()) png(paste0("covid19_", region, ".png"),
                            width=5, height=5, unit="in", res=120, pointsize=11)

    par(mfrow=c(3,1), pch=20)

    oce::oce.plot.ts(confirmed$time, confirmed$data, type="o", drawTimeRange=FALSE, col="gray",
                     xlab="Time", ylab="Case Count", mar=c(2,3,1,1.5))
    points(confirmed$time[recent], confirmed$data[recent], pch=20, col="black")
    mtext(region, adj=0, cex=par("cex"))
    now <- lubridate::with_tz(Sys.time(), "UTC")
    ##mtext(paste("Graph updated", format(now, "%Y %b %d (%H:%M %Z)")), adj=1, cex=0.9)
    mtext(paste(format(now, "%Y %b %d")), adj=1, cex=par("cex"))
    points(deaths$time, trimZeros(deaths$data), col="red", type="o")
    ## points(recovered$time, trimZeros(recovered$data), col="green3")
    ## legend("topleft", pt.cex=1.4, cex=0.9, pch=20, bg="white",
    ##        col=c("gray", "black", "green3", "red"),
    ##        legend=c("Confirmed", "Confirmed", "Recoveries", "Deaths"))
    legend("topleft", pt.cex=1.4, cex=0.9, pch=20, bg="white",
           col=c("gray", "black", "red"),
           legend=c("Confirmed", "Confirmed", "Deaths"))
    mtext(sprintf("Confirmed: %d; deaths: %d",
                  tail(confirmed$data, 1),
                  tail(deaths$data, 1)), side=3,
          cex=par("cex"))
    ## Kludge  a log y axis, because log="y" yields ugly labels and ticks.
    y <- log10(confirmed$data)
    y[!is.finite(y)] <- NA
    ylim <- c(0, 1.04*max(y, na.rm=TRUE))
    oce::oce.plot.ts(confirmed$time, y, ylim=ylim, type="o", axes=FALSE, col="gray",
                     xlab="Time", ylab="Case Count", mar=c(2, 3, 1, 1.5))
    oce::oce.axis.POSIXct(side=1, drawTimeRange=FALSE)
    box()
    powerLow <- floor(1 + par("usr")[3])
    powerHigh <- floor(par("usr")[4])
    tcl <- par("tcl")
    smallTics <- NULL
    ats <- NULL
    labels <- NULL
    for (power in powerLow:powerHigh) {
        rug(side=2, x=power, tcl=tcl, lwd=par("lwd"))
        smallTics <- c(smallTics, -1 + power + log10(2:9))
        smallTics <- c(smallTics,      power + log10(2:9))
        rug(side=4, x=power, tcl=tcl, lwd=par("lwd"))
        if (power < 2L) {
            ## mtext(10^power, side=2, at=power, line=0.5, cex=par("cex"))
            labels <- c(labels, as.expression(10^power))
        } else {
            ## mtext(substitute(10^A, list(A=power)), side=2, at=power, line=0.5, cex=par("cex"))
            labels <- c(labels, substitute(10^A, list(A=power)))
        }
        ats <- c(ats, power)
        abline(h=power, lty="dotted", col="lightgray")
    }
    axis(side=2, labels=labels, at=ats)
    smallTics <- unique(smallTics[par("usr")[3] < smallTics & smallTics < par("usr")[4]])
    rug(side=2, x=smallTics, tcl=0.5*tcl, lwd=par("lwd"))
    rug(side=4, x=smallTics, tcl=0.5*tcl, lwd=par("lwd"))

    points(confirmed$time[recent], log10(confirmed$data[recent]), pch=20)
    x <- as.numeric(confirmed$time[recent])
    y <- log10(confirmed$data[recent])
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
    points(deaths$time, log10(deaths$data), col="red", pch=20, type="o")
    ## points(recovered$time, log10(recovered$data), col="green3")
    ## Skip legend in bottom panel
    ##> legend("topleft", pt.cex=1.4, cex=0.9, pch=20, bg="white",
    ##>        col=c("gray", "black", "green3", "red"),
    ##>        legend=c("Confirmed", "Confirmed", "Recoveries", "Deaths"))

    oce::oce.plot.ts(confirmed$time[-1], diff(confirmed$data), type="o", drawTimeRange=FALSE, col="gray",
                     xlab="Time", ylab="Daily Increase", mar=c(2,3,1,1.5))
    points(confirmed$time[recent][-1], diff(confirmed$data[recent]), pch=20)
    points(deaths$time[-1], diff(deaths$data), col="red", pch=20, type="o")

    if (!interactive()) dev.off()
}

