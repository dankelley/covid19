requireNamespace("oce")                # for decade-format log axis, use github "develop" version
requireNamespace("curl")

recentNumberOfDays <- 10
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
    recent <- abs(as.numeric(now) - as.numeric(confirmed$time)) <= recentNumberOfDays * 86400
    ##print(recent)
    deaths <- acquireCovid19(paste0(base, "/time_series_covid19_deaths_global.csv"), region=region)
    ## recovered <- acquireCovid19(paste0(base, "/time_series_19-covid-Recovered.csv"), region=region)

    if (!interactive()) png(paste0("covid19_", region, ".png"),
                            width=5, height=5, unit="in", res=120, pointsize=11)

    par(mfrow=c(3,1), pch=20, lwd=0.9)

    tlim <- range(confirmed$time)
    oce::oce.plot.ts(confirmed$time, confirmed$data,
                     xlim=tlim,
                     type="o", drawTimeRange=FALSE,
                     pch=20,
                     col=ifelse(recent, "black", "gray"),
                     cex=par("cex") * ifelse(confirmed$data==0, 0.25, 1),
                     xlab="Time", ylab="Case Count", mar=c(2,3,1,1.5))
    mtext(region, adj=0, cex=par("cex"))
    now <- lubridate::with_tz(Sys.time(), "UTC")
    mtext(paste(format(now, "%Y %b %d")), adj=1, cex=par("cex"))
    ##mtext(paste(format(tail(confirmed$time,1), "%Y %b %d")), adj=1, cex=par("cex"))
    points(deaths$time, deaths$data,
           pch=20,
           col=ifelse(recent, "red", "pink"),
           cex=par("cex") * ifelse(confirmed$data==0, 0.25, 1))
    ## points(recovered$time, trimZeros(recovered$data), col="green3")
    ## legend("topleft", pt.cex=1.4, cex=0.9, pch=20, bg="white",
    ##        col=c("gray", "black", "green3", "red"),
    ##        legend=c("Confirmed", "Confirmed", "Recoveries", "Deaths"))
    legend("topleft", pt.cex=1.4, cex=0.9, pch=20, bg="white",
           col=c("black", "red"),
           legend=c("Confirmed", "Deaths"))
    mtext(sprintf("Confirmed: %d; deaths: %d",
                  tail(confirmed$data, 1),
                  tail(deaths$data, 1)), side=3,
          cex=par("cex"))
    ## Log axis
    ylim <- c(1, 2*max(confirmed$data, na.rm=TRUE))
    positive <- confirmed$data > 0
    oce::oce.plot.ts(confirmed$time[positive], confirmed$data[positive],
                     xlim=tlim, ylim=ylim, type="o",
                     log="y", logStyle="decade",
                     pch=20,
                     col=ifelse(recent[positive], "black", "gray"),
                     xlab="Time", ylab="Case Count", mar=c(2, 3, 1, 1.5),
                     drawTimeRange=FALSE)
    ##points(confirmed$time[recent], log10(confirmed$data[recent]), pch=20)
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
    points(deaths$time, deaths$data, pch=20, col=ifelse(recent, "red", "pink"), type="o")

    ## Daily change
    y <- diff(confirmed$data)
    ylim <- c(0, max(y))
    oce::oce.plot.ts(confirmed$time[-1], y,
                     xlim=tlim, type="o", drawTimeRange=FALSE, col="gray",
                     pch=20, cex=par("cex") * ifelse(y==0, 0.25, 1),
                     xlab="Time", ylab="Daily Change", mar=c(2,3,1,1.5))
    ## spline with df proportional to data length (the 7 is arbitrary)
    lines(smooth.spline(confirmed$time[-1], y, df=length(y)/7),
                        col="lightgray")
    y <- diff(confirmed$data[recent])
    points(confirmed$time[recent][-1], diff(confirmed$data[recent]),
           pch=20, cex=par("cex") * ifelse(y==0, 0.25, 1))
    y <- diff(deaths$data)
    points(deaths$time[-1], y, col=ifelse(recent, "red", "pink"),
           pch=20, cex=par("cex") * ifelse(y==0, 0.25, 1))
    if (!interactive()) dev.off()
}

