requireNamespace("oce")
requireNamespace("curl")

maybeDownload <- function(url, file, hours=6)
{
    if (!file.exists(file) || file.info(file)$mtime < Sys.time() - hours * 3600) {
        curl::curl_download(url, file)
    }
}

acquireCovid19 <- function(url, hours=6)
{
    file <- gsub(".*/", "", url)
    maybeDownload(url, file, hour=hours)
    d <- read.csv(file, header=TRUE)
    ## Convert from idiotic US date of month/day/(year-2000) which becomes month.day.(year-2000)
    t <- gsub("X", "", tail(names(d), -4))
    t <- gsub("([0-9]*).([0-9]*).([0-9]*)", "20\\3-\\1-\\2", t)
    t <- as.POSIXct(t, tz="UTC")
    world <- as.vector(mapply(sum, d[, seq(5, dim(d)[2])]))
    list(url=url, time=t, world=world)
}

base <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
confirmed <- acquireCovid19(paste0(base, "/time_series_19-covid-Confirmed.csv"))
deaths <- acquireCovid19(paste0(base, "/time_series_19-covid-Deaths.csv"))
recovered <- acquireCovid19(paste0(base, "/time_series_19-covid-Recovered.csv"))

if (!interactive()) png("covid19.png", width=5, height=5, unit="in", res=150, pointsize=10)
par(mfrow=c(2,1), pch=20)

oce::oce.plot.ts(confirmed$time, confirmed$world, type="p", drawTimeRange=FALSE,
                 xlab="Time", ylab="World-wide Cases", mar=c(2,3,1,1))
mtext("Covid-19 cases in year 2020")
points(deaths$time, deaths$world, col="red", type="b")
points(recovered$time, recovered$world, col="green3")
legend("topleft", lwd=1, pch=20, col=c("black", "red", "green3"), legend=c("Cases", "Deaths", "Recoveries"))

## Kludge  a log y axis, because log="y" yields ugly labels and ticks.
oce::oce.plot.ts(confirmed$time, log10(confirmed$world), type="p", axes=FALSE,
                 xlab="Time", ylab="World-wide Cases", mar=c(2, 3, 1, 1))
oce::oce.axis.POSIXct(side=1, drawTimeRange=FALSE)
box()
powerLow <- floor(1 + par("usr")[3])
powerHigh <- floor(par("usr")[4])
tcl <- par("tcl")
smallTics <- NULL
for (power in powerLow:powerHigh) {
    rug(side=2, x=power, tcl=tcl, lwd=par("lwd"))
    smallTics <- c(smallTics, -1 + power + log10(2:9))
    smallTics <- c(smallTics,      power + log10(2:9))
    rug(side=4, x=power, tcl=tcl, lwd=par("lwd"))
    mtext(substitute(10^A, list(A=power)), side=2, at=power, line=0.5)
    abline(h=power, lty="dotted", col="gray")
}
smallTics <- unique(smallTics[par("usr")[3] < smallTics & smallTics < par("usr")[4]])
rug(side=2, x=smallTics, tcl=0.5*tcl, lwd=par("lwd"))
rug(side=4, x=smallTics, tcl=0.5*tcl, lwd=par("lwd"))


points(deaths$time, log10(deaths$world), col="red", pch=20)
points(recovered$time, log10(recovered$world), col="green3")

if (!interactive()) dev.off()

