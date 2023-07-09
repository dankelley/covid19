url <- "https://health-infobase.canada.ca/src/data/covidLive/wastewater/covid19-wastewater.csv"
library(oce)
tsmooth <- 3 # days used in computing spline df
downloadCache <- function(url, days=7)
{
    file <- gsub(".*/", "", url)
    if (file.exists(file)) {
        if (file.mtime(file) < Sys.time() - days*86400)
            download.file(url, file)
        else
            cat("using cached file", file, "\n")
    } else {
        download.file(url, file)
    }
    file
}
file <- downloadCache(url, 1)

d <- read.csv(file)
d$time <- as.POSIXct(d$Date, tz="UTC")

locations <- unique(d$Location)
tlim <- range(d$time)
for (l in sort(locations)) {
    dd <- subset(d, Location==l)
    t <- as.POSIXct(dd$Date)
    y <- dd$viral_load
    png(paste0("wastewater_", l, ".png"),
        unit="in", width=7, height=2.5, res=100)
    oce.plot.ts(t, y, type="l", col="gray",
        drawTimeRange=FALSE, xlim=tlim, grid=TRUE, ylab="Viral Load")
    nx <- length(unique(t))
    df <- max(nx/tsmooth, 10)
    cat("plotting ", l, " (df=", df, ", nx=", nx, ")\n", sep="")
    sp <- smooth.spline(t, y, df=df)
    smoothed <- predict(sp)
    lines(smoothed$x, smoothed$y, col=2, lwd=2)
    mtext(l, adj=1)
    dev.off()
}

