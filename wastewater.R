library(oce)
tsmooth <- 14 # days used in computing spline df
downloadCache <- function(url, days=1)
{
    file <- gsub(".*/", "", url)
    if (file.exists(file)) {
        mtime <- file.mtime(file)
        if (mtime < Sys.time() - days*86400)
            download.file(url, file)
        else cat("using cached file", file, "\n")
    } else {
        download.file(url, file)
    }
    file
}
file <- downloadCache("https://health-infobase.canada.ca/src/data/covidLive/wastewater/covid19-wastewater.csv", 1)

d <- read.csv(file)
d$time <- as.POSIXct(d$Date, tz="UTC")
dt <- as.numeric(difftime(max(d$time), min(d$time), units="days"))
df <- dt / tsmooth

regions <- unique(d$region)
tlim <- range(d$time)
for (r in regions) {
    cat("plotting wastewater for", r, "\n")
    dr <- subset(d, region==r)
    t <- dr$time
    y <- dr$viral_load
    png(paste0("wastewater_", r, ".png"),
        unit="in", width=7, height=2.5, res=100)
    oce.plot.ts(t, y, type="p", pch=20, col="gray",
        drawTimeRange=FALSE, xlim=tlim, grid=TRUE, ylab="Viral Load")
    sp <- smooth.spline(t, y, df=df)
    smoothed <- predict(sp)
    lines(smoothed$x, smoothed$y, col=2, lwd=2)
    mtext(r, adj=1)
    dev.off()
}

