library(oce)
url <- "https://health-infobase.canada.ca/src/data/covidLive/wastewater/covid19-wastewater.csv"
file <- gsub(".*/", "", url)
if (file.exists(file)) {
    mtime <- file.mtime(file)
    if (mtime < Sys.time() - 12*3600)
        download.file(url, file)
    else cat("using cached file", file, "\n")
} else {
    download.file(url, file)
}
d <- read.csv(file)
dr <- subset(d, region=="Halifax")
t <- as.POSIXct(dr$Date, tz="UTC")
viralLoad <- dr$viral_load
png("wastewater_halifax.png", unit="in", width=7, height=3, res=100)
oce.plot.ts(t, viralLoad, type="p", pch=20, col="gray",
    grid=TRUE, ylab="Viral Load")
sp <- smooth.spline(t, viralLoad, df=length(t)/30)
smoothed <- predict(sp)
lines(smoothed$x, smoothed$y, col=2, lwd=2)
dev.off()

