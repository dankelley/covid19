library(oce)
source("~/git/oce/R/oce.R")
source("~/git/oce/R/misc.R")
source("~/git/oce/R/imagep.R")

file <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
if (!exists("d")) d <- read.csv(file, header=TRUE) else cat("using cached data\n")

sort(names(d))
vaccineStart <- as.POSIXct("2020-12-10")
now <- Sys.Date()
d$time <- as.POSIXct(d$date)
print(c(vaccineStart, now))
par(mfcol=c(2,2))
for (location in c("Canada", "United States")) {
    placeOK <- d$location == location
    timeOK <- d$time > vaccineStart
    dataOK <- is.finite(d$total_vaccinations_per_hundred) & d$total_vaccinations_per_hundred > 0
    dlook <- d[placeOK & timeOK & dataOK, ]
    if (nrow(dlook)) {
        day <- (dlook$time - dlook$time[1]) / 86400
        v100 <- dlook$total_vaccinations_per_hundred
        m <- lm(v100 ~ day)
        oce.plot.ts(dlook$time, dlook$total_vaccinations_per_hundred, xlab="", ylab="Vaccines/100", drawTimeRange=FALSE, grid=TRUE, xlim=c(vaccineStart, now))
        mtext(location, adj=1)
        mtext(sprintf("%.3f v100/d", coef(m)[2]), adj=0, line=-1)
        oce.plot.ts(dlook$time, dlook$total_vaccinations_per_hundred, xlab="", ylab="Vaccines/100", log="y", logStyle="decade", drawTimeRange=FALSE, grid=TRUE, xlim=c(vaccineStart, now))
    } else {
        warning("'", location, "' not found\n")
    }
}

