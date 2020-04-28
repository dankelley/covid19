showExample <- FALSE
load("population.rda")

## Get data straight from the Johns Hopkins github page, which
## is faster than using COVID19, and less subject to UI change, although
## it has the problem of not having population.
baseUrl <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
confirmedSources <- list(url=paste0(baseUrl, "/time_series_covid19_confirmed_global.csv"),
                         file="time_series_covid19_confirmed_global.csv")
deathsSources <- list(url=paste0(baseUrl, "/time_series_covid19_deaths_global.csv"),
                      file="time_series_covid19_deaths_global.csv")
if (!file.exists(confirmedSources$file)) {
    cat("Download", confirmedSources$file, "for the first time\n")
    download.file(confirmedSources$url, confirmedSources$file)
} else if ((as.numeric(Sys.time()) - as.numeric(file.info(confirmedSources$file)$ctime)) > 2 * 3600) {
    cat("Downloading", confirmedSources$file, "since it is more than 2 hours old\n")
    download.file(confirmedSources$url, confirmedSources$file)
}
if (!file.exists(deathsSources$file)) {
    cat("Download", deathsSources$file, "for the first time\n")
    download.file(deathsSources$url, deathsSources$file)
} else if ((as.numeric(Sys.time()) - as.numeric(file.info(deathsSources$file)$ctime)) > 2 * 3600) {
    cat("Downloading", deathsSources$file, "since it is more than 2 hours old\n")
    download.file(deathsSources$url, deathsSources$file)
}

if (!exists("confirmed")) {
    cat("reading", confirmedSources$file, "\n")
    confirmed <- read.csv(confirmedSources$file, header=TRUE)
}
if (!exists("deaths")) {
    cat("reading", deathsSources$file, "\n")
    deaths <- read.csv(deathsSources$file, header=TRUE)
}
stopifnot(names(confirmed) == names(deaths))
stopifnot(names(deaths)[1:4] == c("Province.State", "Country.Region", "Lat", "Long"))
confirmedNum <- confirmed[, seq(5L, dim(confirmed)[2])]
deathsNum <- deaths[, seq(5L, dim(deaths)[2])]

tmp <- gsub("X", "", names(confirmed)[grep("^X[1-9]\\.", names(confirmed))])
confirmedTimes <- lubridate::with_tz(as.POSIXct(tmp, format="%m.%d.%y", tz="UTC"), "UTC")
tmp <- gsub("X", "", names(deaths)[grep("^X[1-9]\\.", names(deaths))])
deathsTimes <- lubridate::with_tz(as.POSIXct(tmp, format="%m.%d.%y", tz="UTC"), "UTC")
stopifnot(confirmedTimes == deathsTimes)
time <- confirmedTimes

getData <- function(Country.Region="Canada", Province.State=NULL)
{
    if (is.null(Province.State)) {
        sub <- confirmed[confirmed$Country.Region == Country.Region, ]
        cases <- unname(apply(sub[seq(5L, dim(sub)[2])], 2, sum))
        sub <- deaths[deaths$Country.Region == Country.Region, ]
        deaths <- unname(apply(sub[seq(5L, dim(sub)[2])], 2, sum))
        res <- list(time=time,
                    cases=cases,
                    deaths=deaths,
                    population=population[[Country.Region]])
    } else {
        sub <- confirmed[confirmed$Country.Region == Country.Region & confirmed$Province.State == Province.State, ]
        cases <- as.numeric(sub[1, seq(5L, length(sub))])
        sub <- deaths[deaths$Country.Region == Country.Region & deaths$Province.State == Province.State, ]
        deaths <- as.numeric(sub[1, seq(5L, length(sub))])
        res <- list(time=time,
                    cases=cases,
                    deaths=deaths,
                    population=population[[Province.State]])
    }
    res
}

if (showExample) {
    library(oce)
    par(mfrow=c(3, 1))
    cex <- par("cex")
    pch <- 20
    canada <- getData("Canada")
    oce.plot.ts(canada$time, canada$cases, ylab="Canada", drawTimeRange=FALSE, type="p", cex=cex, pch=pch)
    points(canada$time, canada$deaths, col="red", cex=cex, pch=pch)
    mtext(canada$population/1e6)
    NS <- getData("Canada", "Nova Scotia")
    oce.plot.ts(NS$time, NS$cases, ylab="Nova Scotia", drawTimeRange=FALSE, type="p", cex=cex, pch=pch)
    points(NS$time, NS$deaths, col="red", cex=cex, pch=pch)
    mtext(NS$population/1e6)
    place <- "Australia"
    D <- getData(place)
    oce.plot.ts(D$time, D$cases, ylab=place, drawTimeRange=FALSE, type="p", cex=cex, pch=pch)
    points(D$time, D$deaths, col="red", cex=cex, pch=pch)
    mtext(D$population/1e6)
}


