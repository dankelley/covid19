rm(list=ls())
showExample <- FALSE
load("population.rda")

## Get data straight from the Johns Hopkins github page, which
## is faster than using COVID19, and less subject to UI change, although
## it has the problem of not having population.
baseUrl <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"
confirmedSources <- list(url=paste0(baseUrl, "/time_series_covid19_confirmed_global.csv"),
                         file="time_series_covid19_confirmed_global.csv")
# https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv
confirmedSourcesUS <- list(url=paste0(baseUrl, "/time_series_covid19_confirmed_US.csv"),
                           file="time_series_covid19_confirmed_US.csv")
deathsSources <- list(url=paste0(baseUrl, "/time_series_covid19_deaths_global.csv"),
                      file="time_series_covid19_deaths_global.csv")
deathsSourcesUS <- list(url=paste0(baseUrl, "/time_series_covid19_deaths_US.csv"),
                        file="time_series_covid19_deaths_US.csv")


## confirmed
if (!file.exists(confirmedSources$file)) {
    cat("Download", confirmedSources$file, "for the first time\n")
    download.file(confirmedSources$url, confirmedSources$file)
} else if ((as.numeric(Sys.time()) - as.numeric(file.info(confirmedSources$file)$ctime)) > 2 * 3600) {
    cat("Downloading", confirmedSources$file, "since it is more than 2 hours old\n")
    download.file(confirmedSources$url, confirmedSources$file)
}
if (!file.exists(confirmedSourcesUS$file)) {
    cat("Download", confirmedSourcesUS$file, "for the first time\n")
    download.file(confirmedSourcesUS$url, confirmedSourcesUS$file)
} else if ((as.numeric(Sys.time()) - as.numeric(file.info(confirmedSourcesUS$file)$ctime)) > 2 * 3600) {
    cat("Downloading", confirmedSourcesUS$file, "since it is more than 2 hours old\n")
    download.file(confirmedSourcesiUS$url, confirmedSourcesus$file)
}

## deaths
if (!file.exists(deathsSources$file)) {
    cat("Download", deathsSources$file, "for the first time\n")
    download.file(deathsSources$url, deathsSources$file)
} else if ((as.numeric(Sys.time()) - as.numeric(file.info(deathsSources$file)$ctime)) > 2 * 3600) {
    cat("Downloading", deathsSources$file, "since it is more than 2 hours old\n")
    download.file(deathsSources$url, deathsSources$file)
}
if (!file.exists(deathsSourcesUS$file)) {
    cat("Download", deathsSourcesUS$file, "for the first time\n")
    download.file(deathsSourcesUS$url, deathsSourcesUS$file)
} else if ((as.numeric(Sys.time()) - as.numeric(file.info(deathsSourcesUS$file)$ctime)) > 2 * 3600) {
    cat("Downloading", deathsSourcesUS$file, "since it is more than 2 hours old\n")
    download.file(deathsSourcesUS$url, deathsSourcesUS$file)
}

if (!exists("confirmed")) {
    cat("reading", confirmedSources$file, "\n")
    confirmed <- read.csv(confirmedSources$file, header=TRUE)
}
if (!exists("confirmedUS")) {
    cat("reading", confirmedSourcesUS$file, "\n")
    confirmedUS <- read.csv(confirmedSourcesUS$file, header=TRUE)
}
if (!exists("deaths")) {
    cat("reading", deathsSources$file, "\n")
    deaths <- read.csv(deathsSources$file, header=TRUE)
}
if (!exists("deathsUS")) {
    cat("reading", deathsSourcesUS$file, "\n")
    deathsUS <- read.csv(deathsSourcesUS$file, header=TRUE)
}

stopifnot(names(confirmed) == names(deaths))
stopifnot(names(deaths)[1:4] == c("Province.State", "Country.Region", "Lat", "Long"))
names(deaths)[1:4]
confirmedNum <- confirmed[, seq(5L, dim(confirmed)[2])]
deathsNum <- deaths[, seq(5L, dim(deaths)[2])]

#stopifnot(names(confirmedUS) == names(deathsUS))

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
        if (Country.Region == "United States" || Country.Region == "US" || Country.Region == "USA") {
            tmp <- confirmedUS[confirmedUS$Province_State == Province.State, ]
            head(names(tmp), 15)
            tmpData <- as.matrix(tmp[, 12:dim(tmp)[2]])
            cases <- apply(tmpData, 2, sum)
            tmp <- deathsUS[deathsUS$Province_State == Province.State, ]
            head(names(tmp), 15)
            population <- sum(tmp$Population)
            tmpData <- as.matrix(tmp[, 13:dim(tmp)[2]])
            deaths <- apply(tmpData, 2, sum)
            res <- list(time=time,
                        cases=unname(cases),
                        deaths=unname(deaths),
                        population=population)
 
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
    }
    res
}

if (showExample) {
    library(oce)
    par(mfrow=c(2, 2))
    cex <- par("cex")
    pch <- 20
    d <- getData("Canada")
    oce.plot.ts(d$time, d$cases, ylab="Canada", drawTimeRange=FALSE, type="p", cex=cex, pch=pch)
    points(d$time, d$deaths, col="red", cex=cex, pch=pch)
    mtext(paste("Population:", round(d$population/1e6,2), "million"), cex=par("cex"))

    d <- getData("Canada", "Nova Scotia")
    oce.plot.ts(d$time, d$cases, ylab="Nova Scotia", drawTimeRange=FALSE, type="p", cex=cex, pch=pch)
    points(d$time, d$deaths, col="red", cex=cex, pch=pch)
    mtext(paste("Population:", round(d$population/1e6,2), "million"), cex=par("cex"))

    place <- "Australia"
    d <- getData(place)
    oce.plot.ts(d$time, d$cases, ylab=place, drawTimeRange=FALSE, type="p", cex=cex, pch=pch)
    points(d$time, d$deaths, col="red", cex=cex, pch=pch)
    mtext(paste("Population:", round(d$population/1e6,2), "million"), cex=par("cex"))
    ## US case
    d <- getData("United States", "Ohio")
    oce.plot.ts(d$time, d$cases, ylab="Ohio", type="p", drawTimeRange=FALSE, cex=cex, pch=pch)
    points(d$time, d$deaths, col=2, cex=cex, pch=pch)
    mtext(paste("Population:", round(d$population/1e6,2), "million"), cex=cex)
}
