message("ONT is broken. It jumps from 50-ish% to 100-ish% in an instant")

max <- 65
days <- 600
library(oce)
library(jsonlite)
par(mfrow=c(2,1))

despike <- function(ty)
{
    m <- median(abs(diff(ty$y)))
    bad <- c(0,abs(diff(ty$y))) > 20 * m
    cat("sum(bad)=", sum(bad), " or ", sum(bad)/length(bad)*100, "percent\n")
    plot(ty$t, ty$y, col=bad)
    stop()
    ty[!bad,]
}


population <- function(region)
{
    switch(region,
        "Canada"=37894799,
        "Ontario"=14446515,
        "Quebec"=8433301,
        "British Columbia"=5020302,
        "Alberta"=4345737,
        "Manitoba"=1360396,
        "Saskatchewan"=1168423,
        "Nova Scotia"=965382,
        "New Brunswick"=772094,
        "Newfoundland and Labrador"=523790,
        "Prince Edward Island"=154748,
        "Repatriated travellers"=NA)
}

abbreviation <- function(region)
{
    switch(region,
        "Canada"="Canada",
        "Ontario"="on",
        "Quebec"="qc",
        "British Columbia"="bc",
        "Alberta"="ab",
        "Manitoba"="mb",
        "Saskatchewan"="sk",
        "Nova Scotia"="ns",
        "New Brunswick"="nb",
        "Newfoundland and Labrador"="nt",
        "Prince Edward Island"="pe",
        "Repatriated travellers"=NA)
}

p <- function(province, first, col=1, xlim=NULL)
{
    pr <- abbreviation(province)
    base <- "https://api.covid19tracker.ca/vaccines/age-groups/province/"
    url <- paste0(base, pr)
    message(url)
    d <- fromJSON(paste(readLines(url)))$data
    t <- as.POSIXct(d$date)
    n <- length(t)
    # NOTE: there is also 'atleast1', a sibling to 'full'
    full <- sapply(
        seq_len(n),
        function(i) {
            res <- fromJSON(d$data[i])[["all_ages"]]$full
            if (is.null(res)) NA else res
        }
        )
    y <- 100 * full / population(province)
    ty <- despike(data.frame(t=t, y=y))
    t <- ty$t
    y <- ty$y
    if (first) {
        oce.plot.ts(t, y, drawTimeRange=FALSE,
            xlim=if (!is.null(xlim)) xlim,
            ylab="Fully Vaccinated (% of Population)", ylim=c(0, max),
            xaxs="i", type="l", col=col)
        y2021 <- as.POSIXct("2021-01-01")
        day <- 86400
        abline(v=y2021)
        mtext("2021", side=3, at=y2021+10*day, line=-1, cex=par("cex"))
    } else {
        lines(t, y, col=col, lwd=if(pr=="ns") 3 else 1)
    }
    #mtext(side=4, at=tail(y,1), pr, col=col)
    invisible(data.frame(t=t,y=y))
}

regions <- c("Alberta", "British Columbia" , "Manitoba", "New Brunswick",
    "Newfoundland and Labrador", "Nova Scotia", "Ontario",
    "Prince Edward Island", "Quebec", "Saskatchewan")
first <- TRUE
day <- 86400
tlook <- as.POSIXct(Sys.Date()) + c(-days*day, 0)
#for (i in seq_along(regions)) {
for (i in which(regions=="Ontario")) {
    DAN<-p(regions[i], first, col=i, xlim=tlook)
    first <- FALSE
}

legend("topleft", lwd=par("lwd"), col=1:10, legend=sapply(regions, abbreviation))
