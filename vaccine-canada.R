message("ONT seems to have a 2X shift in mid July, 2021")

base <- "https://api.covid19tracker.ca/vaccines/age-groups/province/"

DESPIKE <- FALSE # useless, since the ONT problem is not a spike

max <- 65
days <- 90
library(oce)
library(jsonlite)

despike <- function(ty)
{
    m <- median(abs(diff(ty$y)))
    bad <- c(0,abs(diff(ty$y))) > 20 * m
    cat("sum(bad)=", sum(bad), " or ", sum(bad)/length(bad)*100, "percent\n")
    #plot(ty$t, ty$y, col=bad)
    #stop()
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
        "Newfoundland and Labrador"="nl",
        "Prince Edward Island"="pe",
        "Repatriated travellers"=NA)
}

pl <- function(province, first, col=1, xlim=NULL, lty=1, lwd=3,
    base="https://api.covid19tracker.ca/vaccines/age-groups/province/")
{
    message("in p")
    pr <- abbreviation(province)
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
    if (DESPIKE) {
        ty <- despike(data.frame(t=t, y=y))
        t <- ty$t
        y <- ty$y
    }
    if (first) {
        oce.plot.ts(t, y, drawTimeRange=FALSE,
            xlim=if (!is.null(xlim)) xlim, lwd=lwd, lty=lty,
            ylab="Fully Vaccinated (% of Population)", ylim=c(0, max),
            xaxs="i", type="l", col=col)
        y2021 <- as.POSIXct("2021-01-01")
        day <- 86400
        abline(v=y2021)
        mtext("2021", side=3, at=y2021+10*day, line=-1, cex=par("cex"))
    } else {
        lines(t, y, col=col, lwd=lwd, lty=lty)
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

width <- 7
height <- 5
res <- 200
pointsize <- 11

if (!interactive())
    png("vaccine-canada.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
palette("Okabe-Ito")                   # the host machine has an old R wth a poor yellow
for (i in seq_along(regions)) {
    test <- pl(regions[i], first, col=i, xlim=tlook, lty=ifelse(i<=5, 1, 3))
    first <- FALSE
}

legend("topleft", lwd=3, col=1:10, lty=c(rep(1,5), rep(3,5)),
    title="Province",
    seg.len=3, legend=sapply(regions, abbreviation))
mtext("Note: Ontario goes offscale because of a 2X shift")

if (!interactive())
    dev.off()

# debugging ONT (alter the for loop to focus on "on")
if (FALSE) {
    plot(dan$t, dan$y)
    points(dan$t, dan$y/2, col=2)
}
