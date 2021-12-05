base <- "https://api.covid19tracker.ca/vaccines/age-groups/province/"

max <- 100
lwd <- 3
library(oce)
library(jsonlite)

# Set up R4 colours (not avail on this webserver)
col <- c("black", "#E69F00", "#56B4E9", "#009E73", "#F0E442")

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

pl <- function(province, first, col=1, xlim=NULL, lty=1, lwd=2.5,
    base="https://api.covid19tracker.ca/vaccines/age-groups/province/")
{
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
            j <- fromJSON(d$data[i])
            if ("all_ages" %in% names(j)) {
                res <- j[["all_ages"]]$full
            } else {
                res <- sum(sapply(j, function(x) x$full))
            }
            if (is.null(res)) NA else res
        }
        )
    y <- 100 * full / population(province)
    if (first) {
        oce.plot.ts(t, y, drawTimeRange=FALSE,
            yaxs="i",
            xlim=if (!is.null(xlim)) xlim, lwd=lwd, lty=lty,
            ylab="Fully Vaccinated (% of Population)", ylim=c(0, max),
            xaxs="i", type="l", col=col, grid=TRUE)
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
tlook <- as.POSIXct(c("2021-05-01", format(Sys.Date()+14)), tz="UTC")

width <- 7
height <- 5
res <- 150
pointsize <- 10

# regions <- regions[regions=="Ontario"]

if (!interactive())
    png("vaccine-canada.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
for (i in seq_along(regions)) {
    message("i=",i, " region=", regions[i])
    test <- pl(regions[i], first, xlim=tlook,
        col=col[1+(i-1)%%5],
        lwd=2.5,
        lty=ifelse(i<=5, 1, 2))
    first <- FALSE
}

target <- c(75, 85)
abline(h=target, lty=c(2,1), col="red")
mtext(target, side=4, at=target, col="red")

i <-1 + ((1:10)-1)%%5
legend("bottomright",
    col=col[i],
    lwd=2,
    lty=ifelse((1:10)<=5, 1, 2),
    bg="white",
    title="Province",
    seg.len=4,
    cex=1.0,
    legend=toupper(sapply(regions, abbreviation)))

if (!interactive())
    dev.off()

# debugging ONT (alter the for loop to focus on "on")
#<old> if (!interactive())
#<old>     png("vaccine-canada-ontario.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
#<old>     plot(dan$t, dan$y)
#<old>     points(dan$t, dan$y/2, col=2)
#<old> if (!interactive())
#<old>     dev.off()
#<old> }

