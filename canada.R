# NOTE: the file format has changed at least 3 times since I wrote the original
# version of this.  At some point, I will give up trying to track the different
# filenames, variablenames, etc.

test <- !TRUE # helps me to test for changes in format

library(oce)
do_num_active <- FALSE                 # 2022 May: the numactive data are mostly NA
removeOutliers <- FALSE                # remove points that differ from smoothed curve by 8 std-devs
## The name of the Canadian data file changed sometime near the start of
## April, from the commented-out line to the line after it.
#url <- "https://health-infobase.canada.ca/src/data/summary_current.csv"
#url <- "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv" # fails 2022 Jun 8
url <- "https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv"

mar <- c(2, 3, 1.5, 1)
mgp <- c(1.8, 0.7, 0)
mar2 <- c(1.75, 2.5, 1.25, 1)
mgp2 <- c(1.4, 0.5, 0)

fixLastDuplicated <- function(x)
{
    lastTwo <- tail(x$num, 2)
    if (all(is.finite(lastTwo)) && 0 == diff(tail(x$num, 2))) {
        x <- head(x, -1)
        message("NB. removed final point, because it duplicated its predecessor")
    }
    x
}

abbreviateRegion <- function(r, wide=TRUE)
{
    if (wide)
        return(r)
    if (r == "Newfoundland and Labrador")
        return("NFLD & LAB")
    else if (r == "Prince Edward Island")
        return("PEI")
    else if (r == "British Columbia")
        return("BC")
    else if (r == "New Brunswick")
        return("NB")
    else if (r == "Saskatchewan")
        return("Sask.")
    r
}

# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000901
# Q1, 2020
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

numberSimplify <- function(x)
{
    res <- NULL
    for (xx in x) {
        res <- c(res,
            if (is.na(xx)) {
                "NA"
            } else if (xx < 10) {
                sprintf("%g", xx)
            } else if (xx < 9000) {
                sprintf("%.0f", xx)
            } else if (xx < 1e6) {
                sprintf("%.1fK", xx/1e3)
            } else {
                sprintf("%.1fM", xx/1e6)
            }
            )
    }
    res
}

file <- "covid19_canada.csv"
message("downloading from ", url)
download.file(url, file)

recentNumberOfDays <- 14
now <- Sys.time()
## Cache for speed during code development
#load("d.rda")
if (!exists("d")) {
    d <- read.csv(file, stringsAsFactors=FALSE)
    d$time <- as.POSIXct(d$date, format="%Y-%m-%d", tz="UTC")
} else {
    message("using downloaded data")
}
regions <- c("Canada", "Alberta", "British Columbia" , "Manitoba", "New Brunswick",
    "Newfoundland and Labrador", "Nova Scotia", "Ontario",
    "Prince Edward Island", "Quebec", "Saskatchewan")
width <- 8
height <- 5.5
res <- 200
pointsize <- 11
tlim <- range(d$time, na.rm=TRUE)
message("Time range: ", paste(tlim, collapse=" to "))

if (test) {
    dlook <- d[d$prname == "Alberta",]
    with(dlook, plot(time, numcases_weekly, type="s"))
}

dorig <- d # we are going to focus on only 3 columns
d <- dorig[, c("prname", "time", "numcases_weekly")]

if (!interactive())
    png("canada_change.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
par(mfrow=c(4, 3))

#for (region in "Nova Scotia") {
for (region in regions) {
    if (region == "Canada")
        next
    sub <- subset(d, tolower(prname)==tolower(region))
    # get rid of things we don't use, to make it easier to look at the data whilst debugging
    message("Handling ", region, ", population ", round(population(region)/1e6,1), "M, last 3 numconf=", paste(tail(sub$numcases_weekly,3), collapse=" "))
    sub$newDailyPer100K <- 1e5 * sub$numcases_weekly / population(region) / 7
    print(tail(sub, 4))
    t <- sub$time
    y <- sub$newDailyPer100K
    oce.plot.ts(t, y, drawTimeRange=FALSE, ylab="New Daily Cases / 100K", type="p",
        mar=mar2, mgp=mgp2, xlim=tlim,
        col="darkgray", pch=20, cex=0.8*par("cex"))# * ifelse(y==0, 0.25, 1))
    abline(h=0, col=4, lwd=0.5*par("lwd"))
    abline(v=now, col=4, lwd=0.5*par("lwd"))
    # spline with df proportional to data length (the 7 is arbitrary)
    lines(smooth.spline(t, y, df=length(y)/2), col="magenta", lwd=1)
    mtext(sprintf(" %s\n %.1f/day/100K on %s",
            region,
            round(tail(y,1),1),
            format(tail(sub$time,1), "%b %d")),
        adj=0, cex=par("cex"), line=-1)
}

if (!interactive())
    dev.off()

