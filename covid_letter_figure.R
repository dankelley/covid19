library(oce)
removeOutliers <- FALSE                # remove points that differ from smoothed curve by 8 std-devs
## The name of the Canadian data file changed sometime near the start of
## April, from the commented-out line to the line after it.
#url <- "https://health-infobase.canada.ca/src/data/summary_current.csv"
url <- "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv"
mar <- c(2, 3, 1, 1)
mgp <- c(1.8,0.7,0)
mar2 <- c(1.75, 2.5, 0.25, 1)          # tighter
mgp2 <- c(1.4,0.5,0)                   # tighter

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



recentNumberOfDays <- 14
now <- Sys.time()
## Cache for speed during code development
if (!exists("d")) {
    message("downloading from ", url)
    d <- read.csv(url)
    d$time <- as.POSIXct(d$date, format="%d-%m-%Y", tz="UTC")
    d$num <- d$numconf + d$numprob
    d$deaths <- d$numdeaths
} else {
    message("using downloaded data")
}
regions <- c("Canada", "Alberta", "British Columbia" , "Manitoba", "New Brunswick",
             "Newfoundland and Labrador", "Nova Scotia", "Ontario",
             "Prince Edward Island", "Quebec", "Saskatchewan")

width <- 7
height <- 4.5
pointsize <- 12

tlim <- range(d$time, na.rm=TRUE)

if (!interactive())
    pdf("covid_letter_figure.pdf",
        width=width,
        height=height,
        pointsize=pointsize)

par(mfrow=c(4, 3))
for (region in regions) {
    message("Handling ", region)
    sub <- subset(d, tolower(prname)==tolower(region))
    sub <- subset(sub, is.finite(sub$numconf))
    sub <- fixLastDuplicated(sub)
    y <- diff(sub$numconf)
    ys <- smooth(y)
    bad <- if (removeOutliers) abs(y-ys) > 8 * sd(y-ys) else rep(FALSE, length(y))
    ylim <- c(0, max(c(y, ys)))
    oce.plot.ts(sub$time[-1][!bad], y[!bad], drawTimeRange=FALSE, ylab="New Daily Cases", type="p",
                #mar=c(2, 3, 1, 1),
                mar=mar2, mgp=mgp2, xlim=tlim, ylim=ylim,
                col="darkgray", pch=20, cex=0.8*par("cex"))# * ifelse(y==0, 0.25, 1))
    abline(h=0, col=4, lwd=0.5*par("lwd"))
    nbad <- sum(bad)
    label <- if (nbad == 1) sprintf(" %s (skipping %d outlier)", region, sum(bad))
        else if (nbad > 1) sprintf(" %s (skipping %d outliers)", region, sum(bad))
        else paste0(" ", region)

    ## spline with df proportional to data length (the 7 is arbitrary)
    ok <- !bad & is.finite(y)
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    points(sub$time[-1][recent], y[recent], pch=20, cex=0.8*par("cex"))
    lines(smooth.spline(sub$time[-1][ok], y[ok], df=length(y)/7), col="magenta", lwd=1)
    mtext(label, cex=par("cex"), adj=0, line=-1)
    mtext(paste0(format(tail(sub$time,1), " %b %d"), ": ", tail(y,1)), adj=0, cex=par("cex"), line=-2)
}


if (!interactive())
    dev.off()
