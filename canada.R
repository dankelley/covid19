library(oce)
removeOutliers <- FALSE                # remove points that differ from smoothed curve by 8 std-devs
## The name of the Canadian data file changed sometime near the start of
## April, from the commented-out line to the line after it.
#url <- "https://health-infobase.canada.ca/src/data/summary_current.csv"
url <- "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv"
mar <- c(2, 3, 1.5, 1)
mgp <- c(1.8, 0.7, 0)
mar2 <- c(1.75, 2.5, 1, 1)
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



recentNumberOfDays <- 14
now <- Sys.time()
## Cache for speed during code development
if (!exists("d")) {
    message("downloading from ", url)
    d <- read.csv(url)
    d$time <- as.POSIXct(d$date, format="%d-%m-%Y", tz="UTC")
    d$numactive <- as.numeric(d$numactive)
    d$numconf <- as.numeric(d$numconf)
    d$numprob <- as.numeric(d$numprob)
    d$numtoday <- as.numeric(d$numtoday)
    d$numteststoday <- as.numeric(d$numteststoday)
    d$numdeaths <- as.numeric(d$numdeaths)
    d$num <- d$numconf + d$numprob
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

if (!interactive())
    png("canada_linear.png",
        width=width,
        height=height,
        unit="in",
        res=res,
        pointsize=pointsize)
par(mfrow=c(4, 3))
## Problem: "Repatriated travellers" and "Repatriated Travellers" both exist.
tlim <- range(d$time, na.rm=TRUE)
## Ignore the territories (few data) and also repatriated travellers (oddly broken
## up into two groups, presumably because of poor data handling).

message("canada_linear.png")
for (region in regions) {
    message("Handling ", region)
    sub <- subset(d, tolower(prname)==tolower(region))
    sub <- subset(sub, is.finite(sub$numconf))
    sub <- fixLastDuplicated(sub)
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    oce.plot.ts(sub$time, sub$numconf/1e3,
                mar=mar, mgp=mgp,
                ylab="Cases & Deaths", xlim=tlim,
                type="p", pch=20, col=ifelse(recent, "black", "gray"),
                drawTimeRange=FALSE)
    points(sub$time, sub$numdeaths/1e3, pch=20, col=ifelse(recent, "red", "pink"), cex=ifelse(recent, 1, 0.7))
    mtext(paste0(" ", region), cex=par("cex"), adj=0, line=-1)
    mtext(sprintf(" Cases: %s (%.3f%%)",
                  numberSimplify(tail(sub$numconf,1)),
                  100*tail(sub$numconf,1)/population(region)),
          cex=par("cex"), adj=0, line=-2)
    print(tail(sub$numdeaths,1))
    mtext(sprintf(" Deaths: %s (%.3f%%)",
                  numberSimplify(tail(sub$numdeaths,1)),
                  100*tail(sub$numdeaths,1)/population(region)),
          cex=par("cex"), adj=0, line=-3)
}
if (!interactive())
    dev.off()

message("linear plots of positivity percent")
if (!interactive())
    png("canada_positivity.png",
        width=width,
        height=height,
        unit="in",
        res=res,
        pointsize=pointsize)
par(mfrow=c(4, 3))
tlim <- range(d$time)
for (region in regions) {
    message("Handling ", region)
    sub <- subset(d, tolower(prname)==tolower(region))
    sub <- fixLastDuplicated(sub)
    y <- 100 * sub$numtoday / sub$numteststoday
    ok <- is.finite(y)
    t <- sub$time[ok]
    recent <- abs(as.numeric(now) - as.numeric(t)) <= recentNumberOfDays * 86400
    y <- y[ok]
    ylim <- c(0, max(y))
    oce.plot.ts(t, y,
                mar=mar2, mgp=mgp2,
                ylab="Test Positivity [percent]", xlim=tlim, ylim=ylim,
                type="p", pch=20, col=ifelse(recent, "black", "gray"),
                drawTimeRange=FALSE)
    abline(h=0, col=4, lwd=0.5*par("lwd"))
    lines(smooth.spline(t, y, df=length(y)/7), col="magenta", lwd=1)
    nt <- length(t)
    if (tolower(region) == "canada") {
        mtext(region, side=3, adj=1, line=-1, cex=par("cex"))
    } else {
        mtext(paste0(region, " \n",
                     format(t[nt-10L], "%b %d"),
                     " to ",
                     format(t[nt], "%b %d \n"),
                     round(mean(tail(y, recentNumberOfDays)), 2), "% "), adj=1, cex=par("cex"), line=-3)
    }
}
if (!interactive())
    dev.off()

if (!interactive())
    png("canada_linear_active.png",
        width=width,
        height=height,
        unit="in",
        res=res,
        pointsize=pointsize)
par(mfrow=c(4, 3))
tlim <- range(d$time)
message("linear plots of active cases")
for (region in regions) {
    message("Handling ", region)
    sub <- subset(d, tolower(prname)==tolower(region))
    sub <- subset(sub, is.finite(sub$numactive))
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    y <- sub$numactive
    oce.plot.ts(sub$time, y,
                mar=mar2, mgp=mgp2,
                ylab="Active Cases", xlim=tlim,
                type="p", pch=20, col=ifelse(recent, "black", "gray"),
                drawTimeRange=FALSE)
    abline(h=0, col=4, lwd=0.5*par("lwd"))
    ok <- is.finite(y)
    lines(smooth.spline(sub$time[ok], y[ok], df=length(y)/7), col="magenta", lwd=1)
    mtext(paste0(" ", region), cex=par("cex"), adj=0, line=-1)
    mtext(paste0(format(tail(sub$time,1), " %b %d"), ": ", tail(y,1)), adj=0, cex=par("cex"), line=-2)
}
if (!interactive())
    dev.off()

if (!interactive())
    png("canada_log.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
par(mfrow=c(4, 3))
message("log plots")
## Uniform scale for all log plots, to make
## it easier to see slope differences.
ylim <- c(1, 2*max(d$num, na.rm=TRUE))
for (region in regions) {
    message("Handling ", region)
    sub <- subset(d, tolower(prname)==tolower(region))
    sub <- subset(sub, is.finite(sub$numconf) & sub$numconf > 0)
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    lastTwo <- tail(sub$numconf, 2)
    lastDuplicated <- all(is.finite(lastTwo)) && 0 == diff(lastTwo)
    if (lastDuplicated) {
        sub <- head(sub, -1)
        message("NB. removed final point, because it duplicated its predecessor")
    }
    if (length(sub$numconf) > 0) {
        positive <- sub$numconf > 0
        oce.plot.ts(sub$time[positive], sub$numconf[positive],
                    #mar=c(2, 3, 1, 1),
                    mar=mar, mgp=mgp,
                    ylab="Cases & Deaths", xlim=tlim,
                    type="p", pch=20, col=ifelse(recent, "black", "gray"),
                    cex=ifelse(recent, 1, 0.7),
                    ylim=ylim, log="y", logStyle="decade",
                    drawTimeRange=FALSE)
        points(sub$time, sub$numdeaths, pch=20, col=ifelse(recent, "red", "pink"), cex=ifelse(recent, 1, 0.7))
        ## Case doubling time
        y <- sub$numconf[recent]
        ok <- y > 0
        x <- (as.numeric(sub$time)[recent])[ok]
        y <- log10(y[ok])
        canFit <- length(x) > 3 && tolower(region) != "repatriated travellers"
        t2c <- NA
        if (canFit) {
            m <- lm(y ~ x)
            xx <- seq(par("usr")[1], par("usr")[2], length.out=100)
            growthRate <- coef(m)[[2]] * 86400 # in days
            t2c <- log10(2) / growthRate
            if (0 < t2c && t2c < 100) {
                lines(xx, 10^predict(m, list(x=xx)), lty="dotted")
                ##mtext(sprintf(" cases double in %.0fd", t2), side=3, adj=0, line=-1, cex=par("cex"))
            }
        }
        ## Death doubling time
        y <- sub$numdeaths[recent]
        ok <- y > 0
        x <- (as.numeric(sub$time)[recent])[ok]
        y <- log10(y[ok])
        canFit <- length(x) > 3 && tolower(region) != "repatriated travellers"
        t2d <- NA
        if (canFit) {
            m <- lm(y ~ x)
            xx <- seq(par("usr")[1], par("usr")[2], length.out=100)
            growthRate <- coef(m)[[2]] * 86400 # in days
            t2d <- log10(2) / growthRate
            if (is.infinite(t2d))
                t2d <- 1000 # used so we can get CFL later
            if (!is.na(t2d) && 0 < t2d && t2d < 100) {
                lines(xx, 10^predict(m, list(x=xx)), col="red", lty="dotted")
                ##mtext(sprintf(" deaths double in %.0fd", t2), side=3, adj=0, line=-2, col="red", cex=par("cex"))
            }
        }
        lab <- paste0(" Cases double in ",
                      if (is.finite(t2c) && t2c < 100 && t2c > 0) round(t2c,0) else ">100",
                      "d, deaths in ",
                      if (is.finite(t2d) && t2d < 100 && t2d > 0) round(t2d,0) else ">100", "d")
        mtext(lab, side=3, line=-1, cex=par("cex"), adj=0)
        lastDeaths <- tail(sub$numdeaths[is.finite(sub$numdeaths)],1)
        if (lastDeaths == 0) {
            mtext(" Case Fatality Rate: 0%", side=3, line=-2, cex=par("cex"), adj=0)
        } else {
            if (is.finite(t2c) && is.finite(t2d) && (t2c < 0 || t2c > 30) && (t2d < 0 || t2d > 30))
                mtext(sprintf(" Case Fatality Rate: %.1f%%",
                              100 * lastDeaths / tail(sub$numconf, 1)),
                      side=3, line=-2, cex=par("cex"), adj=0)
        }
    } else {
        plot(0:1, 0:1, xlab="", ylab="", type="n")
        text(0.5, 0.5, "No counts")
    }
    mtext(region, cex=par("cex"), adj=0)
    mtext(paste(format(tail(sub$time,1), "%b %d")), adj=1, cex=par("cex"))
}
if (!interactive())
    dev.off()


if (!interactive())
    png("canada_change.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
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
