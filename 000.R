library(oce)
#source("get_data.R")
colGrid <- "gray"
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Nov", "Oct", "Dec")
numberSimplify <- function(x)
{
    res <- NULL
    for (xx in x) {
        res <- c(res,
            if (is.na(xx)) {
                "NA"
            } else if (xx < 10) {
                sprintf("%g", xx)
            } else if (xx < 1000) {
                sprintf("%g", xx)
            } else if (xx < 1e6) {
                sprintf("%gK", xx/1e3)
            } else {
                sprintf("%gM", xx/1e6)
            }
            )
    }
    res
}

width <- 7
height <- 5.5
res <- 110
pointsize <- 11

recentNumberOfDays <- 14
## can specify region in the commandline
args <- commandArgs(trailingOnly=TRUE)
#regions <- if (length(args)) args else "World"
#regions <- if (length(args)) args else "United Kingdom"
#regions <- if (length(args)) args else "Congo (Kinshasa)"
regions <- if (length(args)) args else "US"
#regions <- if (length(args)) args else "China"
#regions <- if (length(args)) args else "Botswana"
#regions <- if (length(args)) args else "Canada"
#regions <- if (length(args)) args else "France"

now <- lubridate::with_tz(Sys.time(), "UTC")
mar <- c(2, 3, 1.5, 1.5)
tlim <- c(as.POSIXct("2020-01-15", format="%Y-%m-%d", tz="UTC"), now)
colDeath <- "red"

for (region in regions) {
    message("handling ", region)
    sub <- getData(region)
    sub$cases_new <- c(0, diff(sub$cases))
    sub$cases_new[sub$cases_new < 0] <- NA
    if (is.na(sub$population[1]))
        stop("unknown population for ", region)

    n <- length(sub$cases)
    if (n < 2) {
        cat("Under 2 data points for", region, "so it is not plotted\n")
        next
    }
    subOrig <- sub
    SD <- sd(tail(head(sub$cases,-1), 7))
    if (abs(sub$cases[n] - sub$cases[n-1]) > 2 * SD) {
        message("dropping most recent point (",
            sub$cases[n], ") since it differs from previous by ",
            round(abs(sub$cases[n] - sub$cases[n-1])),
            ", more than 2* previous recent std-dev of ", round(SD))
        sub$time <- sub$time[seq(1, n-1)]
        sub$cases <- sub$cases[seq(1, n-1)]
        sub$cases_new <- sub$cases_new[seq(1, n-1)]
        sub$deaths <- sub$deaths[seq(1, n-1)]
    }
    lastTime <- tail(sub$time, 1)
    recent <- abs(as.numeric(now) - as.numeric(sub$time)) <= recentNumberOfDays * 86400
    if (!sum(recent))
        next

    if (!interactive())
        png(paste0("covid19_", region, ".png"),
            width=width,
            height=height,
            unit="in",
            res=res,
            pointsize=pointsize)
    par(mfrow=c(1,2))
    t <- sub$time
    y <- diff(sub$cases)
    y <- c(y[1], y)
    y <- smooth(y)
    cm <- colormap(t, col=oceColorsTurbo)
    cex <- 0.8

    ## Cases, linear axis
    oce::oce.plot.ts(t,
        y,
        xaxs="i",
        xlim=tlim,
        col=cm$zcol,
        type="o",
        pch=20,
        cex=cex,
        xlab="Time",
        ylab="Daily Cases",
        mar=mar,
        axes=FALSE)
    box()
    mtext(region, side=3)
    oce.axis.POSIXct(1, drawTimeRange=FALSE)
    yaxp <- par("yaxp")
    yticks <- seq(yaxp[1], yaxp[2], length.out=1+yaxp[3])
    axis(2, at=yticks, labels=numberSimplify(yticks))
    abline(h=yticks, col=colGrid)
    ymax <- par("usr")[4]

    # Polar plot
    t0t <- as.POSIXct("2020-01-01", tz="UTC")
    t0 <- as.numeric(t0t)
    S <- sin(2*pi*(as.numeric(sub$time) - t0)/365/86400)
    C <- cos(2*pi*(as.numeric(sub$time) - t0)/365/86400)
    lim <- c(-1, 1) * max(abs(y))
    par(mar=rep(2,4))
    plot(y * C, y * S,
        axes=FALSE, xlab="", ylab="",
        xlim=lim, asp=1, type="o", pch=20, cex=cex,
        col=cm$zcol)
    grid()
    for (month in 1:12) {
        tt <- ISOdatetime(2020, month, 1, 0, 0, 0, tz="UTC")
        S <- ymax*sin(2*pi*(as.numeric(tt) - t0)/365/86400)
        C <- ymax*cos(2*pi*(as.numeric(tt) - t0)/365/86400)
        lines(c(0, C), c(0, S), col=colGrid)
        tt <- tt + 14 * 86400
        S <- ymax*sin(2*pi*(as.numeric(tt) - t0)/365/86400)
        C <- ymax*cos(2*pi*(as.numeric(tt) - t0)/365/86400)
        text(C, S, months[month])
    }
    theta <- seq(0, 2*pi, pi/16)
    for (tick in yticks) {
        x <- tick * cos(theta)
        y <- tick * sin(theta)
        lines(x, y, col=colGrid)
    }
    if (!interactive()) dev.off()
}
print(numberSimplify(yticks))
