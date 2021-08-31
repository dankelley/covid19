base <- "https://api.covid19tracker.ca/vaccines/age-groups/province/"

max <- 100
lwd <- 3
library(oce)
library(jsonlite)

# Set up R4 colours (not avail on this webserver)
col <- c("black", "#E69F00", "#56B4E9", "#009E73", "#F0E442")

repair <- function(y, criterion=10)
{
    bad <- which(diff(y) > criterion)[1]
    if (length(bad)) {
        i <- seq_along(y)
        y <- ifelse(i > bad, 0.5*y, y)
    }
    y
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
            res <- fromJSON(d$data[i])[["all_ages"]]$full
            if (is.null(res)) NA else res
        }
        )
    y <- 100 * full / population(province)
    if (province == "Ontario")
        y <- repair(y)
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
        dan<<-list(t=t,y=y)
    } else {
        lines(t, y, col=col, lwd=lwd, lty=lty)
    }
    #mtext(side=4, at=tail(y,1), pr, col=col)
    invisible(data.frame(t=t,y=y))
}

regions <- c("Alberta", "British Columbia" , "Manitoba", "New Brunswick",
    "Newfoundland and Labrador", "Nova Scotia", "Ontario",
    "Prince Edward Island", "Quebec", "Saskatchewan")
regions <- "Nova Scotia"
first <- TRUE
day <- 86400
tlook <- as.POSIXct(c("2021-05-01", format(Sys.Date()+14)), tz="UTC")

width <- 7
height <- 5
res <- 150
pointsize <- 10

# regions <- regions[regions=="Ontario"]

if (!interactive())
    png("vaccine-canada-beta.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
for (i in seq_along(regions)) {
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
legend("topleft",
    col=col[i],
    lwd=2,
    lty=ifelse((1:10)<=5, 1, 2),
    bg="white",
    title="Province",
    seg.len=3,
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

spd <- 24 * 3600
extend <- 30
day <- as.numeric(dan$t)/spd - as.numeric(dan$t)[1]/spd
v <- dan$y
par(mfrow=c(1,1), mar=c(3,3,1,1), mgp=c(2,0.7,0))
plot(day, v, type="o", xlim=c(min(day), max(day)+extend), ylim=c(0,100))
grid()
w <- day
w <- ifelse(day > 100, 1, 0)
m <- nls(v ~ 2+98*0.5*(1+tanh((day-day0)/tau)),
    weight=w, start=list(day0=200, tau=30))
dayextended <- c(day, max(day)+(day[2]-day[1])*(1:extend))
lines(dayextended, predict(m, list(day=dayextended)), col=4)

lines(day, ifelse(day<170, 3*day/150, 5 + 100*(1-exp(-(day-150)/100))))
plot(day, v, type="o", xlim=c(min(day), max(day)+extend), ylim=c(0,100))
mm <- lm(v ~ poly(day,3))
lines(dayextended, predict(mm, data.frame(day=dayextended)), col=2)

dv <- diff(v)
dv <- c(dv[1], dv)
m <- nls(dv ~ A/cosh((day-day0)/tau),
    control=nls.control(maxiter=200),
    start=list(A=10, day0=200, tau=20))
par(mfrow=c(2,1))
tlim <- range(dayextended)
plot(day, dv, xlim=tlim, type="o")
lines(dayextended, predict(m, data.frame(day=dayextended)), col=2)
plot(day, v, xlim=tlim, ylim=c(0,100))
lines(dayextended, cumsum(predict(m, data.frame(day=dayextended))), col=2)


# extend using regression on last few points
par(mfrow=c(1,1))
plot(day, v, xlim=range(dayextended), ylim=c(0,100), type="o")
look <- 4
weight <- c(rep(0, length(day) - look), rep(1, look))
m <- lm(v~poly(day, 1), weight=weight)
mp <- predict(m, data.frame(day=dayextended))
lines(dayextended, mp)
abline(v=dan$t[1] + spd*dayextended[which(mp>=75)[1]])
s5 <- dayextended[which(mp>75)[1]]
abline(v=s5)
s5-max(day)


plot(tt, yy, type="o")
xlim <- par("usr")[1:2]
#lines(dan$t, 1+10*exp(-(tt-200)^2/30^2), col=2)
dan$textended <- c(dan$t, tail(dan$t,1)+dt * seq(1, extend))
ttextended <- as.numeric(dan$textended)/spd - as.numeric(dan$textended)[1]/spd
yyextended <- predict(m, list(tt=ttextended))
lines(dan$textended, yyextended, col=4)
plot(dan$textended, cumsum(yyextended), type="l", xlim=xlim, ylim=c(0,100))
grid()
