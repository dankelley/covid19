library(oce)
options(warn=0)
debug <- FALSE
lwd <- 6
spd <- 86400                           # seconds/day
LOOK <- 10                             # points at end for linear fit
criterion <- c(0.75, 0.85)*200         # 2 shots/person for 85% of pop (assume 13% under 12y)
yearsToCriterion <- rep(NA, 2)

timeFormat <- function(y)
{
    d <- y * 365
    if (y > 1.5) paste(round(y, 1), "years")
    else paste(round(d, 0), "days")
}
timeFormatOLD <- function(y)
{
    d <- y * 365
    cat("d=",d)
    rval <- if (y > 1.5)
        paste(round(y, 1), "years")
    else if (d > 3*28) # report >3mo (approx) as mo
        paste(round(d/28, 0), "months")
    else if (d > 3*7) # report >3we as we
        paste(round(d/7, 0), "weeks")
    else
        paste(round(d, 0), "days")
    gsub("^1 (.*)s", "1 \\1", rval)
}
#.for (y in seq(0,4*28, 2)/365)
#.    print(timeFormat(y))
#.stop()

if (!exists("d0")) {
    file <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
    d0 <- read.csv(file, header=TRUE)
    d0$time <- as.POSIXct(d0$date, tz="UTC")
}

variables <- sort(names(d0))
if (debug) {
    cat("variables follow (in alphabetical order)\n")
    print(variables)
}

OK <- is.finite(d0$total_vaccinations_per_hundred) & d0$total_vaccinations_per_hundred > 0
d <- d0[OK, ] # ignore old stuff
xlim <- range(d$time)

if (debug)
    cat("xlim: ", format(xlim[1]), " to ", format(xlim[2]), " (time when vaccinations were done)\n")

locations <- c("Canada", "Germany", "Italy", "South Africa", "United Kingdom", "United States")
width <- 7
height <- 5
res <- 200
pointsize <- 11
if (!interactive())
    png("vaccine.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
par(mar=c(2,3,1,1), mgp=c(2,0.7,0))
nlocations <- length(locations)
N <- ceiling(sqrt(length(locations)))
par(mfrow=c(N, ceiling(nlocations/N)))


for (ilocation in seq_along(locations)) {
    cat("# ", locations[ilocation], "\n")
    dd <- d[d$location == locations[ilocation],]
    if (debug) {
        cat("locations[", ilocation, "]='", locations[ilocation], "'\n", sep="")
        cat("nrow:", nrow(dd), "\n")
    }
    m1 <- NULL # to avoid problem with too few data to fit for prediction
    ndata <- nrow(dd)
    if (ndata > 10) {
        v100 <- dd$total_vaccinations_per_hundred
        focus <- c(rep(FALSE,ndata-LOOK),rep(TRUE,LOOK))
        oce.plot.ts(dd$time, dd$total_vaccinations_per_hundred,
                    mar=c(2,3,1.5,1),
                    drawTimeRange=FALSE,
                    xlab="", ylab="Vaccinations / 100 Persons",
                    xlim=xlim, ylim=c(0, 300), yaxs="i",
                    type="p", pch=20,
                    cex=ifelse(focus, 0.7, 0.3),
                    col=ifelse(focus, "black", "gray"))
        if (nrow(dd) > 3) {
            day <- as.numeric((dd$time - dd$time[1]) / 86400)
            weights <- ifelse(day > max(day) - 10, 1, 0)
            m1 <- lm(v100 ~ day, w=ifelse(focus, 1, 0))
            if (FALSE) {               # dots suffice, since curve is smooth
                abline(m1, col="magenta")
            }
            print(summary(m1))
            x <- seq(min(day), min(day) + 20*365, 1)
            yearsToCriterion[1] <- which(as.vector(predict(m1, list(day=x))) >= criterion[1])[1] / 365 - max(day) / 365
            yearsToCriterion[2] <- which(as.vector(predict(m1, list(day=x))) >= criterion[2]) [1]/ 365 - max(day) / 365
        } else {
            cat("  too few rows (", nrow(dd), ") to fit curve\n", sep="")
        }
        newdata <- list(day=sort(tail(day, LOOK)))
        newdata$day <- c(newdata$day, tail(newdata$day, 1) + 30) # tack on 30d hence, for extrapolation

        if (!is.null(m1)) {
            P1 <- predict(m1, newdata=newdata)
            lines((dd$time[1]+newdata[[1]]*86400), P1, col=rgb(1,0,0,0.6), lwd=2*par("lwd"))
        }
        if (FALSE) { # no need to redraw since no lines shown now
            points(dd$time, dd$total_vaccinations_per_hundred,
                   pch=20,
                   cex=ifelse(focus, 0.7, 0.3),
                   col=ifelse(focus, "black", "gray"))
        }
        mtext(locations[ilocation], side=3, cex=par("cex"))
        if (!is.null(m1) && all(is.finite(yearsToCriterion))) {
            msg <- sprintf(" %s: %.1fM doses (%.1f per 100 persons) given.\n Last %d reports: %.2f doses/100 person/day,\n Predict 2-dose for %.0f%% of population\n in %s, %.0f%% in %s.",
                format(tail(dd$time,1), "%b %d"),
                round(tail(dd$total_vaccinations,1)/1e6, 1),
                tail(dd$total_vaccinations,1)*100/dd$population[1],
                LOOK,
                coef(m1)[[2]],
                #round(coef(m1)[2],3),
                criterion[1]/2, timeFormat(yearsToCriterion[1]),
                criterion[2]/2, timeFormat(yearsToCriterion[2]))
            mtext(msg, adj=0, line=-4, cex=0.9*par("cex"))
        }
        if (debug) {
            cat(oce::vectorShow(dd$population_density[1]))
            cat(oce::vectorShow(dd$median_age[1]))
            cat(oce::vectorShow(dd$aged_65_older[1]))
            cat(oce::vectorShow(dd$aged_70_older[1]))
            cat(oce::vectorShow(dd$gdp_per_capita[1]))
            cat(oce::vectorShow(dd$extreme_poverty[1]))
            cat(oce::vectorShow(dd$cardiovasc_death_rate[1]))
            cat(oce::vectorShow(dd$diabetes_prevalence[1]))
            cat(oce::vectorShow(dd$female_smokers[1]))
            cat(oce::vectorShow(dd$male_smokers[1]))
            cat(oce::vectorShow(dd$hospital_beds_per_thousand[1]))
            cat(oce::vectorShow(dd$life_expectancy[1]))
            cat(oce::vectorShow(dd$human_development_index[1]))
        }
    }
}

