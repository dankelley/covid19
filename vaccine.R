library(oce)
options(warn=0)
debug <- FALSE
lwd <- 6
spd <- 86400                           # seconds/day
LOOK <- 10                             # points at end for linear fit
criterion <- 0.87*200                  # 2 shots/person for 87% of pop (assume 13% under 12y)

timeFormat <- function(y)
{
    if (y > 1.5) 
        paste(round(y, 1), "years")
    else if (y > 3/12)
        paste(round(12*y, 0), "months")
    else if (y > 0.5/12)
        paste(round(4*12*y, 0), "weeks")
    else
        paste(round(7*4*12*y, 0), "days")
}

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

locations <- c("Canada", "France", "Germany", "Italy", "United Kingdom", "United States")
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
        ## Linear plot
        v100 <- dd$total_vaccinations_per_hundred
        focus <- c(rep(FALSE,ndata-LOOK),rep(TRUE,LOOK))
        oce.plot.ts(dd$time, dd$total_vaccinations_per_hundred,
                    mar=c(2,3,1.5,1),
                    drawTimeRange=FALSE,
                    xlab="", ylab="Vaccinations / 100 Persons",
                    xlim=xlim, type="p",
                    cex=ifelse(focus, 1, 0.5),
                    col=ifelse(focus, "black", "gray"))
        if (nrow(dd) > 3) {
            day <- as.numeric((dd$time - dd$time[1]) / 86400)
            weights <- ifelse(day > max(day) - 10, 1, 0)
            m1 <- lm(v100 ~ day, w=ifelse(focus, 1, 0))
            abline(m1, col="magenta")
            print(summary(m1))
            x <- seq(min(day), min(day) + 20*365, 1)
            yearsToAll1 <- which(as.vector(predict(m1, list(day=x))) > criterion)[1] / 365 - max(day) / 365
        } else {
            cat("  too few rows (", nrow(dd), ") to fit curve\n", sep="")
        }
        newdata <- list(day=sort(tail(day, LOOK)))

        if (!is.null(m1)) {
            P1 <- predict(m1, newdata=newdata)
            lines((dd$time[1]+newdata[[1]]*86400), P1, col=rgb(1,0,0,0.4), lwd=lwd)
        }
        points(dd$time, dd$total_vaccinations_per_hundred,
               cex=ifelse(focus, 1, 0.5),
               col=ifelse(focus, "black", "gray"))
        mtext(locations[ilocation], side=3, cex=par("cex"))
        if (!is.null(m1) && is.finite(yearsToAll1)) {
            mtext(sprintf(" %s: %.1fM doses (%.1f per 100 persons) given.\n Last %d reports: %.2f doses/100 person/day,\n Expect 2-dose for 87%% of population in %s.",
                         format(tail(dd$time,1), "%b %d"),
                         round(tail(dd$total_vaccinations,1)/1e6, 1),
                         tail(dd$total_vaccinations,1)*100/dd$population[1],
                         LOOK,
                         coef(m1)[[2]],
                         #round(coef(m1)[2],3),
                         timeFormat(yearsToAll1)),
                  adj=0, line=-3, cex=0.9*par("cex"))
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

