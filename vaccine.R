library(oce)
options(warn=0)
debug <- FALSE
uniformScale <- TRUE

if (!exists("d0")) {
    file <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
    d0 <- read.csv(file, header=TRUE)
    d0$time <- as.POSIXct(d0$date, tz="UTC")
}

variables <- sort(names(d0))
cat("variables follow (in alphabetical order)\n")
print(variables)

OK <- is.finite(d0$total_vaccinations_per_hundred) & d0$total_vaccinations_per_hundred > 0
d <- d0[OK, ] # ignore old stuff
xlim <- range(d$time)
cat("xlim: ", format(xlim[1]), " to ", format(xlim[2]), " (time when vaccinations were done)\n")

locations <- c("Canada", "United Kingdom", "United States", "World")
ylim <- if (uniformScale)
    range(sapply(locations, function(l) range(d[d$location==l,]$total_vaccinations_per_hundred))) else NULL
cat("ylim:", ylim[1], " to ", ylim[2], " (range of vaccinations/[100 persons]) \n")

width <- 8
height <- 5.5
res <- 200
pointsize <- 11
if (!interactive())
    png("vaccine.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
par(mfcol=c(2,length(locations)), mar=c(3,3,1,1), mgp=c(2,0.7,0))

for (ilocation in seq_along(locations)) {
    dd <- d[d$location == locations[ilocation],]
    cat("locations[", ilocation, "]='", locations[ilocation], "'\n", sep="")
    cat("nrow:", nrow(dd), "\n")
    m <- NULL # to avoid problem with too few data to fit for prediction
    if (nrow(dd)) {
        day <- as.numeric((dd$time - dd$time[1]) / 86400)
        v100 <- dd$total_vaccinations_per_hundred
        oce.plot.ts(dd$time, dd$total_vaccinations_per_hundred, ylim=ylim,
                    xlab="", ylab="Vaccines/100", drawTimeRange=FALSE,
                    xlim=xlim, type="p", col=2, pch=20, cex=1.0)

        if (nrow(dd) > 2) {
            m <- lm(v100 ~ day + I(day^2))
            yearsToAll <- uniroot(function(x) 100 - predict(m, list(day=x)), c(0.1, 10000))$root / 365
        }

        ## exponential fit (red)
        ##> dayOut <- seq(min(day), max(day) + diff(range(day)), length.out=100)
        ##> t <- try(mnls <- nls(v100~a+b*exp(day/c), start=list(a=0, b=0.1, c=5)), silent=TRUE)
        ##> yearsToAllNonlinear <- NA
        ##> if (!inherits(t, "try-error")) {
        ##>     message("nls worked")
        ##>     mp <- predict(mnls)
        ##>     lines(dd$time, mp, col="red", lwd=2)
        ##>     yearsToAllNonlinear <- uniroot(function(x) 100 - predict(mnls, list(day=x)), c(0, 5000))$root / 365
        ##> }
        ## linear fit (blue)
        if (!is.null(m))
            lines(dd$time, predict(m), col="blue", lwd=2)
        mtext(locations[ilocation], adj=1, cex=1.1*par("cex"))
        ## trend <- coef(m)[2]
        ## mtext(sprintf(" %.3f vacc./100 persons/day", trend), adj=0, line=-1, cex=0.9*par("cex"))
        ##?yearsToAll <- 100 / trend / 365
        if (!is.null(m))
            mtext(sprintf(" %.1fy to 100%%", yearsToAll), adj=0, line=-1, cex=0.9*par("cex"), col="blue")
        ##> if (is.finite(yearsToAllNonlinear))
        ##>     mtext(sprintf(" Expon.: %.1fy to 100%%", yearsToAllNonlinear), adj=0, line=-2, cex=0.9*par("cex"), col="red")
        oce.plot.ts(dd$time, dd$total_vaccinations_per_hundred, ylim=ylim,
                    xlab="", ylab="Vaccines/100", log="y", logStyle="decade", drawTimeRange=FALSE,
                    xlim=xlim, type="p", col=2, pch=20, cex=1.0)

        ## predictions
        if (!is.null(m))
            lines(dd$time, predict(m), col="blue", lwd=2)
        ##>if (!inherits(t, "try-error")) {
        ##>    lines(dd$time, mp, col="red", lwd=2)
        ##>}

        cat("#", locations[ilocation], "\n")
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
    } else {
        ## warning("'", locations[ilocation], "' not found\n")
    }
}

if (!interactive())
    dev.off()

