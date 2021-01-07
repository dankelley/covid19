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

locations <- c("Canada", "United Kingdom", "United States")# , "World")
ylim <- if (uniformScale)
    range(sapply(locations, function(l) range(d[d$location==l,]$total_vaccinations_per_hundred))) else NULL
cat("ylim:", ylim[1], " to ", ylim[2], " (range of vaccinations/[100 persons]) \n")

width <- 8
height <- 5
res <- 200
pointsize <- 12
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
        grid()
        if (nrow(dd) > 2) {
            m <- lm(v100 ~ day + I(day^2))
            yearsToAll <- uniroot(function(x) 100 - predict(m, list(day=x)), c(0.1, 10000))$root / 365
        }
        if (!is.null(m))
            lines(dd$time, predict(m), col="blue", lwd=2)
        mtext(locations[ilocation], adj=1, cex=1.1*par("cex"))
        if (!is.null(m))
            mtext(sprintf(" %.1fy to 100%%", yearsToAll), adj=0, line=-1, cex=0.9*par("cex"), col="blue")
        if (packageVersion("oce") > "1.0.1") {
            oce::oce.plot.ts(dd$time, dd$total_vaccinations_per_hundred, ylim=ylim,
                             xlab="", ylab="Vaccines/100", log="y", logStyle="decade", drawTimeRange=FALSE,
                             xlim=xlim, type="p", col=2, pch=20, cex=1.0)
        } else {
            plot(dd$time, dd$total_vaccinations_per_hundred, ylim=ylim,
                 xlab="", ylab="Vaccines/100", log="y",
                 xlim=xlim, type="p", col=2, pch=20, cex=1.0)
        }
        grid()
        ## predictions
        if (!is.null(m))
            lines(dd$time, predict(m), col="blue", lwd=2)
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
        cat("#", locations[ilocation], "\n")
    }
}

if (!interactive())
    dev.off()

