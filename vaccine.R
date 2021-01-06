library(oce)
options(warn=0)
uniformScale <- TRUE

if (!exists("d0")) {
    file <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
    d0 <- read.csv(file, header=TRUE)
    d0$time <- as.POSIXct(d0$date, tz="UTC")
}

variables <- sort(names(d0))
cat("variables follow (in alphabetical order)\n")
print(variables)

##vaccineStart <- as.POSIXct("2020-12-10", tz="UTC")
## now <- as.POSIXct(Sys.time(), tz="UTC")
##xlim <- as.POSIXct(c(vaccineStart, now), tz="UTC")

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
    if (nrow(dd)) {
        day <- (dd$time - dd$time[1]) / 86400
        DANday <<- day
        cat(oce::vectorShow(day))
        v100 <- dd$total_vaccinations_per_hundred
        DANv100<<-v100
        cat(oce::vectorShow(v100))
        m <- lm(v100 ~ day)
        oce.plot.ts(dd$time, dd$total_vaccinations_per_hundred, ylim=ylim,
                    xlab="", ylab="Vaccines/100", drawTimeRange=FALSE,
                    xlim=xlim, type="o")
        lines(dd$time, predict(m), col="orange", lwd=2)
        mtext(locations[ilocation], adj=1, cex=1.1*par("cex"))
        trend <- coef(m)[2]
        mtext(sprintf(" %.3f vacc./100 persons/day", trend), adj=0, line=-1, cex=0.9*par("cex"))
        yearsToAll <- 100 / trend / 365
        mtext(sprintf(" -> %.1f years to 100%%", yearsToAll), adj=0, line=-2, cex=0.9*par("cex"))

        oce.plot.ts(dd$time, dd$total_vaccinations_per_hundred, ylim=ylim,
                    xlab="", ylab="Vaccines/100", log="y", logStyle="decade", drawTimeRange=FALSE,
                    xlim=xlim, type="o")

        cat("#", locations[ilocation], "\n")
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
    } else {
        ## warning("'", locations[ilocation], "' not found\n")
    }
}

if (!interactive())
    dev.off()

