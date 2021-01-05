uniformScale <- TRUE

file <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
d <- read.csv(file, header=TRUE)

variables <- sort(names(d))
cat("variables follow (in alphabetical order)\n")
print(variables)

vaccineStart <- as.POSIXct("2020-12-10")
now <- Sys.Date()
d$time <- as.POSIXct(d$date)
print(c(vaccineStart, now))
d <- d[d$time >= vaccineStart, ] # ignore old stuff

locations <- c("Canada", "United Kingdom", "United States")
prange <- function(x) range(x[is.finite(x) & x > 0], na.rm=TRUE)
ylim <- if (uniformScale)
    range(sapply(locations, function(l) prange(d[d$location==l,]$total_vaccinations_per_hundred))) else NULL

width <- 8
height <- 5.5
res <- 200
pointsize <- 11
if (!interactive())
    png("vaccine.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
par(mfcol=c(2,3), mar=c(3,3,1,1), mgp=c(2,0.7,0))
for (location in c("Canada", "United Kingdom", "United States")) {
    dd <- d[d$location == location,]
    ## Trim the zero values (for old oce 1.2.0 and earlier, which does not ignore the <=0 cases)
    dataOK <- is.finite(dd$total_vaccinations_per_hundred) & dd$total_vaccinations_per_hundred > 0
    dd <- dd[dataOK, ]
    if (nrow(dd)) {
        day <- (dd$time - dd$time[1]) / 86400
        v100 <- dd$total_vaccinations_per_hundred
        m <- lm(v100 ~ day)
        plot(dd$time, dd$total_vaccinations_per_hundred, ylim=ylim,
             xlab="", ylab="Vaccines/100",
             xlim=c(vaccineStart, now), type="o")
        mtext(location, adj=1, cex=1.1*par("cex"))
        trend <- coef(m)[2]
        mtext(sprintf(" Trend: %.3f vaccinations/100 persons/day", trend), adj=0, line=-1, cex=0.9*par("cex"))
        yearsToAll <- 100 / trend / 365
        mtext(sprintf(" Implies %.1f years to 100%%", yearsToAll), adj=0, line=-2, cex=0.9*par("cex"))
        plot(dd$time, dd$total_vaccinations_per_hundred, ylim=ylim,
             xlab="", ylab="Vaccines/100", log="y",
             xlim=c(vaccineStart, now), type="o")
        # mtext(sprintf("General Life Expectancy: %.1fy", dd$life_expectancy[1]), cex=par("cex"))
        cat("#", location, "\n")
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
        warning("'", location, "' not found\n")
    }
}

if (!interactive())
    dev.off()

