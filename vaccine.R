library(oce)

file <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
if (!exists("d")) d <- read.csv(file, header=TRUE) else cat("using cached data\n")

variables <- sort(names(d))
vaccineStart <- as.POSIXct("2020-12-10")
now <- Sys.Date()
d$time <- as.POSIXct(d$date)
print(c(vaccineStart, now))
if (!interactive())
    pdf("vaccine.pdf")
par(mfcol=c(2,3))
for (location in c("Canada", "United Kingdom", "United States")) {
    placeOK <- d$location == location
    timeOK <- d$time > vaccineStart
    dataOK <- is.finite(d$total_vaccinations_per_hundred) & d$total_vaccinations_per_hundred > 0
    dlook <- d[placeOK & timeOK & dataOK, ]
    if (nrow(dlook)) {
        day <- (dlook$time - dlook$time[1]) / 86400
        v100 <- dlook$total_vaccinations_per_hundred
        m <- lm(v100 ~ day)
        oce.plot.ts(dlook$time, dlook$total_vaccinations_per_hundred, xlab="", ylab="Vaccines/100", drawTimeRange=FALSE, grid=TRUE, xlim=c(vaccineStart, now), type="o")
        mtext(location, adj=1)
        mtext(sprintf("%.3f v100/d", coef(m)[2]), adj=0, line=-1, cex=par("cex"))
        oce.plot.ts(dlook$time, dlook$total_vaccinations_per_hundred, xlab="", ylab="Vaccines/100", log="y", logStyle="decade", drawTimeRange=FALSE, grid=TRUE, xlim=c(vaccineStart, now), type="o")
        mtext(sprintf("General Life Expectancy: %.1fy", dlook$life_expectancy[1]), cex=par("cex"))

        cat("#", location, "\n")
        cat(oce::vectorShow(dlook$population_density[1]))
        cat(oce::vectorShow(dlook$median_age[1]))
        cat(oce::vectorShow(dlook$aged_65_older[1]))
        cat(oce::vectorShow(dlook$aged_70_older[1]))
        cat(oce::vectorShow(dlook$gdp_per_capita[1]))
        cat(oce::vectorShow(dlook$extreme_poverty[1]))
        cat(oce::vectorShow(dlook$cardiovasc_death_rate[1]))
        cat(oce::vectorShow(dlook$diabetes_prevalence[1]))
        cat(oce::vectorShow(dlook$female_smokers[1]))
        cat(oce::vectorShow(dlook$male_smokers[1]))
        cat(oce::vectorShow(dlook$hospital_beds_per_thousand[1]))
        cat(oce::vectorShow(dlook$life_expectancy[1]))
        cat(oce::vectorShow(dlook$human_development_index[1]))




    } else {
        warning("'", location, "' not found\n")
    }
}

if (!interactive())
    dev.off()

