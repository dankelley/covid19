options(warn=0)
debug <- FALSE
lwd <- 3
spd <- 86400                           # seconds/day
LOOK <- 10                             # days at end for linear fit

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
lastTenDays <- tail(d$time, 1) - LOOK * spd

if (debug)
    cat("xlim: ", format(xlim[1]), " to ", format(xlim[2]), " (time when vaccinations were done)\n")

locations <- c("Canada", "Israel", "United Kingdom", "United States")

width <- 7
height <- 3.5
res <- 120
pointsize <- 10
if (!interactive())
    png("vaccine.png", width=width, height=height, unit="in", res=res, pointsize=pointsize)
N <- ceiling(sqrt(length(locations)))
par(mfrow=c(N,N), mar=c(2,3,1,1), mgp=c(2,0.7,0))

for (ilocation in seq_along(locations)) {
    cat("handling", locations[ilocation], "\n")
    dd <- d[d$location == locations[ilocation],]
    if (debug) {
        cat("locations[", ilocation, "]='", locations[ilocation], "'\n", sep="")
        cat("nrow:", nrow(dd), "\n")
    }
    m1 <- NULL # to avoid problem with too few data to fit for prediction
    if (nrow(dd)) {
        ## Linear plot
        v100 <- dd$total_vaccinations_per_hundred
        focus <- dd$time >= (tail(dd$time, 1) - 10*spd)
        plot(dd$time, dd$total_vaccinations_per_hundred,
             xlab="", ylab="Vaccinations / 100 Persons",
             xlim=xlim, type="p",
             cex=ifelse(focus, 1, 0.5),
             col=ifelse(focus, "black", "gray"))
        grid(lty=1, col="lightgray", lwd=0.33)
        if (nrow(dd) > 3) {
            day <- as.numeric((dd$time - dd$time[1]) / 86400)
            past <- diff(range(day))
            if (debug)
                cat("past=", past, "\n")
            weights <- ifelse(day > max(day) - 10, 1, 0)
            m1 <- lm(v100 ~ day, w=weights)
            cat(locations[ilocation], " coef(m1): ", paste(coef(m1), collapse=", "), "\n")
            print(summary(m1))

            x <- seq(min(day)-5, min(day) + 20*365, 0.5)
            criterion <- 200           # 2 shots/person
            yearsToAll1 <- which(as.vector(predict(m1, list(day=x))) > criterion)[1] / 365
        } else {
            cat("  too few rows (", nrow(dd), ") to fit curve\n", sep="")
        }
        newdata <- list(day=seq(tail(day,1)-LOOK, max(day), 0.5))

        if (!is.null(m1)) {
            P1 <- predict(m1, newdata=newdata)
            lines((dd$time[1]+newdata[[1]]*86400), P1, col=2, lwd=lwd)
        }
        points(dd$time, dd$total_vaccinations_per_hundred,
               cex=ifelse(focus, 1, 0.5),
               col=ifelse(focus, "black", "gray"))
        mtext(locations[ilocation], side=3, cex=par("cex"))
        if (!is.null(m1) && is.finite(yearsToAll1)) {
            mtext(sprintf(" Expect 2 vaccinations per\n person within %.1f years,\n based on linear fit\n over last 10 days.", yearsToAll1),
                  font=2, adj=0, line=-4, cex=0.9*par("cex"), col=2)
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

