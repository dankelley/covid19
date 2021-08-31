url <- "https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv"
d <- read.csv(url, header=TRUE)

if (!interactive())
    png("vaccine-world_%03d.png", unit="in", width=7, height=5, res=150, pointsize=10)
palette("Set 2")#;plot(1:6,pch=20,cex=2,col=1:6)
par(mar=c(2,3,1,1), mgp=c(2,0.7,0))
#regions <- c("Canada", "France", "United Kingdom", "US")
M <- 6 # number per plot
#regions <- sort(unique(d$Country_Region))[1:(2*M)]
regions <- sort(unique(d$Country_Region))[1:3]
tlim <- as.POSIXct(c("2021-01-01", format(Sys.Date()+14)), tz="UTC")
for (i in seq_along(regions)) {
    col <- 1 + (i - 1)%%M
    cat("i=", i, ", col=", col, "\n", sep="")
    sub <- d[d$Country_Region == regions[i] & 0 == nchar(d$Province_State),]
    pop <- sub[1,12]
    data <- sub[1, -(1:13)]
    time <- as.POSIXct(gsub("\\.","-",gsub("^X","", names(data))), tz="UTC")
    vac <- as.vector(unlist(data))
    vacPerPop <- 100 * vac / pop
    vacPerPop2 <- vacPerPop / 2
    #cat("pop=", pop, " ", paste(sub[1,1:15],collapse=" "), "\n")
    #cat("length(time)=", length(time), ", length(vacPerPop)=", length(vacPerPop),"\n")
    if (col == 1L) {
        plot(time, vacPerPop2, xlab="", ylab="Double Vaccination per Population",
            xlim=tlim, ylim=c(0,100),
            yaxs="i", type="o", pch=20, cex=0.5, col=col)
        legend("topleft", col=1:M, pch=20, pt.cex=0.5, lwd=2*par("lwd"), legend=regions[i-1+1:M])
    } else {
        lines(time, vacPerPop2, col=col, type="o", pch=20, cex=0.5)
    }
}
if (!interactive())
    dev.off()

