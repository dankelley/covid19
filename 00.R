library(COVID19)
d <- covid19(end=Sys.Date()-1)
cat("World:\n    ", max(d$deaths), "deaths\n")
for (country in c("Australia", "Canada", "United Kingdom", "United States")) {
    cat(country, ":\n", sep="")
    sub1 <- subset(d, d$country == country)
    cat("    method 1 reveals ", max(sub1$deaths), "deaths\n")
    sub2 <- d[d$country == country, ]
    cat("    method 2 reveals ", max(sub2$deaths), "deaths\n")
}


