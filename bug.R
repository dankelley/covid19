library(COVID19)
d <- covid19(end=Sys.Date()-1)
cat("World:\n    ", max(d$deaths), "deaths\n")
for (country_name in c("Australia", "Canada", "United Kingdom", "United States")) {
  cat(country_name, ":\n", sep="")
  sub1 <- subset(d, country == country_name)
  cat("    method 1 reveals ", max(sub1$deaths), "deaths\n")
  sub2 <- d[d$country == country_name, ]
  cat("    method 2 reveals ", max(sub2$deaths), "deaths\n")
}

