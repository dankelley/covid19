library(COVID19)
old <- world("country")
new <- covid19()
for (country in c("Canada", "United States")) {
    cat("#", country, "\n")
    print(tail(old[old$country == country, ], 3))
    print(tail(new[new$country == country, ], 3))
}

