## Create population.rda holding population=data.frame(place,number)
##
## NOTE: we do *not* download, because the dataset should be constant on the
## scale of this applicaton, and also because of the pain of having to recode
## if the (undocumented) data format changes.
##
## The data source is a cvs generated locally from the UN source
## https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx
rm(list=ls())
d <- read.csv("WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.csv", skip=39, header=FALSE)
place <- d[, 3]
number <- gsub(" ", "", d[,78])
bad <- grep("[0-9]+", number, invert=TRUE)
if (length(bad)) {
    message("removing ", length(bad), " lines, because population had non-numeric characters")
    place <- place[-bad]
    d <- d[-bad,]
}
number <- as.numeric(number)
## Convert to names used in covid19 database
place <- gsub("Bolivia \\(Plurinational State of\\)", "Bolivia", place)
place <- gsub("Brunei Darussalam", "Brunei", place)
place <- gsub("^Congo$", "Congo \\(Brazzaville\\)", place)
place <- gsub("^Democratic Republic of the Congo$", "Congo (Kinshasa)", place)
place <- gsub("China, Taiwan Province of China", "Taiwan*", place)
## I am not sure how to work with accents
## place <- gsub("Côte d'Ivoire", "Cote 'Ivoire", place, useBytes=TRUE)
place[grep("Ivoire", place)] <- "Cote d'Ivoire"
place <- gsub("Iran \\(Islamic Republic of\\)", "Iran", place)
place <- gsub("Republic of Korea", "Korea, South", place)
place <- gsub("Republic of Moldova", "Moldova", place)
place <- gsub("Russian Federation", "Russia", place)
place <- gsub("Syrian Arab Republic", "Syria", place)
place <- gsub("United Republic of Tanzania", "Tanzania", place)
place <- gsub("United States of America", "US", place)
place <- gsub("Venezuela \\(Bolivarian Republic of\\)", "Venezuela", place)
place <- gsub("Viet Nam", "Vietnam", place)
number <- number[-bad]
number <- 1000*as.numeric(number)
stopifnot(length(place) == length(number))
population <- data.frame(place=c(place, "World"), number=c(number, 7.8e9))

## subset(population, place=="Canada")
## subset(population, place=="US")
## subset(population, place=="United Kingdom")

regions <- c("World", "Afghanistan", "Algeria", "Albania", "Andorra", "Angola",
             "Antigua and Barbuda", "Argentina", "Armenia", "Australia",
             "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh",
             "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan",
             "Brazil", "Cambodia", "Canada", "Chile", "China", "Colombia",
             "Croatia", "Cuba", "Denmark", "Dominican Republic", "Ecuador",
             "Egypt", "Estonia", "Finland", "France", "Georgia", "Germany",
             "Greece", "Hungary", "Iceland", "India", "Indonesia", "Iran",
             "Iraq", "Ireland", "Israel", "Italy", "Japan", "Jamaica",
             "Jordan", "Kuwait", "Latvia", "Lebanon", "Liechtenstein",
             "Lithuania", "Luxembourg", "Korea, South", "Malaysia",
             "Mexico", "Monaco", "Morocco", "Nepal", "Netherlands",
             "New Zealand", "Nigeria", "North Macedonia", "Norway", "Oman",
             "Pakistan", "Peru", "Philippines", "Poland", "Portugal", "Qatar",
             "Romania", "Russia", "San Marino", "Saudi Arabia",
             "Senegal", "Singapore", "South Africa", "Slovenia", "Spain",
             "Sri Lanka", "Sweden", "Switzerland", "Syria", "Taiwan*",
             "Thailand", "Tunisia", "Turkey", "United Kingdom", "Ukraine",
             "United Arab Emirates", "United Kingdom", "US",
             "Vietnam")
## Next test if we find all
a <- sapply(regions, function(r) any(population$place==r))
stopifnot(sum(!a) == 0)
save(population, file="population.rda")
