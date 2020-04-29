## Download population data from UN, rename some things, then save in an RDA file.
if (!file.exists("un_population_data.csv"))
    download.file("https://data.un.org/_Docs/SYB/CSV/SYB62_1_201907_Population,%20Surface%20Area%20and%20Density.csv",
                  "un_population_data.csv")
censusUrl <- "https://data.un.org/_Docs/SYB/CSV/SYB62_1_201907_Population,%20Surface%20Area%20and%20Density.csv"
census <- read.csv(censusUrl, header=FALSE, skip=2, encoding="latin1")
names(census) <- c("Key", "place", "year", "category", "value", "x", "source")
target <- "Population mid-year estimates \\(millions\\)"
##TEST ## the tail() is to get the most recent value
##TEST canada <- tail(census[grepl("Canada", census$place) & grepl(target, census$category), ],1)
##TEST 1e6 * canada$value

places <- unique(census$place)
populations <- lapply(places,
                      function(p)
                          1e6 * tail(census[grepl(p, census$place) & grepl(target, census$category), ],1)$value
                      )
population <- list()
population[places] <- populations

## Rename some countries to the names used here (many of which were very
## provided by D. Morrison).
names(population) <- gsub("Bolivia \\(Plurin. State of\\)", "Bolivia", names(population))
names(population) <- gsub("United States of America", "United States", names(population))
names(population) <- gsub("Brunei Darussalam", "Brunei", names(population))
names(population) <- gsub("Côte d’Ivoire", "Cote d'Ivoire", names(population))
names(population) <- gsub("Iran \\(Islamic Republic of\\)", "Iran", names(population))
names(population) <- gsub("Republic of Korea", "Korea, South", names(population))
names(population) <- gsub("Republic of Moldova", "Moldova", names(population))
names(population) <- gsub("Russian Federation", "Russia", names(population))
names(population) <- gsub("Saint Vincent \\& Grenadines", "Saint Vincent and the Grenadines", names(population))
names(population) <- gsub("Syrian Arab Republic", "Syria", names(population))
names(population) <- gsub("Venezuela \\(Boliv. Rep. of\\)", "Venezuela", names(population))
names(population) <- gsub("Viet Nam", "Vietnam", names(population))
names(population) <- gsub("United Rep. of Tanzania", "Tanzania", names(population))
names(population) <- gsub("Dem. Rep. of the Congo", "Congo (Kinshasa)", names(population))
names(population) <- gsub("^Congo$", "Congo (Brazzaville)", names(population))
names(population) <- gsub("State of Palestine", "Palestine", names(population))
names(population) <- gsub("United States", "US", names(population))

## world (summing gives 27 billion, so must be double-counting things)
population[["World"]] <- 7781073029    # https://www.worldometers.info/world-population/

## Insert Canadian provinces
#population[["Canada"]] <- 37894799
population[["Ontario"]] <- 14446515
population[["Quebec"]] <- 8433301
population[["British Columbia"]] <- 5020302
population[["Alberta"]] <- 4345737
population[["Manitoba"]] <- 1360396
population[["Saskatchewan"]] <- 1168423
population[["Nova Scotia"]] <- 965382
population[["New Brunswick"]] <- 772094
population[["Newfoundland and Labrador"]] <- 523790
population[["Prince Edward Island"]] <- 154748

save(population, file="population.rda")


## Below is a list of raw names.
## sink('a');places;sink()
## 
##   [1] "Total, all countries or areas" "Africa"                        "Northern Africa"              
##   [4] "Sub-Saharan Africa"            "Eastern Africa"                "Middle Africa"                
##   [7] "Southern Africa"               "Western Africa"                "Americas"                     
##  [10] "Northern America"              "Latin America & the Caribbean" "Caribbean"                    
##  [13] "Central America"               "South America"                 "Asia"                         
##  [16] "Central Asia"                  "Eastern Asia"                  "South-central Asia"           
##  [19] "South-eastern Asia"            "Southern Asia"                 "Western Asia"                 
##  [22] "Europe"                        "Eastern Europe"                "Northern Europe"              
##  [25] "Southern Europe"               "Western Europe"                "Oceania"                      
##  [28] "Australia and New Zealand"     "Melanesia"                     "Micronesia"                   
##  [31] "Polynesia"                     "Afghanistan"                   "Albania"                      
##  [34] "Algeria"                       "American Samoa"                "Andorra"                      
##  [37] "Angola"                        "Anguilla"                      "Antigua and Barbuda"          
##  [40] "Argentina"                     "Armenia"                       "Aruba"                        
##  [43] "Australia"                     "Austria"                       "Azerbaijan"                   
##  [46] "Bahamas"                       "Bahrain"                       "Bangladesh"                   
##  [49] "Barbados"                      "Belarus"                       "Belgium"                      
##  [52] "Belize"                        "Benin"                         "Bermuda"                      
##  [55] "Bhutan"                        "Bolivia (Plurin. State of)"    "Bonaire, St. Eustatius & Saba"
##  [58] "Bosnia and Herzegovina"        "Botswana"                      "Brazil"                       
##  [61] "British Virgin Islands"        "Brunei Darussalam"             "Bulgaria"                     
##  [64] "Burkina Faso"                  "Burundi"                       "Cabo Verde"                   
##  [67] "Cambodia"                      "Cameroon"                      "Canada"                       
##  [70] "Cayman Islands"                "Central African Republic"      "Chad"                         
##  [73] "Channel Islands"               "Chile"                         "China"                        
##  [76] "China, Hong Kong SAR"          "China, Macao SAR"              "Colombia"                     
##  [79] "Comoros"                       "Congo"                         "Cook Islands"                 
##  [82] "Costa Rica"                    "Côte d’Ivoire"                 "Croatia"                      
##  [85] "Cuba"                          "Curaçao"                       "Cyprus"                       
##  [88] "Czechia"                       "Dem. People's Rep. Korea"      "Dem. Rep. of the Congo"       
##  [91] "Denmark"                       "Djibouti"                      "Dominica"                     
##  [94] "Dominican Republic"            "Ecuador"                       "Egypt"                        
##  [97] "El Salvador"                   "Equatorial Guinea"             "Eritrea"                      
## [100] "Estonia"                       "Eswatini"                      "Ethiopia"                     
## [103] "Falkland Islands (Malvinas)"   "Faroe Islands"                 "Fiji"                         
## [106] "Finland"                       "France"                        "French Guiana"                
## [109] "French Polynesia"              "Gabon"                         "Gambia"                       
## [112] "Georgia"                       "Germany"                       "Ghana"                        
## [115] "Gibraltar"                     "Greece"                        "Greenland"                    
## [118] "Grenada"                       "Guadeloupe"                    "Guam"                         
## [121] "Guatemala"                     "Guinea"                        "Guinea-Bissau"                
## [124] "Guyana"                        "Haiti"                         "Holy See"                     
## [127] "Honduras"                      "Hungary"                       "Iceland"                      
## [130] "India"                         "Indonesia"                     "Iran (Islamic Republic of)"   
## [133] "Iraq"                          "Ireland"                       "Isle of Man"                  
## [136] "Israel"                        "Italy"                         "Jamaica"                      
## [139] "Japan"                         "Jordan"                        "Kazakhstan"                   
## [142] "Kenya"                         "Kiribati"                      "Kuwait"                       
## [145] "Kyrgyzstan"                    "Lao People's Dem. Rep."        "Latvia"                       
## [148] "Lebanon"                       "Lesotho"                       "Liberia"                      
## [151] "Libya"                         "Liechtenstein"                 "Lithuania"                    
## [154] "Luxembourg"                    "Madagascar"                    "Malawi"                       
## [157] "Malaysia"                      "Maldives"                      "Mali"                         
## [160] "Malta"                         "Marshall Islands"              "Martinique"                   
## [163] "Mauritania"                    "Mauritius"                     "Mayotte"                      
## [166] "Mexico"                        "Micronesia (Fed. States of)"   "Monaco"                       
## [169] "Mongolia"                      "Montenegro"                    "Montserrat"                   
## [172] "Morocco"                       "Mozambique"                    "Myanmar"                      
## [175] "Namibia"                       "Nauru"                         "Nepal"                        
## [178] "Netherlands"                   "New Caledonia"                 "New Zealand"                  
## [181] "Nicaragua"                     "Niger"                         "Nigeria"                      
## [184] "Niue"                          "North Macedonia"               "Northern Mariana Islands"     
## [187] "Norway"                        "Oman"                          "Other non-specified areas"    
## [190] "Pakistan"                      "Palau"                         "Panama"                       
## [193] "Papua New Guinea"              "Paraguay"                      "Peru"                         
## [196] "Philippines"                   "Poland"                        "Portugal"                     
## [199] "Puerto Rico"                   "Qatar"                         "Republic of Korea"            
## [202] "Republic of Moldova"           "Réunion"                       "Romania"                      
## [205] "Russian Federation"            "Rwanda"                        "Saint Barthélemy"             
## [208] "Saint Helena"                  "Saint Kitts and Nevis"         "Saint Lucia"                  
## [211] "Saint Martin (French part)"    "Saint Pierre and Miquelon"     "Saint Vincent & Grenadines"   
## [214] "Samoa"                         "San Marino"                    "Sao Tome and Principe"        
## [217] "Saudi Arabia"                  "Senegal"                       "Serbia"                       
## [220] "Seychelles"                    "Sierra Leone"                  "Singapore"                    
## [223] "Sint Maarten (Dutch part)"     "Slovakia"                      "Slovenia"                     
## [226] "Solomon Islands"               "Somalia"                       "South Africa"                 
## [229] "South Sudan"                   "Spain"                         "Sri Lanka"                    
## [232] "State of Palestine"            "Sudan"                         "Suriname"                     
## [235] "Sweden"                        "Switzerland"                   "Syrian Arab Republic"         
## [238] "Tajikistan"                    "Thailand"                      "Timor-Leste"                  
## [241] "Togo"                          "Tokelau"                       "Tonga"                        
## [244] "Trinidad and Tobago"           "Tunisia"                       "Turkey"                       
## [247] "Turkmenistan"                  "Turks and Caicos Islands"      "Tuvalu"                       
## [250] "Uganda"                        "Ukraine"                       "United Arab Emirates"         
## [253] "United Kingdom"                "United Rep. of Tanzania"       "United States of America"     
## [256] "United States Virgin Islands"  "Uruguay"                       "Uzbekistan"                   
## [259] "Vanuatu"                       "Venezuela (Boliv. Rep. of)"    "Viet Nam"                     
## [262] "Wallis and Futuna Islands"     "Western Sahara"                "Yemen"                        
## [265] "Zambia"                        "Zimbabwe"                     
