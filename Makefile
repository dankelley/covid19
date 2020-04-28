SHELL=/bin/sh
regions="World" "Afghanistan" "Albania" "Algeria" "Andorra" "Angola" "Antigua and Barbuda" "Argentina" "Armenia" "Australia" "Austria" "Azerbaijan" "Bahamas" "Bahrain" "Bangladesh" "Barbados" "Belarus" "Belgium" "Belize" "Benin" "Bhutan" "Bolivia" "Bosnia and Herzegovina" "Botswana" "Brazil" "Brunei" "Bulgaria" "Burkina Faso" "Burundi" "Cabo Verde" "Cambodia" "Cameroon" "Canada" "Central African Republic" "Chad" "Chile" "China" "Colombia" "Congo (Brazzaville)" "Congo (Kinshasa)" "Costa Rica" "Cote d'Ivoire" "Croatia" "Cuba" "Cyprus" "Czechia" "Denmark" "Djibouti" "Dominica" "Dominican Republic" "Ecuador" "Egypt" "El Salvador" "Equatorial Guinea" "Eritrea" "Estonia" "Ethiopia" "Fiji" "Finland" "France" "Gabon" "Gambia" "Georgia" "Germany" "Ghana" "Greece" "Grenada" "Guatemala" "Guinea" "Guinea-Bissau" "Guyana" "Haiti" "Honduras" "Hungary" "Iceland" "India" "Indonesia" "Iran" "Iraq" "Ireland" "Israel" "Italy" "Jamaica" "Japan" "Jordan" "Kazakhstan" "Kenya" "Korea, South" "Kuwait" "Kyrgyzstan" "Latvia" "Lebanon" "Liberia" "Libya" "Liechtenstein" "Lithuania" "Luxembourg" "Madagascar" "Malawi" "Malaysia" "Maldives" "Mali" "Malta" "Mauritania" "Mauritius" "Mexico" "Moldova" "Monaco" "Mongolia" "Montenegro" "Morocco" "Mozambique" "Myanmar" "Namibia" "Nepal" "Netherlands" "New Zealand" "Nicaragua" "Niger" "Nigeria" "North Macedonia" "Norway" "Oman" "Pakistan" "Panama" "Papua New Guinea" "Paraguay" "Peru" "Philippines" "Poland" "Portugal" "Qatar" "Romania" "Russia" "Rwanda" "Saint Kitts and Nevis" "Saint Lucia" "Saint Vincent and the Grenadines" "San Marino" "Sao Tome and Principe" "Saudi Arabia" "Senegal" "Serbia" "Seychelles" "Sierra Leone" "Singapore" "Slovakia" "Slovenia" "Somalia" "South Africa" "South Sudan" "Spain" "Sri Lanka" "Sudan" "Suriname" "Sweden" "Switzerland" "Syria" "Tanzania" "Thailand" "Timor-Leste" "Togo" "Trinidad and Tobago" "Tunisia" "Turkey" "Uganda" "Ukraine" "United Arab Emirates" "United Kingdom" "Uruguay" "US" "Uzbekistan" "Venezuela" "Vietnam" "Western Sahara" "Yemen" "Zambia" "Zimbabwe"
all: force
	/usr/local/bin/Rscript covid19.R $(regions) # does not use COVID19 package FIXME: world
	/usr/local/bin/Rscript canada.R
	/usr/local/bin/Rscript usa.R # FIXME: drop COVID19 package usage
	/usr/local/bin/Rscript tests_per_pop.R # uses COVID19 package
	date > last_updated.html
	chmod a+r last_updated.html
force:

clean:
	-rm -rf *~
	-rm -rf *png

