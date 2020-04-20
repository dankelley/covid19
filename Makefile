SHELL=/bin/sh
regions="World" "Afghanistan" "Albania" "Algeria" "Andorra" "Angola" "Antigua and Barbuda" "Argentina" "Armenia" "Australia" "Austria" "Azerbaijan" "Bahamas" "Bahrain" "Bangladesh" "Barbados" "Belarus" "Belgium" "Belize" "Benin" "Bhutan" "Bolivia" "Bosnia and Herzegovina" "Botswana" "Brazil" "Brunei" "Bulgaria" "Burkina Faso" "Burundi" "Cape Verde" "Cambodia" "Cameroon" "Canada" "Central African Republic" "Chad" "Chile" "China" "Colombia" "Congo" "Congo, the Democratic Republic of the" "Costa Rica" "Cote d'Ivoire" "Croatia" "Cuba" "Cyprus" "Czech Republic" "Denmark" "Djibouti" "Dominica" "Dominican Republic" "Ecuador" "Egypt" "El Salvador" "Equatorial Guinea" "Eritrea" "Estonia" "Ethiopia" "Fiji" "Finland" "France" "Gabon" "Gambia" "Georgia" "Germany" "Ghana" "Greece" "Grenada" "Guatemala" "Guinea" "Guinea-Bissau" "Guyana" "Haiti" "Honduras" "Hungary" "Iceland" "India" "Indonesia" "Iran" "Iraq" "Ireland" "Israel" "Italy" "Jamaica" "Japan" "Jordan" "Kazakhstan" "Kenya" "Korea, South" "Kosovo" "Kuwait" "Kyrgyzstan" "Laos" "Latvia" "Lebanon" "Liberia" "Libya" "Liechtenstein" "Lithuania" "Luxembourg" "Madagascar" "Malawi" "Malaysia" "Maldives" "Mali" "Malta" "Mauritania" "Mauritius" "Mexico" "Moldova" "Monaco" "Mongolia" "Montenegro" "Morocco" "Mozambique" "MS Zaandam" "Myanmar" "Namibia" "Nepal" "Netherlands" "New Zealand" "Nicaragua" "Niger" "Nigeria" "Macedonia" "Norway" "Oman" "Pakistan" "Palestine" "Panama" "Papua New Guinea" "Paraguay" "Peru" "Philippines" "Poland" "Portugal" "Qatar" "Romania" "Russia" "Rwanda" "Saint Kitts and Nevis" "Saint Lucia" "Saint Vincent and the Grenadines" "San Marino" "Sao Tome and Principe" "Saudi Arabia" "Senegal" "Serbia" "Seychelles" "Sierra Leone" "Singapore" "Slovakia" "Slovenia" "Somalia" "South Africa" "South Sudan" "Spain" "Sri Lanka" "Sudan" "Suriname" "Sweden" "Switzerland" "Syria" "Tanzania" "Thailand" "Timor-Leste" "Togo" "Trinidad and Tobago" "Tunisia" "Turkey" "Uganda" "Ukraine" "United Arab Emirates" "United Kingdom" "Uruguay" "United States" "Uzbekistan" "Venezuela" "Vietnam" "Western Sahara" "Yemen" "Zambia" "Zimbabwe"
all: force
	/usr/local/bin/Rscript covid19.R $(regions)
	/usr/local/bin/Rscript canada.R
	date > last_updated.html
	chmod a+r last_updated.html
force:

clean:
	-rm -rf *~
	-rm -rf *png

