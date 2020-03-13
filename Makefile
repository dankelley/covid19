regions="World" "Afghanistan" "Algeria" "Andorra" "Argentina" "Armenia" "Australia" "Austria" "Azerbaijan" "Bahrain" "Belarus" "Belgium" "Brazil" "Cambodia" "Canada" "Chile" "Croatia" "Denmark" "Dominican Republic" "Ecuador" "Egypt" "Estonia" "Finland" "France" "Georgia" "Germany" "Greece" "Hungary" "Iceland" "India" "Indonesia" "Iran" "Iraq" "Ireland" "Israel" "Italy" "Japan" "Jordan" "Kuwait" "Latvia" "Lebanon" "Liechtenstein" "Lithuania" "Luxembourg" "Macau" "Mainland China" "Malaysia" "Mexico" "Monaco" "Morocco" "Nepal" "Netherlands" "New Zealand" "Nigeria" "North Macedonia" "Norway" "Oman" "Others" "Pakistan" "Philippines" "Poland" "Portugal" "Qatar" "Romania" "Russia" "Saint Barthelemy" "San Marino" "Saudi Arabia" "Senegal" "Singapore" "South Korea" "Spain" "Sri Lanka" "Sweden" "Switzerland" "Taiwan" "Thailand" "Tunisia" "UK" "Ukraine" "United Arab Emirates" "US" "Vietnam"
all: force
	/usr/local/bin/Rscript covid19.R $(regions)
force:


