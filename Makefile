regions="World" "Afghanistan" "Algeria" "Albania" "Andorra" "Angola" "Antigua and Barbuda" "Argentina" "Armenia" "Australia" "Austria" "Azerbaijan" "Bahamas" "Bahrain" "Bangladesh" "Barbados" "Belarus" "Belgium" "Belize" "Benin" "Bhutan" "Brazil" "Cambodia" "Canada" "Chile" "China" "Colombia" "Croatia" "Cuba" "Denmark" "Dominican Republic" "Ecuador" "Egypt" "Estonia" "Finland" "France" "Georgia" "Germany" "Greece" "Hungary" "Iceland" "India" "Indonesia" "Iran" "Iraq" "Ireland" "Israel" "Italy" "Japan" "Jamaica" "Jordan" "Kuwait" "Latvia" "Lebanon" "Liechtenstein" "Lithuania" "Luxembourg" "Korea, South" "Malaysia" "Mexico" "Monaco" "Morocco" "Nepal" "Netherlands" "New Zealand" "Nigeria" "North Macedonia" "Norway" "Oman" "Pakistan" "Peru" "Philippines" "Poland" "Portugal" "Qatar" "Romania" "Russia" "San Marino" "Saudi Arabia" "Senegal" "Singapore" "South Africa" "Slovenia" "Spain" "Sri Lanka" "Sweden" "Switzerland" "Syria" "Thailand" "Tunisia" "Turkey" "United Kingdom" "Ukraine" "United Arab Emirates" "United Kingdom" "US" "Vietnam"
all: force
	#/usr/local/bin/Rscript covid19.R World "Taiwan*" $(regions)
	##/usr/local/bin/Rscript covid19b.R $(regions)
	/usr/local/bin/Rscript covid19.R $(regions) # does not have 'Taiwan'
	/usr/local/bin/Rscript canada.R
	#/usr/local/bin/Rscript canada_new.R
force:


