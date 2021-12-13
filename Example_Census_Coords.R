library(censusxy)
library(sf)

Data <- [Your Data Here]

# Make a subset with investigation ID and address for positive cases
posOnly <- Data[Data$Case_Status == "Confirmed",]
add <- posOnly %>% select(Street_Address, City, State, Zip, InvestigationID)

# Remove addresses with NA values (NAs make geocoder very slow and less accurate)
add <- add[!is.na(add[1]),]
add <- add[!is.na(add[2]),]
add <- add[!is.na(add[3]),]
add <- add[!is.na(add[4]),]

# Remove PO Boxes and other desired addresses to be excluded
exclusion_Addresses <- "(^100 MACK|^400 MACK|PO Box|P O BOX|P.O. BOX|Unknown|homeless|^2799 (W|WEST)*.*GRAND B|^22101 MOROSS R|^4646 JOHN R R|1 ford pl|one energy pl|1 energy pl|^5635 (W|WEST)*.*FORT S|^2 WOODWARD|^500 GRISWOLD S|^4201 (ST|SAINT) ANTOINE S|^4646 JOHN R S|^570 CLINTON S|^3456 (W|WEST)*.*VERNOR H|^6071 (W|WEST)*.*OUTER DR|^16238 (W|WEST)*.*WARREN A|^600 (E|EAST)*.*LAFAYETTE B|^23 (E|EAST)*.* ADAMS AVE|^4880 LAWNDALE S|^17600 RYAN R)"
add <- add[!grepl(exclusion_Addresses, add$Street_Address, ignore.case = TRUE),]

# Get coords for street addresses by pinging the USA census data
addresses <- cxy_geocode(add, street = "Street_Address", city = "City", state = "State", output = "simple")

# bind latittude, longitude, and investigation ID into a dataframe, then join back into finalData
coords <- addresses %>% select(InvestigationID, cxy_lat, cxy_lon) %>% rename(
  Lattitude = cxy_lat,
  Longitude = cxy_lon
)
Data <- left_join(Data, coords, by = "InvestigationID")
