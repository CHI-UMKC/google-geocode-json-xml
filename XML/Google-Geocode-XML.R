# Google Geocoding:  XML get.geocode function
# Earl F Glynn, Franklin Center for Government & Public Integrity, 2010-12-15.
# Modified 2012-01-06: extracted additional fields.

library(XML)     # htmlTreeParse

################################################################################

# Function to call Google's Geocoding API for given street, city and state.
# "id" could be used to add a "key" to the data record being processed.
get.geocode <- function (id, street, city, state, zip)
{
  address <- paste(street, city, state, zip, sep=", ")
  URL <- paste("http://maps.googleapis.com/maps/api/geocode/xml?address=",
               gsub(" ", "+", address), sep="")
  xml <- htmlTreeParse(URL, useInternal=TRUE)
  status <- unlist(xpathApply(xml, "//status", xmlValue))

  # if more than one, pick only the first in result[1]
  if (status == "OK")
  {
    types <- unlist(xpathApply(xml, "//result[1]/type", xmlValue))
    if (is.null(types)) types <- ""

    results <- getNodeSet(xml, "//result")
    result.count <- length(results)     # study why this is ever > 1

    formatted.address <- unlist(xpathApply(xml, "//result[1]/formatted_address", xmlValue))
    if (is.null(formatted.address)) formatted.address <- ""

    # Add prefix "geo" to fields that were inputs.  This allows later comparison of
    # inputs and outputs if ever necessary.

    street_number <- unlist(xpathApply(xml, "//result[1]/address_component[* = 'street_number']/short_name", xmlValue))
    if (is.null(street_number)) street_number <- ""
    route <- unlist(xpathApply(xml, "//result[1]/address_component[* = 'route']/short_name", xmlValue))
    if (is.null(route)) route <- ""
    geostreet <- paste(street_number, route)

    geocity <- unlist(xpathApply(xml, "//result[1]/address_component[* = 'locality']/long_name", xmlValue))
    if (is.null(geocity)) geocity <- ""

    geostate <- unlist(xpathApply(xml, "//result[1]/address_component[* = 'administrative_area_level_1']/short_name", xmlValue))
    if (is.null(geostate)) geostate <- ""

    geozip5 <- unlist(xpathApply(xml, "//result[1]/address_component[* = 'postal_code']/short_name", xmlValue))
    if (is.null(geozip5)) geozip5 <- ""

    geozip4 <- unlist(xpathApply(xml, "//result[1]/address_component[* = 'postal_code_suffix']/short_name", xmlValue))
    if (is.null(geozip4)) geozip4 <- ""

    county <- unlist(xpathApply(xml, "//result[1]/address_component[* = 'administrative_area_level_2']/short_name", xmlValue))
    if (is.null(county)) county <- ""

    establishment <- unlist(xpathApply(xml, "//result[1]/address_component[* = 'establishment']/long_name", xmlValue))
    if (is.null(establishment)) establishment <- ""

    lat <- unlist(xpathApply(xml, "//result[1]/geometry/location/lat", xmlValue))
    if (is.null(lat)) lat <- ""
    lng <- unlist(xpathApply(xml, "//result[1]/geometry/location/lng", xmlValue))
    if (is.null(lng)) lng <- ""

    location.type <- unlist(xpathApply(xml, "//result[1]/geometry/location_type", xmlValue))
    if (is.null(location.type)) location.type <- ""
  } else {
    types <- ""
    formatted.address <- ""
    geostreet <- ""
    geocity <- ""
    geostate <- ""
    geozip5 <- ""
    geozip4 <- ""
    county <- ""
    establishment <- ""
    lat <- ""
    lng <- ""
    location.type <- ""
    result.count <- 0
  }

  free(xml)

  data.frame(id, status,
             street=street, city=city, state=state, zip=zip,
             geostreet, geocity, geostate, geozip5, geozip4, county,
             lat, lng, location.type,
             formatted.address, establishment,
             types=paste(types, collapse="|"),
             result.count, row.names=1,
             stringsAsFactors=FALSE)
}
