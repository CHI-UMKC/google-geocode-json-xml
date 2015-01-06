# XML Google Geocoding tests
# efg, UMKC Center for Health Insights, 2014-01-05

setwd( "C:/2015/R/Geocoding-Google-JSON-XML/XML/")
sink("Google-Geocode-Example-XML.txt", split=TRUE)

source("Google-Geocode-XML.R")

# No ZIP
d <- get.geocode("IDNumber", "2411 Holmes St", "Kansas City", "MO", "")
d

# With ZIP
id     <- "IDNumber"
street <- "2411 Holmes St"
city   <- "Kansas City"
state  <- "MO"
zip    <- "64108"
d <- get.geocode(id, street, city, state, zip)
t(d)

sink()

