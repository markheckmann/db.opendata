
# DB Station API


library(httr)
library(tidyverse)
library(RColorBrewer)
library(leaflet)
library(htmltools)
library(keyring)

# key_set("DB_FASTA_API_KEY")
# "0b811b7c10bee93e29f06eab5033a468"

# runs in WSL
# curl -X GET --header "Accept: application/json" --header "Authorization: Bearer 0b811b7c10bee93e29f06eab5033a468" "https://api.deutschebahn.com/stada/v2/stations"

key_list()

# Set Up
url = "https://api.deutschebahn.com/stada/v2/stations"
key = key_get("DB_FASTA_API_KEY")

# OPTION 1
headers = add_headers(Authorization = paste("Bearer", key, sep = " "))
r <- GET(url, config = headers)

http_status(r)
headers(r)


#json <- content(r, "text")  # as JSON
l <- content(r, "parsed")  # parsed from JSON
l$result %>% length        # all stations

x <- l$result


# convert non list elements to dataframe
station <- l$result[[1]]
ii <- !sapply(station, is.list)
nms <- names(ii)[ii]

l$result[[1]][nms]

flat <- lapply(l$result, function(l) {
  l %>% unlist %>% t %>% as.data.table
})
x <- bind_rows(flat)

pal <- brewer.pal(7, "RdYlBu")

x2 <- x %>%
  rename(
    lon = evaNumbers.geographicCoordinates.coordinates1,
    lat = evaNumbers.geographicCoordinates.coordinates2
  ) %>%
  mutate(
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    category = as.numeric(category),
    color = pal[category],
    hasWiFi = as.logical(hasWiFi),
    wlan = ifelse(hasWiFi, "Ja", "Nein"),
    hasDBLounge = as.logical(hasDBLounge),
    db_lounge = ifelse(hasDBLounge, "Ja", "Nein")
  )



# Analysis ------------------------------------------

x %>%
  count(federalState, category) %>%
  spread(category, n, fill = "-")
#dcast(federalState ~ category, value = "n",  fill = "-")

x %>%
  count(regionalbereich.name, category) %>%
  spread(category, n)

# how many stations with mobility servcice?
x %>%
  count(mobility_service = hasMobilityService != "no") %>%
  spread(mobility_service, n)



# Map ------------------------------------------

m <- leaflet()

lon <- range(x2$lon, na.rm = T)
lat <- range(x2$lat, na.rm = T)

popup = paste0( "Station: ", x2$name, "<br>",
                "Kategorie: ", x2$category, "<br>",
                "RB: ", x2$regionalbereich.name, "<br>",
                "BM: ", x2$stationManagement.name, "<br>",
                "Bundesland: ", x2$federalState, "<br>",
                "Aufgabenträger: ", x2$aufgabentraeger.name, "<br>",
                "WLAN: ", x2$wlan, "<br>",
                "DB Lounge:", x2$db_lounge)

m <- leaflet(x2) %>%
  addTiles() %>%
  fitBounds(lon[1], lat[1], lon[2], lat[2]) %>%
  addCircleMarkers(lng = ~ lon, lat = ~ lat, radius = 1, label = ~ name,
                   popup = popup) #, popupOptions = )
m








