

# visualize fasta

pal <- c("#f00", "#0f0", "#888")

x2 <- x %>%
  rename(
    lon = geocoordX,
    lat = geocoordY
  ) %>%
  mutate(
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    lon = ifelse(lon > 30, NA, lon),
    lat = ifelse(lat < 30, NA, lat),
    type = recode(type,
                  "ELEVATOR" = "Aufzug",
                  "ESCALATOR" = "Fahrtreppe"),
    state_de = recode(state,
                      "ACTIVE" = "Betriebsbereit",
                      "INACTIVE" = "Nicht betriebsbereit",
                      "UNKNOWN" = "unbekannt"),
    state_de_color = recode(state,
                            "ACTIVE" = "<span style = 'color:green;'>Betriebsbereit</span>",
                            "INACTIVE" = "<span style = 'color:red;'>Nicht betriebsbereit</span>",
                            "UNKNOWN" = "<span style = 'color:grey;'>Unbekannt</span>"),
    color = recode(state,
                   "ACTIVE" = "#0f0",
                   "INACTIVE" = "#f00",
                   "UNKNOWN" = "#999")
  )

x2$timestamp <- Sys.time()

# master <- x2
fwrite(x2, "data/master.csv", append = T)

x3 <- fread("data/master.csv")
dim(x3)
#master <- bind_rows(master, x2)


# keep latest different status



# Analysis ------------------------------------------

# x %>%
#   count(federalState, category) %>%
#   spread(category, n, fill = "-")
# #dcast(federalState ~ category, value = "n",  fill = "-")
#
# x %>%
#   count(regionalbereich.name, category) %>%
#   spread(category, n)
#
# # how many stations with mobility servcice?
# x %>%
#   count(mobility_service = hasMobilityService != "no") %>%
#   spread(mobility_service, n)



# Map ------------------------------------------

m <- leaflet()

lon <- range(x2$lon, na.rm = T)
lat <- range(x2$lat, na.rm = T)

popup = paste( "Equipmentnumber:", x2$equipmentnumber, "<br>",
               "Typ:", x2$type, "<br>",
               "Beschreibung:", x2$description, "<br>",
               "Zustand:", x2$state_de_color, "<br>",
               "Aktualisiert:", x2$timestamp, "<br>",
               "Erklärung:", x2$stateExplanation, "<br>",
               "Station:", x2$stationnumber)

# popup_html <- sapply(popup, function(label) {
#   htmltools::HTML(label) %>% unname
# })

g <- ggplot(mtcars, aes(hp, wt)) + geom_point()
img <- "https://upload.wikimedia.org/wikipedia/commons/thumb/6/62/Mount_Eden.jpg/640px-Mount_Eden.jpg"


x2$label <- popup
m <- leaflet(x2) %>%
  addTiles() %>%
  fitBounds(lon[1], lat[1], lon[2], lat[2]) %>%
  addCircleMarkers(lng = ~ lon, lat = ~ lat, radius = 5,
                   color = "#333", weight = 1, stroke = T, opacity = .9,
                   fill = T, fillOpacity = .9, fillColor = ~ color,
                   label =  ~ equipmentnumber,
                   popup =  popupImage(img, src = "remote"))
m



