

library(leaflet)
library(rgdal)
library(rmapshaper)
library(raster)


#http://www.jeremy-oakley.staff.shef.ac.uk/mas61004/EDAtutorial/maps-with-leaflet.html
#boundaries <- rgdal::readOGR(dsn = "shape/Counties_and_Unitary_Authorities_(December_2016)_Boundaries",
#                             layer = "Counties_and_Unitary_Authorities_(December_2016)_Boundaries" )

#boundaries_1 <- rgdal::readOGR(dsn = "shape/Great_Britain_shapefile",
#                             layer = "gb_100km" )
#simplify the file boundaries
#simplifiedBoundaries <-rmapshaper::ms_simplify(boundaries)

#simplifiedBoundaries_1 <-rmapshaper::ms_simplify(boundaries_1)

#plot_boundries<-function(boundries_data=simplifiedBoundaries){
  #
#  leaflet() %>%
#    setView(lng = -1.47, lat = 52.3, zoom = 6) %>%
#    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
#    addPolygons(data = boundries_data ,
#                color = "blue",
#                fillOpacity = 0,
#                weight  = 1,
#                popup = ~ctyua16nm)
#  
#}

#plot_boundries(simplifiedBoundaries_1)

# 1.0 download the map
#https://gadm.org/download_country.html
### UK_1 <- raster::getData("GADM", country= "GBR", level=1)
# Simplyfiy
### UK_s<- rmapshaper::ms_simplify(UK_1)
# Convert to sf object via st_as_sf() -> results in multipolygon
###bp_sf <- sf::st_as_sf(UK_s)
# object can be subdivided into single polygons using st_cast()
###bps_sf <- sf::st_cast(bp_sf, "POLYGON")
## write to shap file
#sf::st_write(bp_sf , "data/my_shapefile.shp")


#filter on area
#BpSf <- bps_sf[as.numeric(sf::st_area(bps_sf))>=10000,]




plot_uk_map<-function(regional.filter=c("England","Wales","Scotland","Northern Ireland")){
#subset data
bps_sf<-rgdal::readOGR(dsn = "data/my_shapefile.shp")

BpSf <- bps_sf[bps_sf$NAME_1 %in% regional.filter,]

colour.df<-data.frame(region=c("England","Northern Ireland","Scotland","Wales"),
                     col=c("#b7352d", "#2a6b8f", "#0f4461", "#26aef8") )  
  
#colour.df<-colour.df[colour.df$region %in% regional.filter, ]

# 2. create a color vector
pal <- colorFactor(palette = colour.df$col, domain = BpSf$NAME_1 )



# 3. show  the map
#https://rquer.netlify.app/leaflet_maps_second_part/
BpSf  %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldShadedRelief) %>%
  addPolygons(weight = 1,
              stroke = TRUE,
              color = "white",
              fillColor = ~pal(NAME_1),
              fillOpacity = 0.7,
              dashArray = "3",
              label = ~NAME_1,
              popup = ~paste("State/Union Territory:", NAME_1,
                             "<br/>",
                             "Country:", NAME_0),
              highlight = highlightOptions(
                weight = 2,
                dashArray = "",
                color = "grey",
                bringToFront = TRUE
              )) %>%
  addLegend("topleft", pal = pal, values = ~NAME_1,
            title = "UK regions",
            labFormat = labelFormat(prefix = "-"),
            opacity = 1
  )

}
