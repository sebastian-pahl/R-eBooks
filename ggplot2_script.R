library("tidyverse")

################GENERAL############

#ggplot(data = <DATA>) + 
#        <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

###############STRAVA##############

ggplot(data = strava_data) +
        geom_path(mapping = aes(x = hours, y = elevation), colour = "blue")

x <- ggplot(data = strava_data, mapping = aes(x = longitude, y = latitude)) +
        geom_path(colour = "red")
ggplotly(x)

StravaMap <- get_googlemap(center = c(7.95, 48.02), zoom =11)
y = ggmap(StravaMap) +
        geom_point(data=strava_data, aes(longitude, latitude, colour=elevation), 
                   size = 1) +
        scale_color_gradient2(low = "yellow", high = "black", mid = "red", midpoint=500)
y
ggplotly(y)

ggmap(StravaMap) +
        geom_map(data=strava_data, aes(longitude, latitude), size = 2)
?geom_map

plot_ly(data = strava_data, x = longitude, y = latitude, z = elevation, color = pulse,
        colors = "YlOrRd", type="scatter3d")

#############MPG DATA###############

world <- map_data("world")
countries <- unique(world$region)
namibia = world[(world$region == "Namibia"),]
germany = world[(world$region == "Germany"),]
head(germany)
head(strava_data)
?openmap

library("OpenStreetMap")
library("ggplot2")
library("rJava")
library("ggmap")
library("getmap")
#c(56, 5), c(45, 15)

?get_googlemap
ggmap(StravaMap)
?ggplot2::color


#ggplot(data = <DATA>) + 
#        <GEOM_FUNCTION>(
#                mapping = aes(<MAPPINGS>),
#                stat = <STAT>, 
#                position = <POSITION>
#        ) +
#        <COORDINATE_FUNCTION> +
#        <FACET_FUNCTION>