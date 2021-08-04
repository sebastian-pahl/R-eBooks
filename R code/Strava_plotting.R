

#------------PLOT3D-------------------------------------------------------------

#3D with coordinates on x and y axis, and elevation on z axis
#need packages:
#library("plot3D")
#library("plot3Drgl") #also need this (but just once off): library("rJava")

scatter3D(x = strava_data$latitude, y = strava_data$longitude, z = strava_data$elevation, 
          colvar = pmax(150, pmin(300, strava_data$power)),
          theta=15,  #viewing direction: increasing theta shifts image to the left 
          phi=15,    #viewing direction: increasing phi gives more top view
          bty="g",   #creates an empty perspective box
          type="l",  #or "l" for lines; or "h" for histogram
          lwd=5,
          main= "3D Elevation Plot",
          xlab = "Longitude", ylab = "Latitude", zlab = "Elevation", clab = "Power")
#interactive version of plot
plotrgl()


#-----------------------------------------------------------------------

#ELEVATION PLOTS

#ggplot: elevation vs distance OR time 
#for plotting vs time, change x to hours in aes() and change x label in labs()

library("geosphere")
meters <- distCosine(strava_data[3:4])
distance_function <- function(m) {
        
        distance <- as.numeric()
        distance[1] = 0
        distance[2] = m[1]
        
        for(i in 1:(length(m)-1)) {
                distance[i+2] = distance[i+1] + m[i+1]
        }
        distance/1000
}
distance_km <- distance_function(meters)        
strava_data <- data.frame(strava_data, distance_km)

elevation_total = sum(pmax(0, strava_data$elevation[-1]-head(strava_data$elevation,-1)))
distance_total = round(distance_km[length(distance_km)],2)
library("lubridate")
lubri_time = seconds_to_period(hours[length(hours)]*60*60)

ggplot(data = strava_data) +
        geom_point(mapping = aes(x = distance_km, y = elevation, 
                                 colour = pmax(100, pmin(300, power))), 
                   size=2) +
        #theme_bw() +
        scale_x_continuous() +
        labs(x = "Distance in km", y = "Elevation in meters", title = "Elevation Plot", 
             subtitle = paste(" Total elevation gain =", elevation_total, "m\n",
                              "Total distance =", distance_total, "km\n",
                              "Total time =", lubri_time),
             colour = "Power in watts"
        ) +
        scale_colour_continuous(type = "viridis", direction=-1) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5))
        

---------------------------------------------------------------------

#COORDINATE PLOTS

#coordinates plotted on google maps
library("ggmap")
register_google(key = "<key>") #generate own Maps API on Google Cloud Platform
StravaMap <- get_googlemap(center = c(mean(strava_data$longitude), mean(strava_data$latitude)),
                           zoom =11)
ggmap(StravaMap) +
        geom_point(data=strava_data, aes(longitude, latitude, colour=elevation), 
                   size = 1) +
        scale_color_gradient2(low = "yellow", high = "black", mid = "red", midpoint=500)
#ggplotly(y)  #can use ggplotly to create interactive plot in Viewer, but quite laggy

library("plotly")
x <- plot_ly(data = strava_data, 
        x = strava_data$latitude, y = strava_data$longitude, z = strava_data$elevation, 
        color = pmax(100, pmin(300, strava_data$power)),
        #colorscale = "Viridis", colorbar = list(title = "power"),
        type="scatter3d", size=3,
        marker=list(
                colorbar=list(title='Power'),
                colorscale='Viridis',
                reversescale = T
        )
)

layout(x, title = "Strava interactive plotly",
       scene = list(
               xaxis = list(title = "latitude"),
               yaxis = list(title = "longitude"),
               zaxis = list(title = "elevation")
               )
       )


#-----------------MPG DATA--------------------------------------


world <- map_data("world")
countries <- unique(world$region)
namibia = world[(world$region == "Namibia"),]
germany = world[(world$region == "Germany"),]
head(germany)
head(strava_data)
?openmap

#ggplot(data = <DATA>) + 
#        <GEOM_FUNCTION>(
#                mapping = aes(<MAPPINGS>),
#                stat = <STAT>, 
#                position = <POSITION>
#        ) +
#        <COORDINATE_FUNCTION> +
#        <FACET_FUNCTION>