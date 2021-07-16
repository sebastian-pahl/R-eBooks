#setwd("~/r_directory")
#library("plot3D")
#library("plot3Drgl")

#"Lunch_Ride_Lech.txt"
#"Roelli_TT_loop.gpx"
#"Schauinsland_mtb.gpx"
#"Morning_Ride_Kandel.gpx"

#TO DO LIST: 
#plot vs meters OR time
#fix power    
#first check if power, hr and cadence data is available
#also plot vs distance covered
#download from either original data or gpx
#add power zones

par(mar=c(5,4,4,4))
strava_plot("Morning_Ride_Kandel.gpx")

strava_table_creator <- function(data.gpx) {



        data_table <- read.csv(data.gpx, strip.white = TRUE, skip=1,col.names = c("column1"))
        data_character <- as.character(data_table[,1])
        
        find_time <- grep("<time>", data_character, value=TRUE)
        activity_date <- strptime(find_time, "<time>%Y-%m-%dT%H:%M:%SZ</time>")
        hours <- difftime(activity_date[-1], activity_date[1])/60/60
        
        find_elevation <- grep("<ele>", data_character, value=TRUE)
        elevation <- as.numeric(gsub("</?ele>","",find_elevation))
        
        find_coordinates <- grep("<trkpt lat", data_character, value=TRUE)
        latitude <- as.numeric(substr(find_coordinates,12,21))
        longitude <- as.numeric(substr(find_coordinates,27,35))
        
        find_power <- grep("<power>", data_character, value=TRUE)
        power <- as.numeric(gsub("</?power>","",find_power))
        
        find_pulse <- grep("<gpxtpx:hr>", data_character, value=TRUE)
        pulse <- as.numeric(gsub("</?gpxtpx:hr>","",find_pulse))
        
        #cadence
        
        data.frame(hours, elevation, longitude, latitude, pulse)
}

strava_output <- function(data.gpx) {
    
        strava_data <- strava_table_creator(data.gpx)
        
        plot(strava_data$hours, strava_data$elevation, type="l", lwd=3, 
                main= "Elevation vs Time", xlab = "Time in hours", ylab="Elevation")
        
        coordinates <- data.frame(strava_data$longitude, strava_data$latitude)
        plot(coordinates, type="l", lwd=3, 
                main = "Coordinates", xlab = "Longitude", ylab = "Latitude")
        
        scatter3D(strava_data$longitude, strava_data$latitude, strava_data$elevation, 
                  theta=15, phi=0, bty="g", type="h")
        plotrgl()
    
}

strava_output("Morning_Ride_Kandel.gpx")

plot(strava_data$hours, strava_data$elevation, type="l", lwd=3, 
     main= "Elevation vs Time", xlab = "Time in hours", ylab="Elevation")

