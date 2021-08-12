setwd("/Users/sebastianpahl/Git/Strava-project/")
library("tidyverse") #for str
library("zoo") #for na.locf
library("geosphere") #need for distCosine

zone_names <- c("Zone 1", "Zone 2", "Zone 3", "Zone 4", "Zone 5")
power_zones <- c(0, 150, 200, 300, 400)
hr_zones <- c(0, 110, 135, 150, 165)
zones <- data_frame(zone_names, power_zones, hr_zones)
power_zone_checker <- function(x, z = zones) {
        
        if(x < z$power_zones[2]) {
                power = z$zone_names[1]
        }
        else if(x < z$power_zones[3]) {
                power = z$zone_names[2]
        }
        else if(x < z$power_zones[4]) {
                power = z$zone_names[3]
        }
        else if(x < z$power_zones[5]) {
                power = z$zone_names[4]
        }
        else (power = z$zone_names[5])
}
hr_zone_checker <- function(x, z = zones) {
        
        if(x < z$hr_zones[2]) {
                hr = z$zone_names[1]
        }
        else if(x < z$hr_zones[3]) {
                hr = z$zone_names[2]
        }
        else if(x < z$hr_zones[4]) {
                hr = z$zone_names[3]
        }
        else if(x < z$hr_zones[5]) {
                hr = z$zone_names[4]
        }
        else (hr = z$zone_names[5])
}

strava_table_creator <- function(data.gpx) {
        
        data_table <- read.csv(data.gpx, strip.white = TRUE, skip=1,col.names = c("column1"))
        data_character <- as.character(data_table[,1])
        
        find_time <- grep("<time>", data_character, value=TRUE)
        activity_date <- strptime(find_time, "<time>%Y-%m-%dT%H:%M:%SZ</time>")
        time_passed <- difftime(activity_date[-1], activity_date[1])
        hours <- as.numeric(time_passed)/60/60
        
        find_elevation <- grep("<ele>", data_character, value=TRUE)
        elevation <- as.numeric(gsub("</?ele>","",find_elevation))
        
        find_coordinates <- grep("<trkpt lat", data_character, value=TRUE)
        latitude <- as.numeric(substr(find_coordinates,12,21))
        longitude <- as.numeric(substr(find_coordinates,27,35))
        
        #extensions
        data_string <- toString(data_table)
        extensions <- c(str_split(data_string, "/extensions", simplify=TRUE))
        
        power_function <- function(x) {
                extract_power <- str_extract(x, "<power>.*</power>")
                power <- str_extract(extract_power, "\\d+")
                power
        }
        power <- head(na.locf(as.numeric(lapply(extensions, power_function))),-1)
        
        hr_function <- function(x) {
                extract_hr <- str_extract(x, "<gpxtpx:hr>.*</gpxtpx:hr>")
                hr <- str_extract(extract_hr, "\\d+")
                hr
        }
        hr <- head(na.locf(as.numeric(lapply(extensions, hr_function))),-1)
        
        cadence_function <- function(x) {
                extract_cadence <- str_extract(x, "<gpxtpx:cad>.*</gpxtpx:cad>")
                cadence <- str_extract(extract_cadence, "\\d+")
                cadence
        }
        cadence <- head(na.locf(as.numeric(lapply(extensions, cadence_function))),-1)
        
        temperature_function <- function(x) {
                extract_temperature <- str_extract(x, "<gpxtpx:atemp>.*</gpxtpx:atemp>")
                temperature <- str_extract(extract_temperature, "\\d+")
                temperature
        }
        temperature <- head(na.locf(as.numeric(lapply(extensions, temperature_function))),-1)
        
        power_zone <- sapply(power, power_zone_checker)
        hr_zone <- sapply(hr, hr_zone_checker)
        
        data.frame(hours, elevation, longitude, latitude, power, hr, cadence, 
                   temperature, power_zone, hr_zone)
}
#create data frame from gpx file (load your own gpx file here)
strava_data <- strava_table_creator("Morning_Ride_Kandel.gpx")

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
write_csv(strava_data, "strava_data.csv")

avg_finder <- function(x) {
        vector <- as.numeric()
        for (i in x:length(strava_data$power)) {
                vector[i] = mean(strava_data$power[(i-x+1):i])
        }
        vector
}
max_finder <- function(y) {
        vector <- as.numeric()
        for (i in 1:y) {
                vector[i] = max(avg_finder(i), na.rm=TRUE)
        }
        vector
}
power <- max_finder(5*60)
seconds <- c(1:(5*60))
power_curve <- data_frame(seconds, power)
write_csv(power_curve, "power_curve.csv")

