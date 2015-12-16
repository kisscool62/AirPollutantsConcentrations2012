library(dplyr)

raw_csv <- list.files('../data/raw_csv/', pattern = ".*_station_statistics-Tableau.*.csv", full.names = T)

read_raw_csv <- function(path){
    var_name <- gsub(x=path, pattern="\\.\\./data/raw_csv//(.*)_station_statistics-Tableau.*", replacement = "\\1")
    temp <- read.csv2(path, skip=10, quote="")
    temp <- temp[temp$statistics_year == 2012, ]
#     group_by(temp, city_name)

    assign(var_name, temp, envir = .GlobalEnv)

    c(length(unique(get(var_name)$city_name)), length(get(var_name)$city_name))
}

lapply(raw_csv, FUN=read_raw_csv)

bap_eur <- unique(as.character(BaP$station_european_code))
no2_eur <- unique(as.character(NO2$station_european_code))
o3_eur <- unique(as.character(O3$station_european_code))
pm10_eur <- unique(as.character(PM10$station_european_code))
pm2.5_eur <- unique(as.character(PM2.5$station_european_code))

PM10_d <- subset(PM10, select=c(country_iso_code, country_name, city_name, UA_city_pop, percentage_traffic_population, percentage_background_population, station_european_code, type_of_station, station_type_of_area, count_type_by_city, assigned_population, statistic_value, statistics_percentage_valid, above_DLV. ))

PM2.5_d <- subset(PM2.5, select=c(station_european_code, statistic_value, statistics_percentage_valid, above_TV. ))

O3_d <- subset(O3, select=c(station_european_code, statistic_value, statistics_percentage_valid, above_TV. ))

NO2_d <- subset(NO2, select=c(station_european_code, statistic_value, statistics_percentage_valid, above_LV. ))

BaP_d <- subset(BaP, select=c(station_european_code, statistic_value, statistics_percentage_valid, above_TV. ))

pollutants <- merge(
    x = PM10_d,
    y = NO2_d,
    by.x = "station_european_code",
    by.y = "station_european_code")

pollutants <- mutate(
    pollutants,
    PM10=statistic_value.x,
    NO2=statistic_value.y,
    PM10above_DLV. = above_DLV.,
    NO2above_LV. = above_LV.,
    PM10statistics_percentage_valid = statistics_percentage_valid.x,
    NO2statistics_percentage_valid = statistics_percentage_valid.y)

pollutants <- subset(
    pollutants,
    select=-c(
        statistic_value.x, statistic_value.y,
        above_DLV., above_LV.,
        statistics_percentage_valid.x, statistics_percentage_valid.y))

pollutants <- merge(
    x = pollutants,
    y = PM2.5_d,
    by.x = "station_european_code",
    by.y = "station_european_code")

pollutants <- mutate(
    pollutants,
    PM2.5=statistic_value,
    PM2.5above_TV. = above_TV.,
    PM2.5statistics_percentage_valid = statistics_percentage_valid)

pollutants <- subset(
    pollutants,
    select=-c(
        statistic_value,
        above_TV.,
        statistics_percentage_valid))


pollutants <- merge(
    x = pollutants,
    y = O3_d,
    by.x = "station_european_code",
    by.y = "station_european_code")

pollutants <- mutate(
    pollutants,
    O3=statistic_value,
    O3above_TV. = above_TV.,
    O3statistics_percentage_valid = statistics_percentage_valid)

pollutants <- subset(
    pollutants,
    select=-c(
        statistic_value,
        above_TV.,
        statistics_percentage_valid))

pollutants <- merge(
    x = pollutants,
    y = BaP_d,
    by.x = "station_european_code",
    by.y = "station_european_code")

pollutants <- mutate(
    pollutants,
    BaP=statistic_value,
    BaPabove_TV. = above_TV.,
    BaPstatistics_percentage_valid = statistics_percentage_valid)


pollutants <- subset(
    pollutants,
    select=-c(
        statistic_value,
        above_TV.,
        statistics_percentage_valid))


is_above <- function(x){
    x == 0
}

pollutants_concentrations <- mutate(
    pollutants,
    BaPabove_TV. = is_above(BaPabove_TV.),
    O3above_TV. = is_above(O3above_TV.),
    PM10above_DLV. = is_above(PM10above_DLV.),
    PM2.5above_TV. = is_above(PM2.5above_TV.),
    NO2above_LV. = is_above(NO2above_LV.))




