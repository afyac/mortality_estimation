library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)
library(tidyr)

somalia <- st_read("01_extract_smart_surveys/data/som_admin2.shp")
somalia <- st_transform(somalia, 4326)

somalia <- somalia |> dplyr::mutate(adm2_name = tolower(adm2_name))

adm1 <- somalia |>
  group_by(adm1_name) |>
  summarise(geometry = st_union(geometry), .groups = "drop")

surveys <- rio::import('01_extract_smart_surveys/output/metadata_updated.csv') |>
  dplyr::mutate(District = ifelse(District == 'galkacyo', 'gaalkacyo',District),
                District = ifelse(District == 'beletweyne', 'belet weyne',District),
                District = ifelse(District == 'kismayo', 'kismaayo',District),
                District = ifelse(District == 'banadir', 'wadajir (medina)',District),
                District = ifelse(District == 'ceelbarde', 'ceel barde',District),
                District = ifelse(District == 'beledweyne', 'belet weyne',District),
                District = ifelse(District == 'wanlaweyn', 'wanla weyn',District),
                District = ifelse(District == 'laascaanood', 'laas caanood',District),
                District = ifelse(District == 'dhusamareb', 'dhuusamarreeb',District),
                District = ifelse(District == 'xuddur', 'xudur',District),
                District = ifelse(District == 'wajid', 'waajid',District),
                District = ifelse(District == 'odweyne', 'owdweyne',District),
                District = ifelse(District == 'beletxaawo', 'belet xaawo',District),
                District = ifelse(District == 'calula', 'caluula',District),
                District = ifelse(District == 'dinsor', 'diinsoor',District),
                District = ifelse(District == 'bari', 'bandarbeyla',District)
                ) 

full_data <- rio::import('01_extract_smart_surveys/output/som_smart_surveys_data.rds') |>
  dplyr::group_by(surveyId, district) |> 
  dplyr::summarise(n = sum(n), .groups='drop')|>
  dplyr::mutate(district = tolower(district), 
                district = ifelse(district == 'galkacyo', 'gaalkacyo',district),
                district = ifelse(district == 'beletweyne', 'belet weyne',district),
                district = ifelse(district == 'kismayo', 'kismaayo',district),
                district = ifelse(district == 'banadir', 'wadajir (medina)',district),
                district = ifelse(district == 'ceelbarde', 'ceel barde',district),
                district = ifelse(district == 'beledweyne', 'belet weyne',district),
                district = ifelse(district == 'wanlaweyn', 'wanla weyn',district),
                district = ifelse(district == 'laascaanood', 'laas caanood',district),
                district = ifelse(district == 'dhusamareb', 'dhuusamarreeb',district),
                district = ifelse(district == 'xuddur', 'xudur',district),
                district = ifelse(district == 'wajid', 'waajid',district),
                district = ifelse(district == 'odweyne', 'owdweyne',district),
                district = ifelse(district == 'beletxaawo', 'belet xaawo',district),
                district = ifelse(district == 'calula', 'caluula',district),
                district = ifelse(district == 'dinsor', 'diinsoor',district),
                district = ifelse(district == 'bari', 'bandarbeyla',district)
  ) |>
  dplyr::rename(adm2_name = district)

surveys <- surveys |> dplyr::left_join(full_data, by=c('SurveyID' = 'surveyId'))

surveys$n[is.na(surveys$n)] <- 100

survey_map <- somalia |>
  left_join(surveys, by = c("adm2_name"))

survey_counts <- survey_map |>
  group_by(adm2_name) |>
  summarise(n_surveys = sum(n), .groups = "drop")



district_centroids <- st_centroid(survey_counts)


surveys_sf <- st_as_sf(
  survey_map,
  coords = c("geometry"),
  crs = 4326
)

map_plot <- ggplot() +
  
  geom_sf(data = somalia,
          fill = "grey95",
          color = "grey70",
          size = 0.2) +
  geom_sf(data = adm1,
          fill = NA,
          color = "black",
          size = 0.6) +
  geom_sf(
    data = district_centroids |> filter(!is.na(n_surveys)),
    aes(size = n_surveys),
    color = "#e75480",
    alpha = 0.8
  )+
  
  scale_size_continuous(
    name = "Number of surveys",
    range = c(1,10)
  ) +
  
  theme_bw() +
  theme(legend.position = "none")

ggsave('01_extract_smart_surveys/vizualisation/output/som_plot_map.png',
       dpi = "print", units = "cm", width = 30, height = 30)
