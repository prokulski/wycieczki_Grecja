setwd("~/RProjects/wycieczki_grecja")
library(tidyverse)
library(ggmap)

# library(multidplyr)

wycieczki_db <- read_rds("wycieczki_all.RDS")



miejsca <- wycieczki_db %>% distinct(region, miejscowosc)

miejsca

miejsca <- miejsca %>%
  mutate(location = paste0(.$miejscowosc, ", ", .$region, ", Greece")) %>%
  mutate_geocode(location, source = "dsk", messaging = FALSE) %>%
  filter(lon >= 15, lon <= 35, lat >= 20)

miejsca


greece_map <- map_data('world') %>% filter(region == "Greece")

ggplot() +
  geom_polygon(data = greece_map, aes(long, lat, group=group),
               fill = "white", color = "black") +
  geom_point(data = miejsca, aes(lon, lat, color = region)) +
  coord_quickmap()



oceny <- left_join(miejsca,
                   wycieczki_db %>% select(region, miejscowosc, ocena_wycieczki),
                   by = c("region"="region", "miejscowosc"="miejscowosc")) %>%
  group_by(lon, lat) %>%
  summarise(mocena = mean(ocena_wycieczki, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(mocena)) %>%
  mutate(mocena_round = round(mocena))

ggplot() +
  geom_polygon(data = greece_map,
               aes(long, lat, group=group),
               fill = "#e5f5e0", color = "#bdbdbd") +
  geom_point(data = oceny,
             aes(lon, lat, color = mocena),
             size = 3) +
  scale_color_gradient(low="#fde0dd", high="#de2d26") +
  coord_quickmap() +
  facet_wrap(~mocena_round)



wycieczki_db %>%
  select(region, id_wycieczki, data_aktualizacji, ocena_wycieczki) %>%
  group_by(id_wycieczki) %>%
  filter(data_aktualizacji  == max(data_aktualizacji )) %>%
  filter(!is.na(ocena_wycieczki)) %>%
  ungroup %>%
  group_by(region) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  mutate(region = sprintf("%s (%d)", region, n)) %>%
  ggplot() +
  geom_density(aes(ocena_wycieczki)) +
  facet_wrap(~region, scales = "free_y")


wycieczki_db %>%
  select(region, id_wycieczki, data_aktualizacji, ocena_wycieczki) %>%
  group_by(id_wycieczki) %>%
  filter(data_aktualizacji  == max(data_aktualizacji )) %>%
  filter(!is.na(ocena_wycieczki)) %>%
  ungroup %>%
  group_by(region) %>%
  summarise(n = n(), sr_ocena = mean(ocena_wycieczki, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(sr_ocena))
