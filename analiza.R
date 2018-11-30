# 24 lipca - pożary w Grecji, region = Attyka

rm(list = ls())

library(tidyverse)
library(glue)
library(stringr)
library(lubridate)

wycieczki_db <- readRDS("wycieczki_all.RDS")

# liczba wycieczek wg daty zebrania danych
wycieczki_db %>%
   count(data_aktualizacji) %>%
   arrange(data_aktualizacji) %>%
   collect() %>%
   ggplot(aes(data_aktualizacji, n)) +
   geom_col()

wycieczki_db %>%
   filter(data_aktualizacji == max(data_aktualizacji, na.rm = TRUE)) %>%
   count(data_aktualizacji) %>%
   collect()



srednie <- wycieczki_db %>%
   # najwiecej ofert z tych miejsc
   filter(region %in% c("Kreta", "Rodos"),
          miejscowosc %in% c("Hersonissos", "Georgioupolis", "Kolymbia", "Faliraki", "Kiotari")) %>%
   # liczymy średnie
   group_by(data_aktualizacji, organizator, region, miejscowosc) %>%
   summarise(sr_ocena = mean(ocena_wycieczki, na.rm = TRUE),
             sr_cena = mean(cena_za_osobe, na.rm = TRUE),
             sr_rezerwacje = mean(liczba_rezerwacji, na.rm = TRUE)) %>%
   ungroup() %>%
   collect()

# z kim nalepiej w dane miejsce?
srednie %>%
   ggplot() +
   geom_point(aes(organizator, sr_ocena)) +
   coord_flip() +
   facet_wrap(region~miejscowosc, scales = "free_y")

# z kim najtaniej w dane miejsce?
srednie %>%
   ggplot() +
   geom_point(aes(organizator, sr_cena)) +
   coord_flip() +
   facet_wrap(region~miejscowosc, scales = "free_y")

# z kim najlepsza cena/jakosc w dane miejsce?
srednie %>%
   ggplot() +
   geom_point(aes(sr_cena, sr_ocena, color = organizator)) +
   coord_flip() +
   facet_wrap(region~miejscowosc)


# z kim najwiecej w dane miejsce?
srednie %>%
   ggplot() +
   geom_point(aes(organizator, sr_rezerwacje)) +
   coord_flip() +
   facet_wrap(region~miejscowosc, scales = "free_y")



wycieczki_db %>%
   filter(hotel_nazwa == "Princess Andriana") %>%
   collect() %>%
   ggplot() +
   geom_point(aes(data_aktualizacji, cena_za_osobe)) +
   facet_grid(organizator~termin_wycieczki) +
   labs(title = "Cena za osobę dla wycieczki\ndo hotelu \"Princess Andriana\"\nw Kiotari na Rodos")




# która miejscowość najbardziej atrakcyjna
wycieczki_db %>%
   group_by(miejscowosc) %>%
   summarise(sr_ocena = mean(ocena_wycieczki, na.rm = TRUE),
             sr_cena = mean(cena_za_osobe, na.rm = TRUE)) %>%
   ungroup() %>%
   collect() %>%
   ggplot() +
   geom_point(aes(sr_ocena, sr_cena, color = miejscowosc), show.legend = FALSE) +
   geom_text(aes(sr_ocena, sr_cena,
                  label = if_else(miejscowosc %in% c("Hersonissos", "Chania", "Faliraki", "Ixia", "Kolymbia", "Kiotari", "Georgioupolis"),
                                  miejscowosc, "")))


# jakość do ceny per miejscowość
wycieczki_db %>%
   group_by(miejscowosc) %>%
   summarise(sr_ocena = mean(ocena_wycieczki, na.rm = TRUE),
             sr_cena = mean(cena_za_osobe, na.rm = TRUE)) %>%
   ungroup() %>%
   mutate(ocena = sr_ocena/sr_cena) %>%
   arrange(desc(ocena))


wycieczki_db %>%
   select(miejscowosc, ocena_wycieczki, cena_za_osobe) %>%
   filter(miejscowosc %in% c("Hersonissos", "Chania", "Faliraki", "Ixia", "Kolymbia", "Kiotari", "Georgioupolis")) %>%
   ggplot() +
   geom_point(aes(ocena_wycieczki, cena_za_osobe, color = miejscowosc)) +
   geom_smooth(aes(ocena_wycieczki, cena_za_osobe))



# zmina cen
wycieczki_db %>%
   group_by(data_aktualizacji, region) %>%
   summarize(sr_cena = mean(cena_za_osobe, na.rm = TRUE)) %>%
   ungroup() %>%
   collect() %>%
   ggplot(aes(data_aktualizacji, sr_cena)) +
   geom_point() +
   geom_smooth() +
   facet_wrap(~region, scales = "free") +
   labs(title = "Srednia cena wycieczki do Grecji za osobę wg regionów",
        subtitle = "Parametry wycieczki: wyjazd 1-30.09, samolotem z Warszawy, 2 dorosłych, 2 dzieci (4 i 7 lat)\nAll inclusive",
        x = "Data oferty", y = "Średnia cena/osobę")


wycieczki_db %>%
   filter(hotel_nazwa == "Princess Andriana") %>%
   group_by(data_aktualizacji) %>%
   summarize(sr_cena = mean(cena_za_osobe, na.rm = TRUE)) %>%
   ungroup() %>%
   collect() %>%
   ggplot(aes(data_aktualizacji, sr_cena)) +
   geom_point() +
   geom_smooth()


# czy im bliżej wycieczki tym niższa cena?
wycieczki_db %>%
   mutate(dni_przed_wycieczka = termin_wycieczki - data_aktualizacji) %>%
   group_by(dni_przed_wycieczka) %>%
   summarise(cena = mean(cena_za_osobe)) %>%
   ungroup() %>%
   ggplot() +
   geom_smooth(aes(dni_przed_wycieczka, cena)) +
   scale_x_reverse()



# czy im bliżej wycieczki tym niższa cena?
wycieczki_db %>%
   mutate(dni_przed_wycieczka = termin_wycieczki - data_aktualizacji) %>%
   group_by(dni_przed_wycieczka, region, organizator) %>%
   summarise(cena = mean(cena_za_osobe)) %>%
   ungroup() %>%
   ggplot() +
   geom_smooth(aes(dni_przed_wycieczka, cena, color = organizator)) +
   facet_wrap(~region, scales = "free_y")


# czy im bliżej wycieczki tym więcej jest zarezerwowanych?
wycieczki_db %>%
   mutate(dni_przed_wycieczka = termin_wycieczki - data_aktualizacji) %>%
   group_by(dni_przed_wycieczka) %>%
   summarise(rez = sum(liczba_rezerwacji, na.rm = TRUE )) %>%
   ungroup() %>%
   arrange(-dni_przed_wycieczka) %>%
   mutate(rez = cumsum(rez)) %>%
   ggplot() +
   geom_smooth(aes(dni_przed_wycieczka, rez)) +
   scale_y_log10() +
   scale_x_reverse()


# kto ma najlepsze wycieczki? prosty model oceny
normalize <- function(x) { return ((x - min(x))/(max(x)-min(x))) }

df <- wycieczki_db %>%
   group_by(id_wycieczki) %>%
   filter(data_aktualizacji == max(data_aktualizacji)) %>%
   ungroup() %>%
   group_by(organizator, region, miejscowosc) %>%
   summarise(sr_gwiazdki = mean(hotel_gwiazdki, na.rm = TRUE),
             sr_ocena_wycieczki = mean(ocena_wycieczki, na.rm = TRUE),
             n_wycieczek = n(),
             sr_cena_za_osobe = mean(cena_za_osobe, na.rm = TRUE)) %>%
   ungroup() %>%
   na.omit() %>%
   mutate(sr_gwiazdki_scale = normalize(sr_gwiazdki),
          sr_ocena_wycieczki_scale = normalize(sr_ocena_wycieczki),
          sr_cena_za_osobe = 1 - normalize(sr_cena_za_osobe)) %>%
   mutate(ocena = normalize(3*sr_gwiazdki * 6*sr_ocena_wycieczki * 4*sr_cena_za_osobe * log10(n_wycieczek+1))) %>%
   mutate(ocena_przedzial = cut(ocena, breaks = seq(-0.1, 1.1, 0.1)))


ggplot(df) +
   geom_point(aes(sr_ocena_wycieczki, sr_gwiazdki,
                  color = ocena, size = ocena),
              alpha = 0.8) +
   coord_equal() +
   scale_size(range = c(0.5, 2.5)) +
   scale_color_gradient(low="#fde0dd", high="#de2d26")


# który organizator najlepszy?
df %>%
   group_by(organizator, ocena_przedzial) %>%
   summarise(n = n()) %>%
   ungroup() %>%
   group_by(organizator) %>%
   mutate(p = n/sum(n)) %>%
   ungroup() %>% filter(p != 0) %>%
   ggplot() +
   geom_tile(aes(ocena_przedzial, organizator, fill = p), color = "gray80") +
   geom_text(aes(ocena_przedzial, organizator, label = sprintf("%.0f%%", 100*p))) +
   scale_fill_distiller(palette = "GnBu")


# który region najlepszy?
df %>%
   group_by(region, ocena_przedzial) %>%
   summarise(n = n()) %>%
   ungroup() %>%
   group_by(region) %>%
   mutate(p = n/sum(n)) %>%
   ungroup() %>% filter(p != 0) %>%
   ggplot() +
   geom_tile(aes(ocena_przedzial, region, fill = p), color = "gray80") +
   geom_text(aes(ocena_przedzial, region, label = sprintf("%.0f%%", 100*p))) +
   scale_fill_distiller(palette = "GnBu")


df %>%
   top_n(10, ocena) %>%
   arrange(desc(ocena))


# z wybranych regionów, miast i organizatorów wybieramy najlepszą ofertę
wycieczki_db %>%
   filter(organizator %in% c("Wezyr", "TUI", "Rainbow", "Grecos"),
          region %in% c("Kreta", "Rodos", "Kos"),
          miejscowosc %in% c("Hersonissos", "Chania", "Faliraki", "Ixia", "Kolymbia", "Kiotari", "Georgioupolis")) %>%
   group_by(id_wycieczki) %>%
   filter(data_aktualizacji == max(data_aktualizacji),
          !is.na(hotel_gwiazdki), !is.na(ocena_wycieczki), !is.na(cena_za_osobe)) %>%
   ungroup() %>%
   mutate(gwiazdki_scale = normalize(hotel_gwiazdki),
          ocena_wycieczki_scale = normalize(ocena_wycieczki),
          cena_za_osobe_scale = 1- normalize(cena_za_osobe)) %>%
   mutate(ocena = normalize(3*gwiazdki_scale * 6*ocena_wycieczki_scale * 4*cena_za_osobe_scale)) %>%
   select(organizator, region, miejscowosc, hotel_nazwa, hotel_gwiazdki, ocena_wycieczki, ocena, cena_za_osobe) %>%
   top_n(10, ocena) %>%
   arrange(desc(ocena))


# wygrywa ta https://www.grecos.pl/wakacje/grecja/hotel-princess-andriana oferta
# na wakacje.pl było to https://www.wakacje.pl/oferty/grecja/rodos/kiotari/princess-andriana-472864.html
# https://towardsdatascience.com/exploring-machine-learning-for-airbnb-listings-in-toronto-efdbdeba2644
