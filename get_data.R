library(tidyverse)
library(rvest)
library(glue)
library(stringr)
library(lubridate)
library(RPostgreSQL)


# dostęp do bazy danych
source("db_login.R")
# w środku:
# dbname = nazwa bazy
# dbuser = użytkownik bazy
# dbpassword = hasło do bazy
# dbhost = serwer


# ile mamy stron wyników?
first_page_url <- "https://www.wakacje.pl/wczasy/grecja/?od-2018-09-01,do-2018-09-30,7-dni,samolotem,all-inclusive,z-warszawy,2dorosle-2dzieci-4-7lat"
first_page <- read_html(first_page_url)

page_count <- first_page %>%
   html_node("div.paging") %>%
   html_node("span.m") %>%
   html_nodes("a.p") %>%
   html_text()

if(is_empty(page_count)) {
   page_count <- 1
} else {
   page_count <- page_count[length(page_count)] %>% as.numeric()
}


# tabelka na wyniki
wyniki <- tibble()

# pobieranie danyc z kolejnych stron
for(strona in 1:page_count) {
   cat(glue("\rPobieram stronę: {strona} z {page_count}\n"))

   base_url <- glue("https://www.wakacje.pl/wczasy/grecja/?str-{strona},od-2018-09-01,do-2018-09-30,7-dni,samolotem,all-inclusive,z-warszawy,2dorosle-2dzieci-4-7lat")

   # pobranie strony
   page <- read_html(base_url)

   # id offerty
   id_oferty <- page %>%
      html_node("div#filtersGridCommonCont") %>%
      html_node("div#resultsGridCont") %>%
      html_nodes("div.offer") %>%
      html_attr("data-gaid")

   # tabela ze szczegółami
   szczegoly_lista <- page %>%
      html_node("div#filtersGridCommonCont") %>%
      html_node("div#resultsGridCont") %>%
      html_nodes("div.offer") %>%
      html_node("div.leftCont") %>%
      html_node("div.desc") %>%
      html_node("table.currentSettings") %>%
      html_table()

   # miejsce - hotel
   hotel_lista <- page %>%
      html_node("div#filtersGridCommonCont") %>%
      html_node("div#resultsGridCont") %>%
      html_nodes("div.offer") %>%
      html_node("div.leftCont") %>%
      html_node("div.desc") %>%
      html_node("a") %>%
      html_node("span.name") %>%
      html_text() %>%
      str_replace_all("\u00A0", "") %>%
      trimws()

   # miejsce - miejscowość
   miejscowosc_lista <- page %>%
      html_node("div#filtersGridCommonCont") %>%
      html_node("div#resultsGridCont") %>%
      html_nodes("div.offer") %>%
      html_node("div.leftCont") %>%
      html_node("div.desc") %>%
      html_node("a") %>%
      html_node("span.country") %>%
      html_text() %>%
      str_replace_all("\u00A0", "") %>%
      str_split("/")

   # gwiazdki
   gwiazdki <- page %>%
      html_node("div#filtersGridCommonCont") %>%
      html_node("div#resultsGridCont") %>%
      html_nodes("div.offer") %>%
      html_node("div.leftCont") %>%
      html_node("div.desc") %>%
      html_node("a") %>%
      html_node("span.hotelCategory-s") %>%
      html_attr("data-stars") %>%
      as.numeric()

   # gwiazdki - oferta
   gwiazdki_oferta <- page %>%
      html_node("div#filtersGridCommonCont") %>%
      html_node("div#resultsGridCont") %>%
      html_nodes("div.offer") %>%
      html_node("div.ratingBox") %>%
      html_node("b.rating") %>%
      html_text() %>%
      str_replace_all(",", ".") %>%
      trimws() %>%
      as.numeric()

   # liczba rezerwacji
   liczba_rezerwacji <- page %>%
      html_node("div#filtersGridCommonCont") %>%
      html_node("div#resultsGridCont") %>%
      html_nodes("div.offer") %>%
      html_node("div.ratingBox") %>%
      html_node("div.btm") %>%
      html_text() %>%
      str_replace_all("\\n| |\u00A0|rezerwacji|rezerwacje|rezerwacja", "") %>%
      as.numeric()

   # cena za osobę
   cena_za_osobe <- page %>%
      html_node("div#filtersGridCommonCont") %>%
      html_node("div#resultsGridCont") %>%
      html_nodes("div.offer") %>%
      html_node("div.lastCol") %>%
      html_node("a") %>%
      html_node("b.price") %>%
      html_attr("data-price") %>%
      as.numeric()

   for(i in 1:length(gwiazdki)) {
      # przepisane pobranych danych do tabeli pomocniczej
      szczegoly_tabela_tmp <- szczegoly_lista[[i]] %>%
         filter(!grepl("Promocja", X1)) %>%
         t() %>%
         as_tibble()
      colnames(szczegoly_tabela_tmp) <- szczegoly_tabela_tmp[1, ]
      szczegoly_tabela_tmp <- szczegoly_tabela_tmp[2, ]

      if(length(miejscowosc_lista[[i]]) == 3) {
         szczegoly_tabela_tmp$kraj <- miejscowosc_lista[[i]][[1]]
         szczegoly_tabela_tmp$region <- miejscowosc_lista[[i]][[2]]
         szczegoly_tabela_tmp$miejscowosc <- miejscowosc_lista[[i]][[3]]
      } else {
         szczegoly_tabela_tmp$kraj <- miejscowosc_lista[[i]][[1]]
         szczegoly_tabela_tmp$region <- miejscowosc_lista[[i]][[2]]
         szczegoly_tabela_tmp$miejscowosc <- NA
      }

      szczegoly_tabela_tmp$ocena <- gwiazdki_oferta[[i]]
      szczegoly_tabela_tmp$hotel_nazwa <- hotel_lista[[i]]
      szczegoly_tabela_tmp$hotel_gwiazdki <- gwiazdki[[i]]

      szczegoly_tabela_tmp$id <- id_oferty[[i]]
      szczegoly_tabela_tmp$liczba_rezerwacji <- liczba_rezerwacji[[i]]
      szczegoly_tabela_tmp$cena_za_osobe <- cena_za_osobe[[i]]

      # dodanie do pełnych wyników
      wyniki <- bind_rows(wyniki, szczegoly_tabela_tmp)
   }

   # chwile czekamy przed pobraniem kolejnej strony
   Sys.sleep(sample(seq(0.5, 2, 0.5), 1))
}


# poprawka daty itp
wyniki <- wyniki %>%
   # prezycyjne wybieranie kolumn uchroni Cię przed problemami w przyszłości :D
   select(Termin, Wyżywienie, Organizator, region, miejscowosc,
          ocena, hotel_nazwa, hotel_gwiazdki,
          id, liczba_rezerwacji, cena_za_osobe) %>%
   mutate(Termin = str_sub(Termin, 1, 10) %>% dmy()) %>%
   mutate(data_aktualizacji = today()) %>%
   # select(-Wylot, -kraj) %>%
   set_names(c("termin_wycieczki", "wyzywienie", "organizator", "region", "miejscowosc", "ocena_wycieczki",
               "hotel_nazwa", "hotel_gwiazdki", "id_wycieczki", "liczba_rezerwacji", "cena_za_osobe", "data_aktualizacji")) %>%
   mutate(organizator = trimws(organizator),
          wyzywienie = trimws(wyzywienie),
          hotel_nazwa = trimws(hotel_nazwa))


# zapisanie danych do bazy
sterownik <- dbDriver("PostgreSQL")
polaczenie <- dbConnect(sterownik, dbname = dbname, user = dbuser, password = dbpassword, host = dbhost)
dbWriteTable(polaczenie, "wycieczki_grecja", wyniki, append = TRUE, row.names = FALSE)
dbDisconnect(polaczenie)
