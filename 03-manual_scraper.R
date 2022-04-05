
# Loading libraries -------------------------------------------------------

library(rvest)
library(janitor)
library(stringr)
library(tidyverse)
library(googlesheets4)

source("R/nytnyt.R")

# Input days --------------------------------------------------------------

day1 <- 01
day2 <- 04
month <- 04
year <- 2022




# Origin prices -----------------------------------------------------------

pescado_origin <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_origen-res1.asp?T=m&T1=mr&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year,"&RegPag=100000&x=32&y=14")
crustaceos_origin <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_origen-res1.asp?T=C&T1=C&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year,"&RegPag=100000&x=37&y=17")
moluscos_origin <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_origen-res1.asp?T=o&T1=of&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year,"&RegPag=100000&x=42&y=15")
filete_otros_origin <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_origen-res1.asp?T=f&T1=pf&T2=of&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year,"&RegPag=100000&x=41&y=20")

#create list of websites
weblist <- list(pescado_origin, crustaceos_origin, moluscos_origin, filete_otros_origin)
names(weblist) <- c("pescado", "crustaceos", "moluscos", "filetes")


# Scraping the website as a single query category

scrap <- list()

for (i in 1:length(weblist)) {
          
          scrap[[i]] <-  read_html(
                    curl::curl(weblist[[i]], 
                               handle = curl::new_handle("useragent" = "Mozilla/5.0"))) %>% 
                    html_nodes(xpath = '/html/body/div[1]/center/table[2]') %>%
                    html_table() %>% 
                    .[[1]] %>% 
                    as.data.frame() %>% 
                    mutate(category = c("category", rep(names(weblist)[[i]], length(.$X1)-1)))
          nytnyt()

}

results_origin <- do.call(rbind, scrap)

results_origin <- clean_names(row_to_names(results_origin, row_number = 1))
results_origin <- results_origin %>% filter(
          !str_detect(.$fecha, c("Fuente")), 
          !str_detect(.$fecha, "Fecha")) %>% 
          mutate_at(c("pmin", "pmax", "pfrec"), as.numeric) %>% 
          mutate(scrp_id = 1:length(.$fecha), .before = fecha) %>% 
          mutate(fecha = as.Date(fecha, "%d/%m/%Y")) %>% 
          mutate(type = "origin", .before = fecha) %>% 
          mutate(fuente = origen)

# Destiny prices ----------------------------------------------------------


pescado_destiny <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_destino-res1.asp?T=m&T1=mr&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year,"&RegPag=10000&x=36&y=24")
crustaceos_destiny <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_destino-res1.asp?T=C&T1=C&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year,"&RegPag=10000&x=25&y=17")
moluscos_destiny <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_destino-res1.asp?T=o&T1=of&T2=&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year,"&RegPag=10000&x=50&y=9")
filete_otros_destiny <- paste0("http://www.economia-sniim.gob.mx/SNIIM-PESCA/e_destino-res1.asp?T=f&T1=pf&T2=of&fuente=0&prod=0&dia1=",day1,"&dia2=",day2,"&mes=",month,"&anio=",year,"&RegPag=10000&x=28&y=17")


weblist <- list(pescado_destiny, crustaceos_destiny, moluscos_destiny, filete_otros_destiny)
names(weblist) <- c("pescado", "crustaceos", "moluscos", "filetes")

# Scraping the website as a single query category -------------------------------------------------------

scrap <- list()

for (i in 1:length(weblist)) {
          
          scrap[[i]] <-  read_html(curl::curl(weblist[[i]], handle = curl::new_handle("useragent" = "Mozilla/5.0"))) %>%
                    html_nodes(xpath = '/html/body/div[1]/center/table[2]') %>%
                    html_table() %>% 
                    .[[1]] %>% 
                    as.data.frame() %>% 
                    mutate(category = c("category", rep(names(weblist)[[i]], length(.$X1)-1)))
          nytnyt()
          
}

results_destiny <- do.call(rbind, scrap)

results_destiny <- clean_names(row_to_names(results_destiny, row_number = 1))

results_destiny <- results_destiny %>%
          mutate(grp = str_detect(string = fecha, pattern = ":"),
                 fuente = ifelse(grp, sub('.*:', '', fecha), NA_real_)) %>%
          fill(fuente) %>%
          filter(!grp) %>%
          select(-grp) %>% 
          mutate_at(c("pmin", "pmax", "pfrec"), as.numeric) %>% 
          mutate(scrp_id = 1:length(.$fecha), .before = fecha) %>% 
          mutate(fecha = as.Date(fecha, "%d/%m/%Y")) %>% 
          mutate(type = "destiny", .before = fecha) 



results <- rbind(results_origin, results_destiny) %>% 
          select(scrp_id, date = fecha, type, category, 
                 product = producto, 
                 origin_name = origen, market = fuente, 
                 pmin, pmax, price_mxn = pfrec) %>% 
          left_join(., res, by = "product") %>% 
          rename(product_short_name = short_names, IDproduct = ID) %>% 
          relocate(product_short_name, .after = product) %>% 
          relocate(IDproduct, .before = product)



data.table::fwrite(results, "data/fish_price_historical_webscraped_data.csv", append = T)

cat("You succesfully webscraped SNIIM! \n Dates scraped: ", paste0(day1, "-", month,"-", year), "to", paste0(day2, "-", month, "-", year), "\n")
