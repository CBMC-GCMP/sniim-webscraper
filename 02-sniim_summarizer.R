library(tidyverse)

hist <- data.table::fread("data/fish_price_historical_webscraped_data.csv")


hist %>%
          filter(!is.na(price_mxn)) %>%
          mutate(type = factor(type, labels = c("Destiny market", "Origin market")),
                 category = factor(category, labels = c("Crustaceans", "Fish Fillets", "Mollusks", "Fin Fish"))) %>% 
          group_by(date, category, product_short_name) %>%
          filter(category != "Fish Fillets") %>% 
          summarise(price_mxn = mean(price_mxn, na.rm = T)) %>% 
          data.table::fwrite(., file = "data/ts_summary.csv")





