library("cronR")
cron_ls()

path = paste0(getwd(), "/01-sniim_scraper.R")

cmd <- cron_rscript(path)

cron_add(command = cmd, frequency = "daily", at="21:00", days_of_week = c(1:7), 
         tags = "webscraping-sniim",
         id = "webscraping_prices_sniim", 
         description = "webscrape prices from sniim webpage")




path = paste0(getwd(), "/02-sniim_summarizer.R")

cmd <- cron_rscript(path)

cron_add(command = cmd, frequency = "daily", at="23:00", days_of_week = c(1:7), 
         tags = "summarising-webscraping-sniim",
         id = "summarising-webscraping_prices_sniim", 
         description = "summarising prices from sniim webpage")

# To remove: 
# cron_rm(id = "webscraping_prices_sniim")


