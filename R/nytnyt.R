nytnyt <- function(periods = c(1,1.5)) {
          tictoc <- runif(1, periods[1], periods[2])
          cat(paste0(Sys.time()), "Webscraping the shit out of", names(weblist)[i], "- Sleeping for ", round(tictoc, 2), "seconds\n")
          Sys.sleep(tictoc)
}

