source("R/utils.R")
library(usportsscaper)

for(link in links) {
  html <- tryCatch({
    rvest::read_html(link)
  }, error = function(e) {
    return(NULL)
  })
  start_string <- "boxscores/"
  end_string <- ".xml"

  pattern <- paste0("(?<=", strat_string, ").*?(?=", end_string, ")")
  game_id <- stringr::str_extract(link, pattern)

  season = stringr::str_extract(link, "\\d{4}-\\d{2}")
}



# NOTHING - the only Rugby function is schedule, because all stats are on paper
