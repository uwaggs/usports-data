pacman::p_load(rvest, httr)

url <- "https://en.usports.ca/sports/mbkb/2024-25c/boxscores/20250316_ta5h.xml"

res <- GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0.0.0 Safari/537.36"))

if (status_code(res) == 200) {
  page <- read_html(res)
  tables <- html_table(page)
  print(tables)
} else {
  stop("Request failed with status: ", status_code(res))
}
