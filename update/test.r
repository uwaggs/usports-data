library(rvest)

read_html("https://en.usports.ca/sports/mbkb/2024-25c/boxscores/20250316_ta5h.xml") |>
  html_table()
