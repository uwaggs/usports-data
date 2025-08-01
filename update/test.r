pacman::p_load(chromote, selenider, rvest)

options(chromote.headless = "new")

session <- selenider::selenider_session(
  "chromote",
  timeout = 30
)

open_url("https://en.usports.ca/sports/mbkb/2024-25c/boxscores/20250316_ta5h.xml")

Sys.sleep(10)

get_page_source() |>
  rvest::html_table()
