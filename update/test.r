pacman::p_load(httr2, rvest)

request("https://en.usports.ca/sports/mbkb/2024-25c/boxscores/20250316_ta5h.xml") |>
  req_method("GET") |>
  req_headers(
    Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    Cookie = "_ga=GA1.1.1720239674.1745164384; _ga_CWRJE5NM3S=GS2.1.s1754076635$o58$g1$t1754076656$j39$l0$h0; _ga_7M0RQ44GSJ=GS2.1.s1754076635$o67$g1$t1754076655$j40$l0$h0; _gid=GA1.2.885191208.1754072369; __eoi=ID=6b0a526148a64982:T=1745271217:RT=1754076635:S=AA-AfjYr6bEHkU9UiiK5vObQXzC-; __gads=ID=6e351a18228acda5:T=1745271217:RT=1754076635:S=ALNI_MZx59oCGU_p-8MBLoAm4EDxFLeeiw; __gpi=UID=0000109ab1c48a56:T=1745271217:RT=1754076635:S=ALNI_MaRImOz9PFByV0iPfOpsuAfO1y3WA; _gat_gtag_UA_1939879_1=1; __qca=P1-5925ff08-7feb-48a4-8b8f-bc78808b5682",
    `Accept-Encoding` = "gzip, deflate, br",
    Host = "en.usports.ca",
    `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.1 Safari/605.1.15",
    `Accept-Language` = "en-CA,en-US;q=0.9,en;q=0.8",
  ) |>
  req_perform() |> resp_body_html() |> rvest::html_table()
