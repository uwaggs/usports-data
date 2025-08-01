pacman::p_load(httr2, rvest, stringi)

user_agents <- c(
  # Chrome, Firefox, Safari, Edge examples
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.1 Safari/605.1.15",
  "Mozilla/5.0 (Windows NT 10.0; rv:115.0) Gecko/20100101 Firefox/115.0",
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36",
  "Mozilla/5.0 (iPhone; CPU iPhone OS 17_1 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Mobile/15E148 Safari/604.1"
)

accept_languages <- c(
  "en-US,en;q=0.9",
  "en-CA,en;q=0.8",
  "fr-CA,fr;q=0.7,en-US;q=0.6",
  "de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7",
  "es-ES,es;q=0.9,en-US;q=0.8,en;q=0.7"
)

referers <- c(
  "https://en.usports.ca/",
  "https://www.google.com/",
  "https://www.bing.com/",
  "https://www.facebook.com/",
  "https://www.yahoo.com/"
)

accept_encodings <- c(
  "gzip, deflate, br",
  "gzip, deflate",
  "identity"
)

hosts <- c(
  "en.usports.ca"
)

# Optional: Randomize cookie string
rand_cookie <- function() {
  paste0("_ga=GA1.1.", sample(1000000000:9999999999, 1), ".", sample(1000000000:9999999999, 1), "; ",
         "_gid=GA1.2.", sample(1000000000:9999999999, 1), ".", sample(1000000000:9999999999, 1), "; ",
         "__qca=P1-", stri_rand_strings(1, 6, '[a-z0-9]'), "-7feb-48a4-8b8f-bc78808b5682")
}


make_headers <- function() {
  # Shuffle order for more randomness
  headers <- list(
    Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    Cookie = rand_cookie(),
    `Accept-Encoding` = sample(accept_encodings, 1),
    Host = sample(hosts, 1),
    `User-Agent` = sample(user_agents, 1),
    `Accept-Language` = sample(accept_languages, 1),
    Referer = sample(referers, 1)
  )
  headers[sample(names(headers))] # randomize order
}


random_delay <- function(min = 2, max = 10) {
  delay <- runif(1, min, max)
  cat(sprintf("Sleeping for %.2f seconds...\n", delay))
  Sys.sleep(delay)
}


scrape_page <- function(url) {
  random_delay() # wait randomly before each request
  hdrs <- make_headers()
  cat("Using headers:\n")
  print(hdrs)

  req <- request(url) |>
    req_method("GET") |>
    req_headers(!!!hdrs) # !!! unpacks the list into arguments

  # Optionally, add random query params to reduce caching detection
  if (runif(1) > 0.7) {
    req <- req_url_query(req, dummy = stri_rand_strings(1, 8, '[a-zA-Z0-9]'))
  }

  # Perform request and handle errors
  tryCatch({
    resp <- req_perform(req)
    html <- resp_body_html(resp)
    tables <- rvest::html_table(html)
    cat("Scrape successful!\n")
    return(tables)
  }, error = function(e) {
    cat("Scrape failed: ", conditionMessage(e), "\n")
    return(NULL)
  })
}

url <- "https://en.usports.ca/sports/mbkb/2024-25c/boxscores/20250316_ta5h.xml"
result <- scrape_page(url)
