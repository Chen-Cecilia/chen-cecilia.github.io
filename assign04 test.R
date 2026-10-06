
library(robotstxt)
get_robotstxt("https://books.toscrape.com")
paths_allowed("https://books.toscrape.com/")

library(rvest)
url <- "https://books.toscrape.com/"
page <- read_html(url)
books <- page %>%
  html_elements("article.product_pod")
length(books)
titles <- books |>
  html_elements("h3 a") |>
  html_attr("title")

titles <- books |>
  html_elements("h3 a") |>
  xml2::xml_attr("title")
titles

prices <- books |>
  rvest::html_elements("p.price_color") |>
  rvest::html_text2()
prices
length(prices)

rating_classes <- books |>
  rvest::html_elements("p.star-rating") |>
  xml2::xml_attr("class")
rating_classes
ratings <- sub("star-rating ", "", rating_classes)
ratings

books_df <- data.frame(
  title = titles,
  price = prices,
  rating = ratings
)
books_df

urls <- paste0(
  "https://books.toscrape.com/catalogue/page-",
  1:5,
  ".html"
)
urls

scrape_page <- function(page_url) {
  
  page <- read_html(page_url)
  
  books <- page |>
    rvest::html_elements("article.product_pod")
  
  titles <- books |>
    rvest::html_elements("h3 a") |>
    xml2::xml_attr("title")
  
  prices <- books |>
    rvest::html_elements("p.price_color") |>
    rvest::html_text2()
  
  rating_classes <- books |>
    rvest::html_elements("p.star-rating") |>
    xml2::xml_attr("class")
  
  ratings <- sub("star-rating ", "", rating_classes)
  
  data.frame(
    title = titles,
    price = prices,
    rating = ratings
  )
}
pages <- lapply(urls, scrape_page)
pages
length(pages)
nrow(pages[[1]])
books_5pages <- do.call(rbind, pages)
nrow(books_5pages)
head(books_5pages)

robotstxt::paths_allowed(
  "https://en.wikipedia.org/wiki/List_of_countries_by_foreign-exchange_reserves"
)
wiki_url <- "https://en.wikipedia.org/wiki/List_of_countries_by_foreign-exchange_reserves"
wiki_page <- rvest::read_html(wiki_url)
wiki_tables <- wiki_page |>
  rvest::html_elements("table")
length(wiki_tables)

table1 <- wiki_tables[[1]] |>
  rvest::html_table(fill = TRUE)
head(table1)

table2 <- wiki_tables[[2]] |>
  rvest::html_table(fill = TRUE)
head(table2)

table3 <- wiki_tables[[3]] |>
  rvest::html_table(fill = TRUE)
head(table3)
names(table3)
dim(table3)
print(table3[1:8, ], width = Inf)

names(table3) <- c(
  "country",
  "continent",
  "reserves_incl_gold",
  "change_incl_gold",
  "reserves_excl_gold",
  "change_excl_gold",
  "date",
  "ref"
)
reserves_raw <- table3[-c(1, 2), ]
head(reserves_raw)

reserves_raw[
  grepl("Total|World", reserves_raw$country, ignore.case = TRUE),
]
tail(reserves_raw, 10)

reserves_clean <- reserves_raw |>
  dplyr::select(
    country,
    continent,
    reserves_incl_gold,
    date
  )
head(reserves_clean)
rvest::read_html(urls[1])
for (u in urls) {
  cat("Testing:", u, "\n")
  
  result <- tryCatch(
    {
      rvest::read_html(u)
      "OK"
    },
    error = function(e) {
      paste("FAILED:", e$message)
    }
  )
  
  print(result)
}

pages <- lapply(urls, scrape_page)

reserves_clean$reserves_incl_gold <-
  readr::parse_number(reserves_clean$reserves_incl_gold)
head(reserves_clean)
str(reserves_clean)

reserves_clean$date <-
  lubridate::dmy(reserves_clean$date)
head(reserves_clean)
str(reserves_clean)
sum(is.na(reserves_clean$date))

reserves_raw[
  is.na(reserves_clean$date),
  c("country", "date")
]

sum(is.na(reserves_clean$reserves_incl_gold))

reserves_raw[
  grepl("\\[", reserves_raw$country),
  "country"
]
sum(grepl("[\u2009\u202F\u00A0]", reserves_raw$reserves_incl_gold))

dim(reserves_clean)
summary(reserves_clean)
dim(reserves_clean)
summary(reserves_clean)

pop_url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"

robotstxt::paths_allowed(pop_url)

pop_page <- rvest::read_html(pop_url)
pop_tables <- pop_page |>
  rvest::html_elements("table")

length(pop_tables)

pop_table1 <- pop_tables[[1]] |>
  rvest::html_table(fill = TRUE)

head(pop_table1)

dim(pop_table1)
names(pop_table1)

tail(pop_table1, 10)

lubridate::dmy(date)
pop_clean <- pop_table1 |>
  dplyr::filter(Location != "World") |>
  dplyr::select(
    Location,
    Population,
    Date
  ) |>
  dplyr::rename(
    location = Location,
    population = Population,
    date = Date
  )
head(pop_clean)
tail(pop_clean)
dim(pop_clean)

pop_clean$population <-
  readr::parse_number(pop_clean$population)
str(pop_clean)
sum(is.na(pop_clean$population))

date_type <- ifelse(
  grepl("^\\d{4}$", pop_clean$date),
  "year_only",
  ifelse(
    grepl("^\\d{1,2} [A-Za-z]{3} \\d{4}$", pop_clean$date),
    "day_month_year",
    "other"
  )
)

table(date_type)
pop_clean[date_type == "other", ]
pop_clean[date_type == "year_only", ]
table(pop_clean$date_precision)
sum(is.na(pop_clean$date_exact))

pop_clean$year <-
  as.integer(substr(
    pop_clean$date_raw,
    nchar(pop_clean$date_raw) - 3,
    nchar(pop_clean$date_raw)
  ))
head(pop_clean)
tail(pop_clean)
sum(is.na(pop_clean$year))

library(WDI)

wdi_reserves <- WDI::WDI(
  country = "all",
  indicator = "FI.RES.TOTL.CD",
  start = 2024,
  end = 2026
)
head(wdi_reserves)
str(wdi_reserves)
table(wdi_reserves$year, useNA = "ifany")

wdi_reserves |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    non_missing = sum(!is.na(FI.RES.TOTL.CD)),
    missing = sum(is.na(FI.RES.TOTL.CD))
  )

wdi_reserves |>
  dplyr::filter(!is.na(FI.RES.TOTL.CD)) |>
  head(20)

wdi_latest <- wdi_reserves |>
  dplyr::filter(!is.na(FI.RES.TOTL.CD)) |>
  dplyr::group_by(country) |>
  dplyr::slice_max(year, n = 1, with_ties = FALSE) |>
  dplyr::ungroup()
table(wdi_latest$year)
head(wdi_latest, 20)

wdi_latest <- wdi_latest |>
  dplyr::mutate(
    reserves_million_usd = FI.RES.TOTL.CD / 1e6)

wdi_latest |>
  dplyr::select(country, year, FI.RES.TOTL.CD, reserves_million_usd) |>
  head(20)

head(WDI::WDI_data$country)
names(WDI::WDI_data$country)

real_countries <- WDI::WDI_data$country |>
  dplyr::filter(region != "Aggregates") |>
  dplyr::select(iso3c)

wdi_countries <- wdi_latest |>
  dplyr::semi_join(real_countries, by = "iso3c")

dim(wdi_countries)
wdi_countries |>
  dplyr::filter(country %in% c(
    "Euro area",
    "Arab World",
    "Africa Eastern and Southern"
  ))

comparison <- reserves_clean |>
  dplyr::left_join(
    wdi_countries |>
      dplyr::select(
        country,
        year,
        reserves_million_usd
      ),
    by = "country"
  )
comparison |>
  dplyr::filter(is.na(reserves_million_usd)) |>
  dplyr::select(country) |>
  head(30)
sum(is.na(comparison$reserves_million_usd))
unmatched <- comparison |>
  dplyr::filter(is.na(reserves_million_usd)) |>
  dplyr::select(country)

print(unmatched, n = Inf)

exists("wiki_for_join")
comparison2 <- wiki_for_join |>
  dplyr::left_join(
    wdi_countries |>
      dplyr::select(
        country_wdi = country,
        year,
        reserves_million_usd
      ),
    by = "country_wdi"
  )
sum(is.na(comparison2$reserves_million_usd))
wiki_for_join <- reserves_clean |>
  dplyr::mutate(
    country_wdi = trimws(sub("\\s*\\*$", "", country)),
    country_wdi = dplyr::recode(
      country_wdi,
      "Russia" = "Russian Federation",
      "Hong Kong" = "Hong Kong SAR, China",
      "South Korea" = "Korea, Rep.",
      "Turkey" = "Turkiye",
      "Czech Republic" = "Czechia",
      "Vietnam" = "Viet Nam",
      "Egypt" = "Egypt, Arab Rep.",
      "Iran" = "Iran, Islamic Rep.",
      "Macau" = "Macao SAR, China",
      "Ivory Coast" = "Cote d'Ivoire",
      "North Korea" = "Korea, Dem. People's Rep.",
      "Slovakia" = "Slovak Republic",
      "Venezuela" = "Venezuela, RB",
      "Brunei" = "Brunei Darussalam",
      "Kyrgyzstan" = "Kyrgyz Republic",
      "Bahamas" = "Bahamas, The",
      "Laos" = "Lao PDR",
      "Curaçao" = "Curacao",
      "Cape Verde" = "Cabo Verde",
      "Gambia" = "Gambia, The",
      "Palestine" = "West Bank and Gaza",
      "Democratic Republic of the Congo" = "Congo, Dem. Rep.",
      "Republic of the Congo" = "Congo, Rep.",
      "Syria" = "Syrian Arab Republic",
      "Federated States of Micronesia" = "Micronesia, Fed. Sts.",
      "Yemen" = "Yemen, Rep.",
      "São Tomé and Príncipe" = "Sao Tome and Principe"
    )
  )

sum(is.na(comparison2$reserves_million_usd))
comparison2 |>
  dplyr::filter(is.na(reserves_million_usd)) |>
  dplyr::select(country, country_wdi) |>
  print(n = Inf)

remaining_check <- comparison2 |>
  dplyr::filter(is.na(reserves_million_usd)) |>
  dplyr::select(country, country_wdi) |>
  dplyr::left_join(
    WDI::WDI_data$country |>
      dplyr::select(
        country_wdi = country,
        iso3c,
        region
      ),
    by = "country_wdi"
  ) |>
  dplyr::mutate(
    reason = dplyr::if_else(
      is.na(iso3c),
      "not found in WDI metadata",
      "in WDI, but no 2024-2025 reserve value"
    ))

remaining_check |>
  dplyr::count(reason)
print(remaining_check, n = Inf)

WDI::WDI_data$country |>
  dplyr::filter(
    grepl(
      "Kitts|Lucia|Vincent|Somal|Montserrat|Taiwan|Vatican",
      country,
      ignore.case = TRUE
    )
  ) |>
  dplyr::select(country, iso3c, region)

wiki_for_join <- wiki_for_join |>
  dplyr::mutate(
    country_wdi = dplyr::recode(
      country_wdi,
      "Saint Kitts and Nevis" = "St. Kitts and Nevis",
      "Saint Lucia" = "St. Lucia",
      "Saint Vincent and the Grenadines" = "St. Vincent and the Grenadines",
      "Somalia" = "Somalia, Fed. Rep."
    )
  )

comparison3 <- wiki_for_join |>
  dplyr::left_join(
    wdi_countries |>
      dplyr::select(
        country_wdi = country,
        year,
        reserves_million_usd
      ),
    by = "country_wdi"
  )
sum(is.na(comparison3$reserves_million_usd))
comparison3 |>
  dplyr::filter(is.na(reserves_million_usd)) |>
  dplyr::select(country, country_wdi) |>
  print(n = Inf)

matched_comparison <- comparison3 |>
  dplyr::filter(!is.na(reserves_million_usd)) |>
  dplyr::mutate(
    difference_million =
      reserves_incl_gold - reserves_million_usd,
    
    percent_difference =
      100 * difference_million / reserves_million_usd
  )
dim(matched_comparison)
matched_comparison |>
  dplyr::mutate(
    abs_percent_difference = abs(percent_difference)
  ) |>
  dplyr::arrange(dplyr::desc(abs_percent_difference)) |>
  dplyr::select(
    country,
    reserves_incl_gold,
    date,
    year,
    reserves_million_usd,
    percent_difference
  ) |>
  head(20)


