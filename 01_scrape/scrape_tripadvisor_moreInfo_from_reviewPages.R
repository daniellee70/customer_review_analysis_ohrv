# Scrape detailed ATV tour info from tripadvisor.com ----
# scraped on 6/26/2023 

# Learn to scrape with RSelenium ----
# following the tutorial
# http://joshuamccrain.com/tutorials/web_scraping_R_selenium.html

# Also use the following tutorial
# https://rpubs.com/shinhaenglee/841035

library(tidyverse)
library(rvest)
library(RSelenium)
library(wdman)

# 1.0 Set up ----

Tour_info_tbl <- read_rds("../00_data/data_scraped/Tour_info_tbl.rds")
## 1.1 Run the Selenium Server ----

# binman::rm_platform("phantomjs")
wdman::selenium(retcommand = TRUE)


## 1.2 Open a Server ---- 

rD <- rsDriver(browser = 'firefox', verbose = F, port = netstat::free_port(), chromever = NULL)

## 1.3 Assign the browser to an object (remDr) ----
remDr <- rD[["client"]]

## 1.4 Navigate to the page ----

remDr$navigate("https://www.tripadvisor.com/AttractionProductReview-g60724-d23508631-Guided_Hell_s_Revenge_UTV_Tour-Moab_Utah.html")

# 2.0 Extract Data ----

## 2.1 What is included ----

# Click the "Go" button
remDr$findElements("id", ":lithium-Repiitkqlsnkla:")[[1]]$clickElement()

# Save the HTML to an object
html <- remDr$getPageSource()[[1]]

## Scrape data
included <- read_html(html) %>% # parse HTML
    html_nodes("div[id=':lithium-RepiitkqlsnklaH1:']") %>% 
    html_text2() %>% 
    str_split_1("What's not included") %>% 
    .[1]

## 2.2 What is NOT included ----
not_included <- read_html(html) %>% # parse HTML
    html_nodes("div[id=':lithium-RepiitkqlsnklaH1:']") %>% 
    html_text2() %>% 
    str_split_1("What's not included") %>% 
    .[2]

## 2.3 What to expect ----

## Click the "Go" button
# remDr$findElements("id", ":lithium-Rmpiitkqlsnkla:")[[1]]$clickElement()

what_to_expect <- read_html(html) %>% # parse HTML
    html_nodes(".tyUdl._d") %>% 
    html_text2()

## 2.4 Departure and Return ----

remDr$findElements("id", ":lithium-Rupiitkqlsnkla:")[[1]]$clickElement()

depart <- read_html(html) %>% # parse HTML
    html_nodes("div[id=':lithium-RupiitkqlsnklaH1:'] div[class='biGQs _P pZUbB KxBGd'] div div:nth-child(1)") %>% 
    .[[1]] %>% 
    html_text2()

## 2.5 Accessibility ----

## Click the "Go" button
# remDr$findElements("id", ":lithium-R16piitkqlsnkla:")[[1]]$clickElement()

accessibility <- read_html(html) %>% # parse HTML
    html_nodes("div[id=':lithium-R16piitkqlsnklaH1:']") %>% 
    .[[1]] %>% 
    html_elements(".IMSns") %>% 
    html_text2()

## 2.6 Additional info ----

## Click the "Go" button
# remDr$findElements("id", ":lithium-R1epiitkqlsnkla:")[[1]]$clickElement()

additional_info <- read_html(html) %>% # parse HTML
    html_nodes("div[id=':lithium-R1epiitkqlsnklaH1:']") %>% 
    html_text2()

## 2.7 Cancellation policy ----

## Click the "Go" button
# remDr$findElements("id", ":lithium-R1mpiitkqlsnkla:")[[1]]$clickElement()

cancel_policy <- read_html(html) %>% # parse HTML
    html_nodes("div[id=':lithium-R1mpiitkqlsnklaH1:'] div[class='biGQs _P pZUbB KxBGd'] div div[class='biGQs _P pZUbB KxBGd']") %>% 
    html_text2()

## 2.8 Covid safety ----

covid_safety <- read_html(html) %>% # parse HTML
    html_nodes("div[id=':lithium-R1upiitkqlsnklaH1:'] ul[class='IMSns']") %>% 
    html_text2()


## 2.9 FAQ ----

## Click the "Go" button
# remDr$findElements("id", ":lithium-R1upiitkqlsnkla:")[[1]]$clickElement()

faq <- read_html(html) %>% # parse HTML
    html_nodes("body div[id='lithium-root'] main div[data-test-target='fusion-screen-detail'] div[class='hJiTo z xuKAb'] div div[class='Skses z vECdA fzvYt'] div[class='xfLuN'] div[class='C'] section[class='vwOfI nlaXM'] div[data-automation='WebPresentation_PoiInlineBookingSectionGroup'] div[class='QvCXh mvTrV cyIij fluiI'] div[class='wAiJR'] div[class='SHPAN'] div[class='IsYTu A'] section[class='vwOfI nlaXM'] div[data-automation='WebPresentation_ProductAboveTheFoldAccordion'] div[class='C'] dl:nth-child(1)") %>% 
    html_text2()

## 2.9 Help ----

## Click the "Go" button
# remDr$findElements("id", ":lithium-R26piitkqlsnkla:")[[1]]$clickElement()

help <- read_html(html) %>% # parse HTML
    html_nodes("div[id=':lithium-R2epiitkqlsnklaH1:'] div[class='tyUdl'] div:nth-child(1)") %>% 
    html_text2()

# 3.0 Iteration ----
## 1.1 Run the Selenium Server
# binman::rm_platform("phantomjs")
wdman::selenium(retcommand = TRUE)

## 1.2 Open a Server 
rD <- rsDriver(browser = 'firefox', verbose = F, port = netstat::free_port(), chromever = NULL)

## 1.3 Assign the browser to an object (remDr) 
remDr <- rD[["client"]]


get_detailed_tour_info <- function(link) {
    
    ## Navigate to the page
    remDr$navigate(link)
    
    Sys.sleep(3)
    
    # Click the "Go" button
    remDr$findElements("id", ":lithium-Repiitkqlsnkla:")[[1]]$clickElement()
    
    # Save the HTML to an object
    Sys.sleep(5) # give the page time to fully load
    html <- remDr$getPageSource()[[1]]
    
    ## 2.1 What is included ----
    included <- read_html(html) %>% # parse HTML
        html_nodes("div[id=':lithium-RepiitkqlsnklaH1:']") %>% 
        html_text2() %>% 
        str_split_1("What's not included") %>% 
        .[1]
    
    ## 2.2 What is NOT included ----
    not_included <- read_html(html) %>% # parse HTML
        html_nodes("div[id=':lithium-RepiitkqlsnklaH1:']") %>% 
        html_text2() %>% 
        str_split_1("What's not included") %>% 
        .[2]
    
    ## 2.3 What to expect ----
    what_to_expect <- read_html(html) %>% # parse HTML
        html_nodes(".tyUdl._d") %>% 
        html_text2()
    
    ## 2.4 Departure and Return ----
    depart <- read_html(html) %>% # parse HTML
        html_nodes("div[id=':lithium-RupiitkqlsnklaH1:'] div[class='biGQs _P pZUbB KxBGd'] div div:nth-child(1)") %>% 
        .[[1]] %>% 
        html_text2()
    
    ## 2.5 Accessibility ----
    accessibility <- read_html(html) %>% # parse HTML
        html_nodes("div[id=':lithium-R16piitkqlsnklaH1:']") %>% 
        .[[1]] %>% 
        html_elements(".IMSns") %>% 
        html_text2()
    
    ## 2.6 Additional info ----
    additional_info <- read_html(html) %>% # parse HTML
        html_nodes("div[id=':lithium-R1epiitkqlsnklaH1:']") %>% 
        html_text2()
    
    ## 2.7 Cancellation policy ----
    cancel_policy <- read_html(html) %>% # parse HTML
        html_nodes("div[id=':lithium-R1mpiitkqlsnklaH1:'] div[class='biGQs _P pZUbB KxBGd'] div div[class='biGQs _P pZUbB KxBGd']") %>% 
        html_text2()
    
    ## 2.8 Covid safety ----
    # covid_safety <- read_html(html) %>% # parse HTML
    #     html_nodes("div[id=':lithium-R1upiitkqlsnklaH1:'] ul[class='IMSns']") %>% 
    #     html_text2()
    
    if(length(included)==0) { included <- "none" }
    if(length(not_included)==0) { not_included <- "none" }
    if(length(what_to_expect)==0) { what_to_expect <- "none" }
    if(length(depart)==0) { depart <- "none" }
    if(length(accessibility)==0) { accessibility <- "none" }
    if(length(additional_info)==0) { additional_info <- "none" }
    if(length(cancel_policy)==0) { cancel_policy <- "none" }
    # if(length(covid_safety)==0) { covid_safety <- "none" }
    
    detailed_info_tbl <- tibble(included, not_included, what_to_expect, depart,
                                accessibility, additional_info, cancel_policy)
    
    return(detailed_info_tbl)
    
    Sys.sleep(runif(1,1,3))
}

detailed_info_tbl <- map(Tour_info_tbl$Review_links, possibly(get_detailed_tour_info, otherwise = NA)) %>%
    enframe() %>% 
    unnest(value)

# write_rds(detailed_info_tbl, "00_data/data_scraped/detailed_info_tbl.rds")


# 4.0 Clean ----

detailed_info_tbl <- read_rds("../00_data/data_scraped/detailed_info_tbl.rds")

## 4.1 What is included ----
# Examine common categories
detailed_info_tbl %>%
    select(name, included) %>%
    separate_rows(included, sep = "\n") %>%
    
    # Combine similar categories
    mutate(included = if_else(str_detect(included, regex("entry|admission", ignore_case = T)), 
                              "Entry/Admission", included)) %>%
    mutate(included = if_else(str_detect(included, regex("tax", ignore_case = T)), 
                              "Taxes", included)) %>%
    mutate(included = if_else(str_detect(included, regex("guide", ignore_case = T)), 
                              "Guide", included)) %>%
    mutate(included = if_else(str_detect(included, regex("snack|lunch|meal", ignore_case = T)), 
                              "Lunch or Snacks", included)) %>%
    mutate(included = if_else(str_detect(included, regex("helmet", ignore_case = T)), 
                              "Helmets", included)) %>%
    mutate(included = if_else(str_detect(included, regex("pick|transportation", ignore_case = T)), 
                              "Pick-up & Drop off", included)) %>%
    mutate(included = if_else(str_detect(included, regex("water|coffee|tea|beverage|drink", ignore_case = T)), 
                              "Water or Beverages", included)) %>%
    count(included, sort = T) %>% 
    head(20)

included_tbl <- detailed_info_tbl %>%
    select(name, included) %>%
    separate_rows(included, sep = "\n") %>%
    
    # Combine similar categories
    mutate(included = if_else(str_detect(included, regex("entry|admission", ignore_case = T)), 
                              "Entry/Admission", included)) %>%
    mutate(included = if_else(str_detect(included, regex("tax", ignore_case = T)), 
                              "Taxes", included)) %>%
    mutate(included = if_else(str_detect(included, regex("guide", ignore_case = T)), 
                              "Guide", included)) %>%
    mutate(included = if_else(str_detect(included, regex("snack|lunch|meal", ignore_case = T)), 
                              "Lunch or Snacks", included)) %>%
    mutate(included = if_else(str_detect(included, regex("helmet", ignore_case = T)), 
                              "Helmets", included)) %>%
    mutate(included = if_else(str_detect(included, regex("pick|transportation", ignore_case = T)), 
                              "Pick-up & Drop off", included)) %>%
    mutate(included = if_else(str_detect(included, regex("water|coffee|tea|beverage|drink", ignore_case = T)), 
                              "Water or Beverages", included)) %>%
    # A category may appear multiple times due to factor collapse
    distinct(name, included) %>%
    
    # Remove rare categories
    add_count(included) %>%
    filter(n > 20) %>%
    select(-n) %>%
    filter(str_detect(included, "[a-z]")) %>%
    
    # Transform wide
    mutate(value = "yes") %>%
    pivot_wider(names_from = included, values_from = value, 
                values_fill = "no", names_glue = "include_{included}") %>%
    janitor::clean_names()

## 4.2 What is NOT included ----

# Examine common categories
detailed_info_tbl %>%
    select(name, not_included) %>%
    separate_rows(not_included, sep = "\n") %>%
    filter(str_detect(not_included, "[a-z]")) %>%
    
    # Combine similar categories
    mutate(not_included = if_else(str_detect(not_included, regex("entry|admission", ignore_case = T)), 
                              "Entry/Admission", not_included)) %>%
    mutate(not_included = if_else(str_detect(not_included, regex("tax", ignore_case = T)), 
                              "Taxes", not_included)) %>%
    mutate(not_included = if_else(str_detect(not_included, regex("guide", ignore_case = T)), 
                              "Guide", not_included)) %>%
    mutate(not_included = if_else(str_detect(not_included, regex("snack|lunch|meal", ignore_case = T)), 
                              "Lunch or Snacks", not_included)) %>%
    mutate(not_included = if_else(str_detect(not_included, regex("helmet", ignore_case = T)), 
                              "Helmets", not_included)) %>%
    mutate(not_included = if_else(str_detect(not_included, regex("pick|transportation", ignore_case = T)), 
                              "Pick-up & Drop off", not_included)) %>%
    mutate(not_included = if_else(str_detect(not_included, regex("water|coffee|tea|beverage|drink", ignore_case = T)), 
                              "Water or Beverages", not_included)) %>%
    mutate(not_included = if_else(str_detect(not_included, regex("gratuit", ignore_case = T)), 
                                  "Gratuities", not_included)) %>%
    # A category may appear multiple times due to factor collapse
    distinct(name, not_included) %>%
    
    count(not_included, sort = T)

not_included_tbl <- detailed_info_tbl %>%
    select(name, not_included) %>%
    separate_rows(not_included, sep = "\n") %>%
    filter(str_detect(not_included, "[a-z]")) %>%
    
    # Combine similar categories
    mutate(not_included = if_else(str_detect(not_included, regex("entry|admission", ignore_case = T)), 
                                  "Entry/Admission", not_included)) %>%
    mutate(not_included = if_else(str_detect(not_included, regex("tax", ignore_case = T)), 
                                  "Taxes", not_included)) %>%
    mutate(not_included = if_else(str_detect(not_included, regex("guide", ignore_case = T)), 
                                  "Guide", not_included)) %>%
    mutate(not_included = if_else(str_detect(not_included, regex("snack|lunch|meal", ignore_case = T)), 
                                  "Lunch or Snacks", not_included)) %>%
    mutate(not_included = if_else(str_detect(not_included, regex("helmet", ignore_case = T)), 
                                  "Helmets", not_included)) %>%
    mutate(not_included = if_else(str_detect(not_included, regex("pick|transportation", ignore_case = T)), 
                                  "Pick-up & Drop off", not_included)) %>%
    mutate(not_included = if_else(str_detect(not_included, regex("water|coffee|tea|beverage|drink", ignore_case = T)), 
                                  "Water or Beverages", not_included)) %>%
    mutate(not_included = if_else(str_detect(not_included, regex("gratuit", ignore_case = T)), 
                                  "Gratuities", not_included)) %>%
    # A category may appear multiple times due to factor collapse
    distinct(name, not_included) %>%
    
    # Remove rare categories
    add_count(not_included) %>%
    filter(n >= 7) %>%
    select(-n) %>%

    # Transform wide
    mutate(value = "yes") %>%
    pivot_wider(names_from = not_included, values_from = value,
                values_fill = "no", names_glue = "not_include_{not_included}") %>%
    janitor::clean_names()


## 4.3 longitude and latitude ----
library(tidygeocoder)

geo_raw_tbl <- detailed_info_tbl %>% 
    mutate(zip_codes = str_extract(depart, "[0-9]{4}[0-9](,|\\.)")) %>%
    geocode(zip_codes, method = 'arcgis', lat = latitude , long = longitude) %>% 
    select(name, zip_codes, latitude, longitude)

geo_with_zip_tbl <- geo_raw_tbl %>%
    filter(!is.na(latitude))

detailed_info_tbl %>% 
    mutate(zip_codes = str_extract(depart, "[0-9]{4}[0-9]")) %>%
    filter(is.na(zip_codes)) %>%
    select(name, depart, zip_codes)

# Get 20 missing values from Google Map
geo_no_zip_tbl <- tribble(
    ~name, ~latitude, ~longitude,
    "2. Private Washington DC Tour", 38.905626006053716, -77.03780720666887,
    "14. VIP Private Boutique Tour of Yellowstone's Lower Loop - West Yellowstone, MT", 44.66096979609458, -111.10454262161474,
    "15. 3.0 Hours of Monument Valley's Sunrise or Sunset 4×4 Tour", 36.96958322428195, -110.08015948888695,
    "16. Kualoa Ranch - Jurassic Adventure Tour", 21.518862300449957, -157.85341077848153,
    "20. 3-hour Private Tour", 36.11930737263048, -109.31966228949454,
    "28. VIP Private Custom Road to Hana Tour with Pick Up", 20.80151031634496, -156.30981420920583,
    "29. 2.5 Hours of Monument Valley's Backcountry 4×4 Tour", 36.967033054461034, -110.07966596246017,
    "32. Yellowstone Private Tour", 44.65950458912027, -111.10608757390726,
    "34. Canyonlands National Park Half-Day Tour from Moab", 38.263939266707034, -109.9049469555969,
    "36. Perfect Private Night Tour of Washington DC", 38.895241945642354, -77.03313388705055,
    "74. 1.5 Hour Tour of Monument Valley's Loop Drive", 36.96710163157402, -110.07961231828335,
    "75. Day-Time or Sunset Big Iron Off-Road Guided Tour of Moab", 38.57560397763644, -109.55244880949633,
    "83. Extreme RZR Tour of Hidden Valley and Primm from Las Vegas", 36.21687071892803, -115.14784060598322,
    "86. Guided Half-Day Tour to Shoshone Falls & City Tour", 42.593857936244135, -114.40097854678459,
    "89. 3.5 Hours of Monument Valley's Cultural 4×4 Tour", 36.96710163157402, -110.07961231828335,
    "94. Guided UTV Sand Buggy Tour Scottsdale - 2 Person Vehicle in Sonoran Desert", 34.876530705211906, -111.79440709140843,
    "95. Las Vegas Valley of Fire ATV Quad 3 hour Tour MOST SCENIC!", 36.1709255636519, -115.18540028717888,
    "96. Hoover Dam Tour by Luxury SUV", 36.0162216770892, -114.73775396251227,
    "99. 20-Minute Grand Canyon Helicopter Flight with Optional Upgrades: ATV + Horseback", 36.058025838034126, -112.14144788545816,
    "107. Area 51 Day Tour from Las Vegas", 37.348742619861284, -115.81151817082039)

# Combine
geo_tbl <- bind_rows(geo_with_zip_tbl, geo_no_zip_tbl) %>%
    select(-zip_codes)

geo_tbl

## 4.4 Cancel policy ----
detailed_info_tbl %>%
    count(cancel_policy)

cancel_policy_tbl <- detailed_info_tbl %>%
    mutate(full_refund = case_when(
        str_detect(cancel_policy, "All sales are final") ~ "no",
        str_detect(cancel_policy, "For a full refund") ~ "yes",
        TRUE ~ cancel_policy
    )) %>%
    select(name, full_refund)

## 4.5 Accessibility ----
# Examine common categories
detailed_info_tbl %>% 
    select(name, accessibility) %>% 
    separate_longer_delim(col = accessibility, delim = "\n") %>% 
    count(accessibility, sort = T)

# Extract attributes
accessibility_tbl <- detailed_info_tbl %>% 
    select(name, accessibility) %>% 
    separate_longer_delim(col = accessibility, delim = "\n") %>%
    
    # Remove less frequent categories
    add_count(accessibility) %>% 
    filter(n >= 7) %>%
    select(-n) %>%
    
    # Remove because only accessible is useful for analysis
    filter(accessibility != "Not wheelchair accessible") %>%
    
    # Combine two infant categories
    mutate(accessibility = fct_collapse(accessibility,
        "infant allowed" = c("Infant seats available", "Infants must sit on laps"),
    )) %>%
    # the infant accessibility may appear twice due to factor collapse
    distinct(name, accessibility) %>%  
    
    mutate(value = "yes") %>% 
    pivot_wider(names_from = accessibility, values_from = value, 
                values_fill = "no", names_glue = "access_{accessibility}") %>%
    janitor::clean_names()


## 4.6 Merge
detailed_info_clean_tbl <- detailed_info_tbl %>%
    
    # Get all 110 tours
    select(name) %>%
    
    left_join(included_tbl, by = "name") %>%
    left_join(not_included_tbl, by = "name") %>%
    left_join(geo_tbl, by = "name") %>%
    left_join(cancel_policy_tbl, by = "name") %>%
    left_join(accessibility_tbl, by = "name") %>%
    
    mutate(across(where(is.character), factor)) %>%
    
    # Replace NA with no b/c being not mentioned mean not available
    mutate(across(!ends_with("tude"), replace_na, "no"))

detailed_info_clean_tbl %>% glimpse()

write_rds(detailed_info_clean_tbl, "../00_data/data_wrangled/detailed_info_clean_tbl.rds")

