# Learn to scrape with RSelenium ----
# following the tutorial
# http://joshuamccrain.com/tutorials/web_scraping_R_selenium.html

# Also use the following tutorial
# https://rpubs.com/shinhaenglee/841035

library(tidyverse)
library(rvest)
library(RSelenium)
library(wdman)

# 1.0 Run the Selenium Server ----

# binman::rm_platform("phantomjs")
wdman::selenium(retcommand = TRUE)


# 2.0 Connect to a Running Server ---- 

rD <- rsDriver(browser = 'firefox', verbose = F, port = free_port(), chromever = NULL)
# assign the browser to an object (remDr)
remDr <- rD[["client"]]

# 3.0 Navigate to the page ----

remDr$navigate("https://www.fcc.gov/media/engineering/dtvmaps")

# 4.0 Perform tasks ----

## 4.1 Find information for the form ----
## 4.2 Send a string of text into that form ----
zip <- "30308"
remDr$findElement(using = "id", value = "startpoint")$sendKeysToElement(list(zip))

## 4.3 Click the "Go" button ----
remDr$findElements("id", "btnSub")[[1]]$clickElement()

# 5.0 Extract data from HTML ----

## 5.1 Save the HTML to an object ----

Sys.sleep(5) # give the page time to fully load
html <- remDr$getPageSource()[[1]]

## 5.2 extract the table of stations and their signal strength ----

signals <- read_html(html) %>% # parse HTML
    html_nodes("table.tbl_mapReception") %>% # extract table nodes with class = "tbl_mapReception"
    .[3] %>% # keep the third of these tables
    .[[1]] %>% # keep the first element of this list
    html_table(fill=T) # have rvest turn it into a dataframe

signals

# Clean data
names(signals) <- c("rm", "callsign", "network", "ch_num", "band", "rm2") # rename columns

signals <- signals %>%
    select(-1) %>%
    slice(-1) %>% # drop unnecessary first row
    filter(Callsign != "") %>% # drop blank rows
    select(Callsign:Band) # drop unnecessary columns

head(signals)

## 5.3 actual signal strength ----

read_html(html) %>% 
    html_nodes(".callsign") %>% 
    html_attr("onclick")