# Scrape ATV tour reviews from tripadvisor.com ----
# scraped on 6/19/2023 to 6/20/2023

library(tidyverse)
library(rvest)

# 1.0 Scrape only 1st page ---- 
# There are multiple pages of tours

# Examine the website to understand the code.

url <- "https://www.tripadvisor.com/Attractions-g191-Activities-c61-t211-United_States.html"

page <- read_html(url)

## 1.1 Tour name ----

Tour_names <- page %>%
    html_nodes(".AIbhI") %>%
    html_text(trim = TRUE) 

Tour_names

## 1.2 Rankings ----

# Consider using TripAdvisor.com traveler favorite rankings
# Tours, activities and experiences bookable on Tripadvisor, ranked according to revenue made by Tripadvisor on those bookings, plus the number of page views by Tripadvisor users, and the quantity and quality of reviews

# Extract it from Tour_names

## 1.3 Operators ----

Operators <- page %>%
    html_nodes(".hmDzD .SwZTJ") %>%
    html_text() %>%
    str_remove("By ")

Operators

## 1.4 Types ----

# Not all tour has this info: 22 out of 30 

## 1.5 Length ----

Length <- page %>%
    html_nodes(".fOSqw") %>%
    html_text() %>%
    str_remove("By ")

Length

## 1.6 Free cancellation ----

# Not all tour has this info: 28 out of 30

## 1.7 Recommendation ----
# Not all tour has this info: 29 out of 30
# Recommend <- page %>%
#     html_nodes(".P") %>%
#     html_text() 
# 
# Recommend

## 1.8 Price ----

Price <- page %>%
    html_nodes(".avBIb.ngXxk") %>%
    html_text() %>%
    parse_number() %>%
    as.integer()

Price

## 1.9 Price per ----

Price_per <- page %>%
    html_nodes(".ZbAEz div~ div+ div") %>%
    html_text() 

Price_per

## 1.10 Description ----

Description <- page %>%
    html_nodes(".SwTtt") %>%
    html_text() 

Description

## 1.10 Review counts ----

n_reviews <- page %>%
    html_nodes(".JqMhy") %>%
    html_element("span") %>% 
    html_text() %>% 
    str_remove_all("[:punct:]") %>% 
    as.integer() 

n_reviews

# 2.0 Scrape all four pages ----

## 2.1 Tour links ----

# https://www.tripadvisor.com/Attractions-g191-Activities-c61-t211-United_States.html
# https://www.tripadvisor.com/AttractionProductReview-g60724-d11468011-Hell_s_Revenge_4x4_Off_Roading_Tour_from_Moab-Moab_Utah.html
# https://www.tripadvisor.com/AttractionProductReview-g60724-d11454838-Arches_National_Park_4x4_Adventure_from_Moab-Moab_Utah.html 
# Review_links <- page %>%
#     html_nodes(".hmDzD .SwZTJ") %>%
#     html_attr("href") %>%
#     paste0("https://www.tripadvisor.com", .)

# Get links for the first page of tours
# Review_links <- page %>%
#     html_nodes(".VLKGO .tnGGX") %>% 
#     html_element("a") %>% 
#     html_attr("href") %>%
#     paste0("https://www.tripadvisor.com", .)

url_1 <- "https://www.tripadvisor.com/Attractions-g191-Activities-c61-t211-United_States.html"
url_2 <- "https://www.tripadvisor.com/Attractions-g191-Activities-c61-t211-oa30-United_States.html"
url_3 <- "https://www.tripadvisor.com/Attractions-g191-Activities-c61-t211-oa60-United_States.html"
url_4 <- "https://www.tripadvisor.com/Attractions-g191-Activities-c61-t211-oa90-United_States.html"
urls <- c(url_1, url_2, url_3, url_4)


## 2.2 Tour names ----

get_tour_names <- function(url) {
    
    page <- read_html(url)
    
    Tour_name <- page %>%
        html_nodes(".AIbhI") %>%
        html_text(trim = TRUE) 
    
    return(Tour_name)
}

Tour_names <- map(urls, get_tour_names) %>%
    reduce(c)

Tour_names

## 2.3 Rankings ----

# Consider using TripAdvisor.com traveler favorite rankings
# Tours, activities and experiences bookable on Tripadvisor, ranked according to revenue made by Tripadvisor on those bookings, plus the number of page views by Tripadvisor users, and the quantity and quality of reviews

# Extract it from Tour_names

## 2.4 Operators ----

Operators <- page %>%
    html_nodes(".hmDzD .SwZTJ") %>%
    html_text() %>%
    str_remove("By ")


get_tour_operators <- function(url) {
    
    page <- read_html(url)
    
    Operators <- page %>%
        html_nodes(".hmDzD .SwZTJ") %>%
        html_text() %>%
        str_remove("By ")
    
    return(Operators)
}

Operators <- map(urls, get_tour_operators) %>%
    reduce(c)
Operators

## 2.5 Length ----

get_tour_length <- function(url) {
    
    page <- read_html(url)
    
    Length <- page %>%
        html_nodes(".fOSqw") %>%
        html_text() %>%
        str_remove("By ")
    
    return(Length)
}

Length <- map(urls, get_tour_length) %>%
    reduce(c)
Length


## 2.6 Price ----

Price <- page %>%
    html_nodes(".avBIb.ngXxk") %>%
    html_text() %>%
    parse_number() %>%
    as.integer()

get_tour_price <- function(url) {
    
    page <- read_html(url)
    
    Price <- page %>%
        html_nodes(".avBIb.ngXxk") %>%
        html_text() %>%
        parse_number() %>%
        as.integer()
    
    return(Price)
}

Price <- map(urls, get_tour_price) %>%
    reduce(c)
Price

## 2.7 Price per ----

get_tour_price_per <- function(url) {
    
    page <- read_html(url)
    
    Price_per <- page %>%
        html_nodes(".ZbAEz div~ div+ div") %>%
        html_text() 
    
    return(Price_per)
}

Price_per <- map(urls, get_tour_price_per) %>%
    reduce(c)
Price_per

## 2.8 Description ----

get_tour_desc <- function(url) {
    
    page <- read_html(url)
    
    Description <- page %>%
        html_nodes(".SwTtt") %>%
        html_text() 
    
    return(Description)
}

Description <- map(urls, get_tour_desc) %>%
    reduce(c)
Description

## 2.9 Review counts ----

get_n_reviews <- function(url) {
    
    page <- read_html(url)
    
    n_reviews <- page %>%
        html_nodes(".JqMhy") %>%
        html_element("span") %>% 
        html_text() %>% 
        str_remove_all("[:punct:]") %>% 
        as.integer() 
    
    return(n_reviews)
}

n_reviews <- map(urls, get_n_reviews) %>%
    reduce(c)
n_reviews


# 3.0 Scrape reviews ----

## 3.1 Review_page urls ----
get_links_to_review_pages <- function(url) {
    
    page <- read_html(url)
    
    Review_links <- page %>%
        html_nodes(".VLKGO .tnGGX") %>%
        html_element("a") %>%
        html_attr("href") %>%
        paste0("https://www.tripadvisor.com", .)
    
    return(Review_links)
}

Review_links <- map(urls, get_links_to_review_pages) %>%
    reduce(c)

Review_links

# Name the link vector to use map_db?
names(Review_links) <- Tour_names
Review_links 

## 3.2 Merge ----

Tour_info_tbl <- tibble(Tour_names, Description, Price, Price_per, 
                        Length, Operators, n_reviews, Review_links) %>%
    
    # Add rankings of Travelor favorates
    mutate(Rankings = parse_number(Tour_names))

Tour_info_tbl

write_rds(Tour_info_tbl, "00_data/data_scraped/Tour_info_tbl.rds")

## 3.3 Get reviews

get_tour_reviews <- function(link){

    # link <- "https://www.tripadvisor.com/AttractionProductReview-g32846-d11481405-Joshua_Tree_Open_Air_Hummer_Adventure-Palm_Desert_Greater_Palm_Springs_California.html"
    review_page <- read_html(link)
    
    tour_rating <- review_page %>%
        html_nodes(".f.k+ div") %>% 
        html_elements("svg") %>% 
        html_attr("aria-label") %>% 
        parse_number() 
    
    tour_name <- review_page %>%
        html_nodes(".EVnyE") %>%
        html_text(trim = TRUE) %>%
        
        # Sometimes it extracts additional string
        paste(collapse = " & ") %>%
        str_split_1(pattern = " & ") %>% .[1]
    
    reviewer <- review_page %>%
        html_nodes(".f .o .zpDvc") %>%
        html_text()
    
    review <- review_page %>%
        html_nodes(".KxBGd .yCeTE") %>%
        html_text(trim = TRUE)
    
    review_tbl <- tibble(tour_name, tour_rating, reviewer, review)
    
    return(review_tbl)
}

## 3.3 One page reviews for 1 tour ----
get_tour_reviews(link = Review_links[1])

## 3.4 One page reviews of 2 tours ----
map(Review_links[1:2], get_tour_reviews) %>%
    
    # Convert to a nested tibble with dates column
    enframe() %>% 
    unnest(cols = value)

## 3.5 All page reviews of all tours ----

# https://www.tripadvisor.com/AttractionProductReview-g60724-d23508631-Guided_Hell_s_Revenge_UTV_Tour-Moab_Utah.html
# https://www.tripadvisor.com/AttractionProductReview-g60724-d23508631-or10-Guided_Hell_s_Revenge_UTV_Tour-Moab_Utah.html
# https://www.tripadvisor.com/AttractionProductReview-g60724-d23508631-or20-Guided_Hell_s_Revenge_UTV_Tour-Moab_Utah.html

# looping over three arguments
# allowing the number of reviews to vary across tours 

get_tour_reviews_of_multi_pages <- function(review_link, review_count, tour) {
    
    message(paste0(tour, ": ", review_link))
    
    last_review_page <- (review_count-1) %/% 10 * 10
    tour_reviews_multi <- data.frame()
    
    for (i in seq(from = 0, to = last_review_page, by = 10)){
        link_1 <- review_link %>% 
            map(.x = ., .f = ~ str_split_1(.x, "-") %>% .[1:3] %>% paste(collapse = "-") %>% paste0("-or", i, "-"))
        
        link_2 <- review_link %>% 
            map(.x = ., .f = ~ str_split_1(.x, "-") %>% .[4:5] %>% paste(collapse = "-"))
        
        multipage_links <- paste0(link_1, link_2)
        
        # Sys.sleep pauses R for two seconds before it resumes
        # Putting it there avoids error messages such as "Error in open.connection(con, "rb") : Timeout was reached"
        Sys.sleep(20)
        
        # Extract titles 
        titles_per_page <- map(multipage_links, possibly(get_tour_reviews, otherwise = NA)) %>%
            
            # Convert to a nested tibble with dates column
            enframe() %>%
            mutate(name = multipage_links) %>% 
            unnest(cols = value)%>%
            
            # Add a page number
            mutate(page = i)
        
        tour_reviews_multi <- bind_rows(tour_reviews_multi, titles_per_page)
        
        print(paste0("Page", i))
    }
    
    return(tour_reviews_multi)
    
}

# Split the scraping into small pieces each run. Overwhelming request to the website will likely freeze the process.

Tour_reviews_multi_list <- list(Tour_info_tbl[c(5,37),]$Review_links, 
                                Tour_info_tbl[c(5,37),]$n_reviews, 
                                Tour_info_tbl[c(5,37),]$Tour_names) %>%
    pmap(get_tour_reviews_of_multi_pages)

# Check for duplicate reviews
Tour_reviews_multi_list_1_to_10 %>% 
    enframe() %>% 
    set_names(c("Tour_names", "Review_info")) %>% 
    unnest(Review_info) %>% 
    distinct(reviewer, review)

# write_rds(Tour_reviews_multi_list, "00_data/data_scraped/Tour_reviews_multi_list_5_37.rds")

Tour_reviews_multi_list_1_to_10 <- read_rds("00_data/data_scraped/Tour_reviews_multi_list_1_to_10.rds")
Tour_reviews_multi_list_11_to_20 <- read_rds("00_data/data_scraped/Tour_reviews_multi_list_11_to_20.rds")
Tour_reviews_multi_list_21_to_110 <- read_rds("00_data/data_scraped/Tour_reviews_multi_list_21_to_110.rds")
Tour_reviews_multi_list_64_to_90 <- read_rds("00_data/data_scraped/Tour_reviews_multi_list_64_to_90.rds")
Tour_reviews_multi_list_5_37 <- read_rds("00_data/data_scraped/Tour_reviews_multi_list_5_37.rds")

# Combine reviews
Tour_reviews_multi_tbl <- list(Tour_reviews_multi_list_1_to_10,
                                Tour_reviews_multi_list_11_to_20,
                                Tour_reviews_multi_list_21_to_110,
                                Tour_reviews_multi_list_64_to_90,
                                Tour_reviews_multi_list_5_37) %>%
    
    # Convert it to a list of tibbles
    map(enframe) %>%
    
    # Convert it to a tibble
    reduce(bind_rows) %>%
    
    filter(!is.na(value)) %>% 
    set_names(c("Tour_names", "Review_info"))
                                

# 4.0 Merge with other tour info ----
Tour_info_review_final_tbl <- Tour_reviews_multi_tbl %>% 
    right_join(Tour_info_tbl, by = "Tour_names")

Tour_info_review_final_tbl %>% glimpse()

write_rds(Tour_info_review_final_tbl, "00_data/data_scraped/Tour_info_review_final_tbl.rds")



# To do list
# 1. Fix the vector Recommend. It should be the length of 30, not 60.
# 2. Verify the reviews in tour_reviews_multi. Reviews and pages don't match.
# 3. 




