rm(list = ls())
library(rvest)
library(ggplot2)

# Store web url
books <- html("https://www.goodreads.com/list/show/13086.Goodreads_Top_100_Literary_Novels_of_All_Time")

#Scrape the website for the movie rating
rank <- books %>%
  html_nodes(".number") %>%
  html_text() %>%
  as.numeric()

titles <- books %>%
  html_nodes(".bookTitle span") %>%
  html_text()

rating <- books %>%
  html_nodes(".minirating") %>%
  html_text %>%
  str_sub(start = 2, end = 5) %>%
  as.numeric()

author <- books %>%
  html_nodes(".authorName span") %>%
  html_text

no_ratings <-books %>%
  html_nodes(".minirating") %>%
  html_text()

no_ratings <- sub(".*rating — ", "", no_ratings)
no_ratings <- sub("ratings", "", no_ratings)
no_ratings <- sub(",","", no_ratings)
no_ratings <- sub(",","", no_ratings)
no_ratings <- as.numeric(sub(" ","", no_ratings))


url <- books %>%
  html_nodes(".bookTitle") %>%
  html_attr("href") 
url <- paste0("https://www.goodreads.com", url)

books100 <- tibble(rank, author, titles, rating, no_ratings, url)

books100 %>%
  mutate(totaal = sum(no_ratings)) %>%
  mutate(percentage = no_ratings/totaal) %>%
  select(-url, -totaal) %>%
  arrange(percentage)


# number of pages, werkt nog niet helemaal
no_pages1 <- c()
delay_seconds <- 1
for(i in books100$url){
  recent_book <- read_html(i)
  body <- recent_book %>%
    html_nodes("#details span~ span+ span") %>%
    html_text()
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<delay_seconds){}
    if(is_empty(body)){
    body <- NA
  }
  no_pages1 <- append(no_pages1, body)
}