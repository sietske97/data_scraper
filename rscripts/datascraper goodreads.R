rm(list = ls())
library(rvest)
library(ggplot2)
library(forcats)

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
  html_text 
rating <- as.numeric(gsub("*\\savg\\srating.*|*\\s*", "", rating))

author <- books %>%
  html_nodes(".authorName span") %>%
  html_text

no_ratings <-books %>%
  html_nodes(".minirating") %>%
  html_text()
no_ratings <- as.numeric(gsub(".*rating\\s—\\s*|\\s*ratings.*$|,", "", no_ratings))


url <- books %>%
  html_nodes(".bookTitle") %>%
  html_attr("href") 
url <- paste0("https://www.goodreads.com", url)



books100 <- tibble(rank, author, titles, rating, no_ratings, url)

# number of pages, werkt nog niet helemaal
no_pages <- c()
delay_seconds <- 1

for(i in books100$url){
  recent_book <- read_html(i)
  body <- recent_book %>%
    html_nodes(".row") %>%
    html_text()
  date_time<-Sys.time()
  while((as.numeric(Sys.time()) - as.numeric(date_time))<delay_seconds){}
  if(is_empty(body)){
    body <- NA
  }
  body <- body[grep("pages", body)]
  body <- gsub("*\\spages.*", "", body)
  body <- gsub(".*\\s", "", body)
  as.numeric(no_pages <- append(no_pages, body))
}

books100 <- tibble(books100, no_pages)

# plotting
books100 <- books100 %>%
  mutate(aantal_pagina = case_when(no_pages < 101 ~ "<100",
                                   no_pages < 201 ~ "100-200",
                                   no_pages < 301 ~ "200-300",
                                   no_pages < 401 ~ "300-400",
                                   no_pages < 501 ~ "400-500",
                                   no_pages < 601 ~ "500-600",
                                   no_pages < 701 ~ "600-700",
                                   no_pages < 801 ~ "700-800",
                                   no_pages < 901 ~ "800-900", 
                                   TRUE ~ ">900"))
books100$aantal_pagina <- fct_relevel(books100$aantal_pagina, ">900", after = Inf)

books100 %>%
  group_by(aantal_pagina) %>%
  summarize(aantal = n())

books100 %>%
  group_by(aantal_pagina) %>%
  summarize(gemiddelde_rating = mean(rating, na.rm = TRUE),
            aantal_ratings = sum(no_ratings),
            aantal_boeken = n()) %>%
  mutate(ratings_per_boek = aantal_ratings/aantal_boeken) %>%
  ggplot(aes(x=aantal_pagina, y=gemiddelde_rating)) +
  geom_col() +
  ylim(0, 5) 

books100 %>%
  mutate(no_pages = as.numeric(no_pages)) %>%
  ggplot(aes(x=no_pages, y = rating, colour = no_ratings)) +
  geom_point() +
  geom_smooth(method = lm) +
  xlim(0,1000) +
  ylim(3, 5)


books100 %>%
  mutate(no_pages = as.numeric(no_pages)) %>%
  ggplot(aes(x=no_pages, y = no_ratings, colour = no_ratings)) +
  geom_point() +
  geom_smooth(method=lm) +
  xlim(0,1000) 

books100 %>%
  mutate(totaal = sum(no_ratings)) %>%
  mutate(percentage = no_ratings/totaal) %>%
  select(-url, -totaal) %>%
  arrange(percentage) %>%
  ggplot(aes(rating, no_ratings, colour = rank)) +
  geom_point()

