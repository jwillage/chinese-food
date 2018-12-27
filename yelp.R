require(httr)
require(httpuv)
require(jsonlite)
require(base64enc)
library(tidyverse)
library(rvest)

api_key <- readLines("key.txt")

key <- api_key
term <- "chinese food"
radius <- 40000
categories <- "chinese, all"
limit <- 50

paginate_results <- function(location, index) {
  res <- GET(paste0("https://api.yelp.com/v3/businesses/search"), 
             add_headers(Authorization=paste("Bearer", api_key)),
             query = list(term=term, location=location, radius=radius, categories=categories, 
                          limit=limit,offset=index))
  
  ct <-content(res)
  names <- lapply(ct$businesses, function(business) {
    data.frame(name=business$name, id=business$id, rating=business$rating, location=location,
               delivery=ifelse(grepl("delivery", 
                               paste(unlist(business$transactions), collapse=" ")), 
                               "Delivery", "No Delivery"),
               stringsAsFactors = FALSE)
  }) %>% 
    bind_rows()

  if((index+length(ct$businesses)) < ct$total &
     (index+length(ct$businesses)) < 1000) {
    names <- rbind(names, 
                   paginate_results(location, 
                                    index = index+length(ct$businesses)))
  }

  names
  
}

url <- "https://www.biggestuscities.com/"
cities <- url %>%
  html() %>%
  html_nodes(xpath='/html/body/div[2]/div[2]/div/table[1]') %>%
  html_table()
cities <- cities[[1]]
cities <- unlist(paste(cities$City, cities$State, sep = ", "))

names <- NULL
for (city in cities) {
  print(city)
  location <- city
  tmp_names <- paginate_results(location, index=0)
  
  names <- rbind(names, tmp_names)
  saveRDS(names, "names.RDS")
  
}

all_cities <- names %>% distinct(id, .keep_all = T)

#percentage of "great wall" and relaetd
all_cities[grepl("great wall", all_cities$name, ignore.case = T), ] %>% 
  nrow() / nrow(all_cities)
all_cities[grepl("(?=.*great)(?!.*wall)", all_cities$name, ignore.case = T, perl=T), ] %>% 
  nrow() / nrow(all_cities)
sum(grepl("wall", all_cities[grepl("(?!.*great)", all_cities$name, ignore.case = T, perl=T), 
                             ]$name)) / nrow(all_cities)

#most common names
all_cities$edit <- trimws(gsub("restaurant", "", 
                          gsub("chinese", "", 
                          gsub("of", "", 
                          gsub("the", "", all_cities$name, ignore.case = T), 
                                                           ignore.case = T),
                                                           ignore.case = T), 
                                                           ignore.case = T))
# top names
all_cities %>% 
  group_by(edit) %>% 
  summarize(n=n(), 
            pct=n/nrow(all_cities)) %>% 
  arrange(desc(n)) %>% 
  head(10)

# top words
toks <- tidytext::unnest_tokens(all_cities, word, name)
toks %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(percent = paste0(round(n/nrow(all_cities)*100, 2), "%")) %>%
  head(10)

# top words w/o panda express
toks_np <- tidytext::unnest_tokens(all_cities %>% filter(name != "Panda Express"), word, name)
toks_np %>% 
  count(word) %>% 
  arrange(desc(n)) %>% 
  mutate(percent = paste0(round(n/nrow(all_cities)*100, 2), "%")) %>%
  as.data.frame() %>%
  head(10)

# city and rating relationship
city_rating <- names %>% group_by(location) %>% summarize(n=n(), avg_rating=mean(rating))
ggplot(city_rating, aes(x=n, y=avg_rating)) + 
  geom_point(aes(x=n, y=avg_rating)) + 
  stat_smooth(formula = y ~x+I(x^2), method="lm") +
  scale_y_continuous(name="Average Rating") +
  scale_x_continuous(name="Number of Restaurants in Location") +
  theme_classic()
ggsave("ratings_by_loc.png", width=3.5, height = 1.5, dpi=1000)
summary(lm(avg_rating ~ n + I(n^2), city_rating))

# uncommon restaurant names
string_dist <- stringdist::stringdistmatrix(unique(all_cities$name))
matt <- as.matrix(string_dist)
rownames(matt) <- unique(all_cities$name)
colnames(matt) <- unique(all_cities$name)

# Two most dissimilar restaurrants are
rowMax <- apply(matt, 1, max)
names(which.max(rowMax))
which.max(matt[which.max(rowMax), ])

# Overarll most uncommon
rs <- rowSums(matt)
names(which.max(rs))
# Top 3 most uncommon
head(names(sort(desc(rs))), 10)

# average rating distribution
all_cities %>% 
  count(rating) %>% 
  filter(rating > 0) %>% 
  ggplot(aes(x=as.character(rating), y=n)) + 
  geom_bar(stat="identity") +
  scale_x_discrete(name="Rating") +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) 
ggsave("distr.png", height = 1, units="in", width=3, dpi = 1000)

# n restaurants vs rating
ggplot(city_rating, aes(x=n, y=avg_rating)) + 
  geom_point(aes(x=n, y=avg_rating)) + 
  stat_smooth(formula = y ~x+I(x^2), method="lm") +
  scale_y_continuous(name="Average Rating") +
  scale_x_continuous(name="Number of Restaurants in Location") +
  theme_classic()

# highest rated words
toks %>%
  group_by(word) %>%
  summarize(n=n(),
            avg_word_rating=mean(rating),
            sd=sd(rating)) %>%
  filter(n >= 5) %>%
  arrange(desc(avg_word_rating), desc(n)) 

# lowest rated words
toks %>%
  group_by(word) %>%
  summarize(n=n(),
            avg_word_rating=mean(rating),
            sd=sd(rating)) %>%
  filter(n >= 5) %>%
  arrange((avg_word_rating), desc(n)) %>%
  filter(avg_word_rating>=2.6) 

# great wall rating
mean(all_cities[grepl("great wall", all_cities$name, ignore.case = T),]$rating)





