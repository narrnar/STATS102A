#install.packages("rvest")
library(polite)
library(rvest)
library(stringr)


# Store web url
lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")

#Scrape the website for the movie rating
rating <- lego_movie %>% 
    html_nodes(".cMEQkK") %>%
    html_text() %>%
    as.numeric()
rating

# Scrape the website for the cast
cast <- lego_movie %>%
    html_nodes(".gCQkeh") %>%
    html_text() 
cast

# Extract the first review
review <- lego_movie %>%
    html_nodes(".ipc-overflowText--listCard.ipc-overflowText--long .ipc-html-content--base div") %>%
    html_text()
review

# extract just the project titles of an actor
chris_project <- read_html('https://www.imdb.com/name/nm0695435/?ref_=tt_cl_t_1') %>% 
  html_nodes('.ipc-metadata-list-summary-item__t') %>% html_text()
chris_project 

##############################################################################
### Automated Browser example
# The following code no longer works because imdb is protected from bot scraping
# See the updated code below
# 
# s <- session("http://www.imdb.com/title/tt1490017/")
# 
# # for one actor:
# chris_page <- s %>% session_follow_link("Chris Pratt") %>% read_html()
# 
# # create the empty list for data storage
# cast_movies <- list()
# 
# # run the loop that will go through the first three actors listed on the Lego Movie page
# 
# for(actor_name in cast[1:4]){
#     actorpage <- s %>% session_follow_link(actor_name) %>% read_html()
#     
#     # get the first 10 movies in the actor's filmography
#     cast_movies[[actor_name]]$movies <-  actorpage %>% 
#         html_nodes(".ipc-metadata-list-summary-item__t") %>% html_text() %>% head(10)
#     
#     # get the corresponding year values
#     cast_movies[[actor_name]]$years <- actorpage %>%
#         html_nodes(".ipc-metadata-list-summary-item__ctl .ipc-metadata-list-summary-item__li") %>% html_text() %>% 
#         head(10) %>% str_extract("[0-9]{4}")
#     
#     # insert the actor's name as a new column
#     cast_movies[[actor_name]]$name <- rep(actor_name, length(cast_movies[[actor_name]]$years))
# }
# 
# cast_movies
# 
# results <- list(movies = character(0),
#                 years = character(0),
#                 name = character(0))
# for (j in 1:4){
#   results$movies <- c(results$movies, cast_movies[[j]]$movies)
#   results$years <- c(results$years, cast_movies[[j]]$years)
#   results$name <- c(results$name, cast_movies[[j]]$name)
# }
# 
# results <- data.frame(results)
# print(results)
##############################################################################

## Code has been Updated to be more Polite

# Open a polite session with the server
session <- bow("http://www.imdb.com/title/tt1490017/") 

# Scrape the content of the page and store it in object page_content
page_content <- session %>% 
  scrape(content = "text/html; charset=UTF-8") 

# With object page_content, extract the desired information with html_nodes()
actor_urls <- page_content %>% 
  html_nodes(".gCQkeh") %>% 
  html_attr("href")
actor_urls

cast <- page_content %>% 
  html_nodes(".gCQkeh") %>% 
  html_text()
cast

# Name the urls with the actor names so we can refer to the url by name
names(actor_urls) <- cast
print(actor_urls)

# For example, we can get the url for one actor:
actor_urls["Chris Pratt"]

# To change the location of our session, we use "nod" and the desired url.
# Once we are at that page, we scrape the content and save it.
chris_page_content <- session %>% 
  nod(path = actor_urls["Chris Pratt"]) %>% 
  scrape(content = "text/html; charset=UTF-8")

# From Chris Pratt's page, we extract the list of projects
chris_page_content %>% 
  html_nodes('.ipc-metadata-list-summary-item__t') %>% 
  html_text()


# We now want to repeat this process for several actors:
# We begin by creating an empty list for data storage.
cast_movies <- list()


# I create a loop that will go through the first few actors in cast.
# The results are stored in a list of lists.
# The elements of the top-level list are actor_name, which themselves are lists.
# For each actor_name, there are three elements: movies, years, name

for(actor_name in cast[1:4]){
  actor_page_content <- session %>% 
    nod(path = actor_urls[actor_name]) %>% 
    scrape(content = "text/html; charset=UTF-8")
  
  # get the first 10 movies in the actor's filmography
  cast_movies[[actor_name]]$movies <- actor_page_content %>% 
    html_nodes(".ipc-metadata-list-summary-item__t") %>% 
    html_text() %>% head(10)
  
  # get the corresponding year values
  cast_movies[[actor_name]]$years <- actor_page_content %>%
    html_nodes(".ipc-metadata-list-summary-item__ctl .ipc-metadata-list-summary-item__li") %>% 
    html_text() %>% 
    head(10) %>% str_extract("[0-9]{4}")
  
  # insert the actor's name as a new column
  cast_movies[[actor_name]]$name <- rep(actor_name, length(cast_movies[[actor_name]]$years))
}

cast_movies

# I now create a new list with three elements: movies, year, name
results <- list(movies = character(0),
                years = character(0),
                name = character(0))

# I iterate over the elements of cast_movies and combine their content
for (j in seq_along(cast_movies)){
  results$movies <- c(results$movies, cast_movies[[j]]$movies)
  results$years  <- c(results$years , cast_movies[[j]]$years)
  results$name   <- c(results$name  , cast_movies[[j]]$name)
}

results
# The results can now be formatted as a data.frame
as.data.frame(results)
