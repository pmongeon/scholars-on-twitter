# ******************************************************************
# title: "Matching OpenAlex author IDs with Twitter accounts"
# author: "Philippe Mongeon"
# date: "`r Sys.Date()`"
# Summary: This code uses the APIs of OpenAlex, Crossref, and Twitter to fetch 
#          the potential Twitter accounts of an OpenAlex author. 
#          The only requirement to run this code is a bearer token for the Twitter API
# reference: Mongeon, P., Bowman, T. D., & Costas, R. (2022). *An open dataset of scholars on Twitter* (arXiv:2208.11065). arXiv. <https://doi.org/10.48550/arXiv.2208.11065>
# ******************************************************************

# Load packages

library(tidyverse)
library(stringi)
library(httr)
library(openalexR)
library(rtweet)

# Get author names and works for an OpenAlex author_id

author_id <- "https://openalex.org/A1923114979" # Insert your OpenAlex author ID here.
email <- "" # Insert your email here to provide it with your call to the API.

author_url <- str_replace(author_id,"https://openalex.org","https://api.openalex.org") # Creates the URL for the API call.
openalex_author_data <- openalexR::oaApiRequest(author_url, mailto = email)

# Author names ----

# OpenAlex has two name columns for each author ID: display_name, and diplay_name_alternatives. This code creates a tibble with one column combining the display_name and all display_name_alternatives for our author. In this example, there are no display_name_alternatives for the author, so only one row is returned.

# Get the author names
author_name <-openalex_author_data$display_name
display_name <- author_name # We will use this at the very end

# Get the alternative names
author_alternate_names <- openalex_author_data$display_name_alternatives

i=1
while (i <= length(author_alternate_names)) {
  author_name <- c(display_name, author_alternate_names[[i]])
  i=i+1
}

author <- tibble(author_id = author_id,
                 author_name = author_name)



# Get the author's works ----

works_url <-openalex_author_data$works_api_url # This retrieves the URL for an API call retrieving the list of works associated with the author ID.
works<- openalexR::oaApiRequest(works_url, mailto = email) # this gets the list of works using the works API url

# this creates a tibble with the DOIs of the works.
dois <- tibble()
for (i in 1:length(works)) {
  dois <- bind_rows(dois, works_data <- tibble(doi = works[[i]]$doi))
}


# Identify tweeters of the works from Crossref Event Data----

handles <- character()
for (d in 1:nrow(dois)) {
  doi<-dois[d,]$doi
  tweets_url<-str_c("https://api.eventdata.crossref.org/v1/events?mailto=pmongeon@dal.ca&obj-id=",dois[d,]$doi,"&source=twitter&rows=1000", sep = "")
  tweets <- GET(tweets_url)
  tweeters <- content(tweets, "parsed")$message$events
  
  t = 1
  while (t <= length(tweeters)) {
    handles <- c(handles,tweeters[[1]]$subj_id) %>% unique()
    handles <- c(handles,tweeters[[t]]$subj$`original-tweet-author`) %>% unique()
    t=t+1
  }
}

# This extracts the the twitter handle from the tweet url and stores the handles in a tibble
handles <- str_replace_all(handles, ".*.com/","")
handles <- str_replace_all(handles, ".*screen_name\\=","")
handles <- str_replace_all(handles, "/.*","")
handles <- tibble(handles = handles) %>%
  filter(handles != "twitter:") %>% 
  unique()



# Fetch tweeters' profile names from the Twitter API ----

auth_setup_default() # You might need to run this code to authenticate yoursef with Twitter
bearer <- "" # Your bearer token for the Twitter API
appname <- "" # Name of your app for the Twitter API

# Once authenticated, we can fetch the names using this code:

interval = 2 # set an interval for the API calls
users <- tibble()
h = 1
repeat {
  tryCatch(expr = {
    startTime = Sys.time()
    id <- rtweet::lookup_users(handles[h,]$handles)$id
    twt_url <- str_c("https://api.twitter.com/2/users/",id,"?user.fields=name",sep = "")
    twitter_profile <- GET(twt_url, add_headers(Authorization = paste("Bearer", bearer, sep = " ")))
    
    users <- bind_rows(users,tibble(tweeter_id = id,
                                    handle = handles[h,]$handles,
                                    profile_name = content(twitter_profile, "parsed")$data$name))
    
    sleepTime = startTime + interval - Sys.time()
    if (sleepTime > 0) {
      Sys.sleep(sleepTime)
    }
  },
  error = function(e){
    message(str_c(id, " not found on Twitter"))
    print(e)
  }
  )
  
  h <- h+1
  
  if (h > nrow(handles)) {
    break()
  }
} 

# create a tibble with a the handle and cleaned user name for all the Tweeters of all DOI in our list recorded in Crossref Event Data
users <- users %>% 
  unique() %>% 
  mutate(profile_name = iconv(profile_name,"latin1","ASCII", sub="")) %>% 
  mutate(profile_name = str_squish(profile_name))


# Name variants for matching ----

# NOTE: This next bit of code, which is certainly longer than it really needs to be, creates author name and tweeter name variants based on the tokens that compose the names. It also separates the variants in 5 columns: first name, last name, initials, first token, and first initials. These columns are used for the matching at the next step

users_token <- users %>%
  mutate(token = profile_name) %>% 
  separate_rows(token,
                sep = " ") %>%
  group_by(tweeter_id, profile_name) %>% 
  mutate(token_number = row_number()) %>% 
  mutate(tokens = max(token_number)) %>% 
  mutate(token = stri_trans_general(str = token, id = "Latin-ASCII")) %>% 
  mutate(token = str_replace_all(token, "\\."," ")) %>% 
  mutate(token = str_squish(str_to_lower(token))) %>% 
  ungroup %>% 
  pivot_wider(names_from = token_number,
              values_from = token)

users_name_variants <- tibble()
# 2 tokens
output <- users_token %>% 
  filter(tokens == 2) %>% 
  mutate(first_name = `1`,
         last_name = `2`,
         initials = str_sub(`1`,1,1),
         first_token = `1`,
         first_initial = str_sub(`1`,1,1)) %>% 
  select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial)

users_name_variants <- bind_rows(users_name_variants, output) %>% 
  unique()

# 3 tokens 
if (ncol(users_token) >= 7) {
  output <- bind_rows(users_token %>%
                        filter(tokens == 3) %>% 
                        mutate(first_name = str_c(`1`,`2`, sep = " ")) %>% 
                        mutate(last_name = str_c(`3`, sep = " ")) %>% 
                        mutate(initials = str_c(str_sub(`1`,1,1), str_sub(`2`,1,1), sep = "")) %>% 
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id,
                               profile_name,
                               first_name,
                               last_name,
                               initials,
                               first_initial),
                      users_token %>% 
                        filter(tokens == 3) %>%
                        mutate(first_name = str_c(`1`, sep = " ")) %>%
                        mutate(last_name = str_c(`2`,`3`, sep = " ")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id,
                               profile_name,
                               first_name,
                               last_name,
                               initials,
                               first_initial)
  )
  
  users_name_variants <- bind_rows(users_name_variants, output) %>% 
    unique()
  
  
}

# 4 tokens 
if(ncol(users_token)>=8) {
  output <- bind_rows(users_token %>%
                        filter(tokens == 4) %>% 
                        mutate(first_name = str_c(`1`,`2`,`3`, sep = " ")) %>% 
                        mutate(last_name = str_c(`4`, sep = " ")) %>% 
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                sep = "")) %>% 
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial),
                      users_token %>% 
                        filter(tokens == 4) %>%
                        mutate(first_name = str_c(`1`,`2`, sep = " ")) %>%
                        mutate(last_name = str_c(`3`,`4`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                str_sub(`2`,1,1), 
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial),
                      users_token %>% 
                        filter(tokens == 4) %>%
                        mutate(first_name = str_c(`1`, sep = " ")) %>%
                        mutate(last_name = str_c(`2`,`3`,`4`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial)
  )
  
  users_name_variants <- bind_rows(users_name_variants, output) %>% 
    unique()
  
}
# 5 tokens
if (ncol(users_token) >= 9) {
  output <- bind_rows(users_token %>%
                        filter(tokens == 5) %>% 
                        mutate(first_name = str_c(`1`,`2`,`3`,`4`, sep = " ")) %>% 
                        mutate(last_name = str_c(`5`, sep = " ")) %>% 
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                str_sub(`4`,1,1),
                                                sep = "")) %>% 
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial),
                      users_token %>% 
                        filter(tokens == 5) %>%
                        mutate(first_name = str_c(`1`,`2`,`3`, sep = " ")) %>%
                        mutate(last_name = str_c(`4`,`5`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial),
                      users_token %>% 
                        filter(tokens == 5) %>%
                        mutate(first_name = str_c(`1`,`2`, sep = " ")) %>%
                        mutate(last_name = str_c(`3`,`4`,`5`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                str_sub(`2`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial),
                      users_token %>% 
                        filter(tokens == 5) %>%
                        mutate(first_name = str_c(`1`, sep = " ")) %>%
                        mutate(last_name = str_c(`2`,`3`,`4`,`5`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial)
                      
  )
  
  users_name_variants <- bind_rows(users_name_variants, output) %>% 
    unique()
  
}

# 6 tokens
if (ncol(users_token) >= 10) {
  
  output <- bind_rows(users_token %>%
                        filter(tokens == 6) %>% 
                        mutate(first_name = str_c(`1`,`2`,`3`,`4`,`5`, sep = " ")) %>% 
                        mutate(last_name = str_c(`6`, sep = " ")) %>% 
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                str_sub(`4`,1,1),
                                                str_sub(`5`,1,1),
                                                sep = "")) %>% 
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial),
                      users_token %>% 
                        filter(tokens == 6) %>%
                        mutate(first_name = str_c(`1`,`2`,`3`,`4`, sep = " ")) %>%
                        mutate(last_name = str_c(`5`,`6`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                str_sub(`4`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial),
                      users_token %>% 
                        filter(tokens == 6) %>%
                        mutate(first_name = str_c(`1`,`2`,`3`, sep = " ")) %>%
                        mutate(last_name = str_c(`4`,`5`,`6`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial),
                      users_token %>% 
                        filter(tokens == 6) %>%
                        mutate(first_name = str_c(`1`, `2`, sep = " ")) %>%
                        mutate(last_name = str_c(`3`,`4`,`5`,`6`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                str_sub(`2`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial),
                      users_token %>% 
                        filter(tokens == 6) %>%
                        mutate(first_name = str_c(`1`, sep = " ")) %>%
                        mutate(last_name = str_c(`2`,`3`,`4`,`5`,`6`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial)
                      
  )
  users_name_variants <- bind_rows(users_name_variants, output) %>% 
    unique()
}
# 7 tokens
if (ncol(users_token) >= 11) {
  output <- bind_rows(users_token %>%
                        filter(tokens == 7) %>% 
                        mutate(first_name = str_c(`1`,`2`,`3`,`4`,`5`,`6`, sep = " ")) %>% 
                        mutate(last_name = str_c(`7`, sep = " ")) %>% 
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                str_sub(`4`,1,1),
                                                str_sub(`5`,1,1),
                                                str_sub(`6`,1,1),
                                                sep = "")) %>% 
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial),
                      users_token %>% 
                        filter(tokens == 7) %>%
                        mutate(first_name = str_c(`1`,`2`,`3`,`4`,`5`, sep = " ")) %>%
                        mutate(last_name = str_c(`6`,`7`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                str_sub(`4`,1,1),
                                                str_sub(`5`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial),
                      users_token %>% 
                        filter(tokens == 7) %>%
                        mutate(first_name = str_c(`1`,`2`,`3`,`4`, sep = " ")) %>%
                        mutate(last_name = str_c(`5`,`6`,`7`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                str_sub(`4`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial),
                      users_token %>% 
                        filter(tokens == 7) %>%
                        mutate(first_name = str_c(`1`, `2`, `3`, sep = " ")) %>%
                        mutate(last_name = str_c(`4`,`5`,`6`,`7`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial),
                      users_token %>% 
                        filter(tokens == 7) %>%
                        mutate(first_name = str_c(`1`,`2`, sep = " ")) %>%
                        mutate(last_name = str_c(`3`,`4`,`5`,`6`,`7`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                str_sub(`2`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial),
                      users_token %>% 
                        filter(tokens == 7) %>%
                        mutate(first_name = str_c(`1`, sep = " ")) %>%
                        mutate(last_name = str_c(`2`,`3`,`4`,`5`,`6`,`7`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(tweeter_id, handle, profile_name, first_name, last_name, initials, first_token, first_initial)
  )
  
  users_name_variants <- bind_rows(users_name_variants, output) %>% 
    unique()
  
  
}


author_token <- author %>%
  mutate(token = author_name) %>% 
  separate_rows(token,
                sep = " ") %>%
  group_by(author_id, author_name) %>% 
  mutate(token_number = row_number()) %>% 
  mutate(tokens = max(token_number)) %>% 
  mutate(token = stri_trans_general(str = token, id = "Latin-ASCII")) %>% 
  mutate(token = str_replace_all(token, "\\."," ")) %>% 
  mutate(token = str_squish(str_to_lower(token))) %>% 
  ungroup %>% 
  pivot_wider(names_from = token_number,
              values_from = token)

author_name_variants <- tibble()
# 2 tokens
output <- author_token %>% 
  filter(tokens == 2) %>% 
  mutate(first_name = `1`,
         last_name = `2`,
         initials = str_sub(`1`,1,1),
         first_token = `1`,
         first_initial = str_sub(`1`,1,1)) %>% 
  select(author_id, author_name, first_name, last_name, initials, first_token, first_initial)

author_name_variants <- bind_rows(author_name_variants, output) %>% 
  unique()

# 3 tokens 
if (ncol(author_token) >= 6) {
  output <- bind_rows(author_token %>%
                        filter(tokens == 3) %>% 
                        mutate(first_name = str_c(`1`,`2`, sep = " ")) %>% 
                        mutate(last_name = str_c(`3`, sep = " ")) %>% 
                        mutate(initials = str_c(str_sub(`1`,1,1), str_sub(`2`,1,1), sep = "")) %>% 
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id,
                               author_name,
                               first_name,
                               last_name,
                               initials,
                               first_initial),
                      author_token %>% 
                        filter(tokens == 3) %>%
                        mutate(first_name = str_c(`1`, sep = " ")) %>%
                        mutate(last_name = str_c(`2`,`3`, sep = " ")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id,
                               author_name,
                               first_name,
                               last_name,
                               initials,
                               first_initial)
  )
  
  author_name_variants <- bind_rows(author_name_variants, output) %>% 
    unique()
  
  
}

# 4 tokens 
if(ncol(author_token)>=7) {
  output <- bind_rows(author_token %>%
                        filter(tokens == 4) %>% 
                        mutate(first_name = str_c(`1`,`2`,`3`, sep = " ")) %>% 
                        mutate(last_name = str_c(`4`, sep = " ")) %>% 
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                sep = "")) %>% 
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial),
                      author_token %>% 
                        filter(tokens == 4) %>%
                        mutate(first_name = str_c(`1`,`2`, sep = " ")) %>%
                        mutate(last_name = str_c(`3`,`4`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                str_sub(`2`,1,1), 
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial),
                      author_token %>% 
                        filter(tokens == 4) %>%
                        mutate(first_name = str_c(`1`, sep = " ")) %>%
                        mutate(last_name = str_c(`2`,`3`,`4`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial)
  )
  
  author_name_variants <- bind_rows(author_name_variants, output) %>% 
    unique()
  
}
# 5 tokens
if (ncol(author_token) >= 8) {
  output <- bind_rows(author_token %>%
                        filter(tokens == 5) %>% 
                        mutate(first_name = str_c(`1`,`2`,`3`,`4`, sep = " ")) %>% 
                        mutate(last_name = str_c(`5`, sep = " ")) %>% 
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                str_sub(`4`,1,1),
                                                sep = "")) %>% 
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial),
                      author_token %>% 
                        filter(tokens == 5) %>%
                        mutate(first_name = str_c(`1`,`2`,`3`, sep = " ")) %>%
                        mutate(last_name = str_c(`4`,`5`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial),
                      author_token %>% 
                        filter(tokens == 5) %>%
                        mutate(first_name = str_c(`1`,`2`, sep = " ")) %>%
                        mutate(last_name = str_c(`3`,`4`,`5`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                str_sub(`2`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial),
                      author_token %>% 
                        filter(tokens == 5) %>%
                        mutate(first_name = str_c(`1`, sep = " ")) %>%
                        mutate(last_name = str_c(`2`,`3`,`4`,`5`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial)
                      
  )
  
  author_name_variants <- bind_rows(author_name_variants, output) %>% 
    unique()
  
}

# 6 tokens
if (ncol(author_token) >= 9) {
  
  output <- bind_rows(author_token %>%
                        filter(tokens == 6) %>% 
                        mutate(first_name = str_c(`1`,`2`,`3`,`4`,`5`, sep = " ")) %>% 
                        mutate(last_name = str_c(`6`, sep = " ")) %>% 
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                str_sub(`4`,1,1),
                                                str_sub(`5`,1,1),
                                                sep = "")) %>% 
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial),
                      author_token %>% 
                        filter(tokens == 6) %>%
                        mutate(first_name = str_c(`1`,`2`,`3`,`4`, sep = " ")) %>%
                        mutate(last_name = str_c(`5`,`6`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                str_sub(`4`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial),
                      author_token %>% 
                        filter(tokens == 6) %>%
                        mutate(first_name = str_c(`1`,`2`,`3`, sep = " ")) %>%
                        mutate(last_name = str_c(`4`,`5`,`6`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial),
                      author_token %>% 
                        filter(tokens == 6) %>%
                        mutate(first_name = str_c(`1`, `2`, sep = " ")) %>%
                        mutate(last_name = str_c(`3`,`4`,`5`,`6`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                str_sub(`2`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial),
                      author_token %>% 
                        filter(tokens == 6) %>%
                        mutate(first_name = str_c(`1`, sep = " ")) %>%
                        mutate(last_name = str_c(`2`,`3`,`4`,`5`,`6`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial)
                      
  )
  author_name_variants <- bind_rows(author_name_variants, output) %>% 
    unique()
}
# 7 tokens
if (ncol(author_token) >= 10) {
  output <- bind_rows(author_token %>%
                        filter(tokens == 7) %>% 
                        mutate(first_name = str_c(`1`,`2`,`3`,`4`,`5`,`6`, sep = " ")) %>% 
                        mutate(last_name = str_c(`7`, sep = " ")) %>% 
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                str_sub(`4`,1,1),
                                                str_sub(`5`,1,1),
                                                str_sub(`6`,1,1),
                                                sep = "")) %>% 
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial),
                      author_token %>% 
                        filter(tokens == 7) %>%
                        mutate(first_name = str_c(`1`,`2`,`3`,`4`,`5`, sep = " ")) %>%
                        mutate(last_name = str_c(`6`,`7`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1), 
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                str_sub(`4`,1,1),
                                                str_sub(`5`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial),
                      author_token %>% 
                        filter(tokens == 7) %>%
                        mutate(first_name = str_c(`1`,`2`,`3`,`4`, sep = " ")) %>%
                        mutate(last_name = str_c(`5`,`6`,`7`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                str_sub(`4`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial),
                      author_token %>% 
                        filter(tokens == 7) %>%
                        mutate(first_name = str_c(`1`, `2`, `3`, sep = " ")) %>%
                        mutate(last_name = str_c(`4`,`5`,`6`,`7`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                str_sub(`2`,1,1),
                                                str_sub(`3`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial),
                      author_token %>% 
                        filter(tokens == 7) %>%
                        mutate(first_name = str_c(`1`,`2`, sep = " ")) %>%
                        mutate(last_name = str_c(`3`,`4`,`5`,`6`,`7`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                str_sub(`2`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial),
                      author_token %>% 
                        filter(tokens == 7) %>%
                        mutate(first_name = str_c(`1`, sep = " ")) %>%
                        mutate(last_name = str_c(`2`,`3`,`4`,`5`,`6`,`7`, sep = " ")) %>%
                        mutate(initials = str_c(str_sub(`1`,1,1),
                                                sep = "")) %>%
                        mutate(first_token = `1`) %>% 
                        mutate(first_initial = str_sub(`1`,1,1)) %>% 
                        select(author_id, author_name, first_name, last_name, initials, first_token, first_initial)
  )
  
  author_name_variants <- bind_rows(author_name_variants, output) %>% 
    unique()%>% 
    unnest(author_name)
}


# Matching the author and the tweeter of the works ----

users_name_variants <- users_name_variants %>% 
  mutate(handle_clean = str_to_lower(str_remove_all(handle,"[^[:alpha:]]")))
author_name_variants <- author_name_variants %>% 
  mutate(display_name = display_name)

matches <- bind_rows(
  author_name_variants %>% 
    inner_join(users_name_variants, by=c("first_name","last_name")) %>% 
    mutate(criteria = "full name (profile name)") %>% 
    unique(),
  author_name_variants %>% 
    crossing(users_name_variants, .name_repair="universal") %>% 
    filter(str_detect(str_remove_all(str_to_lower(profile_name)," "), str_c(first_name...3, last_name...4))) %>% 
    mutate(criteria = "Substring (profile name)") %>% 
    unique(),
  author_name_variants %>% 
    inner_join(users_name_variants, by=c("first_token","last_name")) %>% 
    mutate(criteria = "Last name + first token (profile name)") %>% 
    unique(),
  author_name_variants %>% 
    inner_join(users_name_variants, by=c("initials","last_name")) %>% 
    select(author_id, display_name, tweeter_id, handle, profile_name) %>% 
    mutate(criteria = "Last name + initials (profile name)") %>% 
    unique(),
  author_name_variants %>% 
    inner_join(users_name_variants, by=c("first_initial","last_name")) %>% 
    mutate(criteria = "Last name + first initial (profile name)") %>% 
    unique(),
  author_name_variants %>% 
    mutate(handle_clean = str_c(first_name,last_name)) %>% 
    inner_join(users_name_variants, by="handle_clean") %>% 
    mutate(criteria = "Full name (handle)") %>% 
    unique(),
  author_name_variants %>% 
    mutate(handle_clean = str_c(first_token,last_name)) %>% 
    inner_join(users_name_variants, by="handle_clean") %>% 
    mutate(criteria = "Last name + first token (handle)") %>% 
    unique(),
  author_name_variants %>% 
    mutate(handle_clean = str_c(initials,last_name)) %>% 
    inner_join(users_name_variants, by="handle_clean") %>% 
    mutate(criteria = "Last name + initials (handle)") %>% 
    unique(),
  author_name_variants %>% 
    mutate(handle_clean = str_c(first_initial,last_name)) %>% 
    inner_join(users_name_variants, by="handle_clean") %>% 
    mutate(criteria = "Last name + first initial (handle)") %>% 
    unique()) %>% 
  group_by(author_id, tweeter_id, handle, profile_name) %>% 
  mutate(criteria = paste(criteria, collapse=", ")) %>%
  select(author_id, display_name, tweeter_id, handle, profile_name, criteria) %>% 
  unique()

  
