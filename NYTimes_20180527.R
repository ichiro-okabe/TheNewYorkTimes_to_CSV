# ---
# title: "The New York Times to data frame"
# author: "Cristian Mejia"
# date: "June 1, 2017"
# Revised by Ichiro Okabe on 5/27'18.
# ---

knitr::opts_chunk$set(echo = TRUE)

# ## Objective
# Here, we collect news metadata from [The New York Times](https://www.nytimes.com/) API and save it as .csv file.
# Each row will be a news, and each column a piece of metadata provided by the article_search_v2 API.
# 
# For this example I will collect news *about* "robots" published in 2016.
# 
# ## Background
# You can check the [API documentation here](http://developer.nytimes.com/). Some of the fields we can obtain are the headline, 
# abstract and/or snippets, author, date of publication, URL, section, among others. 
# Notice that you cannot obtain the full news text, but in most of cases the provided abstract or 
# lead paragraph summarizes well the story.
# 
# ## API key
# 
# In order to run this code you have to obtain an [Article Search API key](http://developer.nytimes.com/signup).
# An API key is a long string of numbers and letters The New York Times API administrators provide you. 
# 
# Replace yours in the following
key <- "68e26e71cfac466cb66ad9ec81fdeb30" 

## Housekeeping
# I like to define the destination folder and file names at the beginning of my codes.

# set working directory
# setwd("D:/2018/05/NYTimes_to_CSV")
setwd("E:/OneDrive/2018_OneDrive/05/NYTimes_to_CSV")

# Write the name of the output file, always put .csv
file_name <- "robot_test.csv" 

## Libraries
# We can call the API manually and send requests to the dedicated URL. , others have created packages
# that interact with the API easily, so that we are going to use that. 
# 
# Here I am using the [rtimes](https://www.rdocumentation.org/packages/rtimes/versions/0.3.0) library, 
# however there are other packages that do something similar. 
# 
# I will also use the [plyr](https://cran.r-project.org/web/packages/plyr/plyr.pdf) library.

#install.packages("rtimes")
library(rtimes)
library(plyr) # dplyr is better?

## Crafting the query

# This is where we tell what we are looking for. If you are getting news for academic purposes better to design a good query, 
# and this is only possible by reading through the documentation of the API to know what we can ask for, and how to filter, 
# and the library documentation to know how to write it in code. But the short explanation is as follows:
# 
# **q** is the word(s) the news will contain in the headline or body.
# **fq** are filters.
# **begin_date** and **end_date** are self explanatory, the format is YYYYMMDD. If we do not write any the output will be from today backwards. The oldest possible date is: 18510918.
# **sort** from the oldest or newest
# 
# **fq** is the tricky one. I am searching news *about* robots, the technology. So that, news about *Westworld*,  *Mr.Robot* or *Terminator* are not necessary this time. To avoid them I used the **"subject.contains"** filter. Subject is a type of tags used by The New York Times different to the sections, and manually assigned to each article. [The list of subjects is here](http://www.nytimes.com/pages/topics/). I know that robotic technologies are labeled either as Robot, Technology or Artificial Intelligence, so I use that. 
# 
# If I wanted only specific sections of the newspaper I can also specify it in the **fq** as you can see in the following lines, but I let them unused.

# Query
q <- "robots"
# Filters
fq <- 'subject.contains: ("Robots*" "Technology" "Artificial Intelligence")'
                    # AND section_name:("World" "Opinion" "Blogs" "Public Editor" "The Public Editor" 
                    #"Times Topics" "Topics" "Front Page" "Business" "Food" "Health" 
                    #"Technology" "Science" "Open")'
begin_date <- "20170101"
end_date <- "20171231"
sort <- "oldest"

## Calling function
# By default the article search function  only gets the first *page* of results containing 10 news. 
# But we want them all. Then, simply create a function that iterates over all possible pages in our search.

search_by_page <- function(pag) { 
  as_search(key = key
            , q = q
            , fq = fq
            , begin_date = begin_date
            , end_date = end_date
            , sort = sort
            , page = pag
  )
}

### Test and number of pages ###
# In this section, we call the function one time to know how many news are, and thus, how many pages are.
# Some queries (words) are too general that we might find thousands of news having them. 
# However the API only allows you to get up to 100 pages (1000 news). If necessary we can circumvent that, 
# by adjusting the query. For example, instead of retrieving all year, change dates and get news for every six months. 
# Here we figure out how many news are that comply our query, 
# if the number surpass the 100 pages we print a message telling us to make adjustments.

output1 <- search_by_page(0)
pages <- as.numeric(ceiling(output1$meta["hits"]/10))
if (pages > 100) {sprintf("Too many news (%d pages), adjust the dates",pages)} else {sprintf("%d pages. OK to proceed",pages)}

### Get the data
# Once we sorted the limitation, we can collect the data.
articles <- lapply(0:(pages-1), function(x) {
  search <- search_by_page(x)
  data <- search$data # data has the tibble format ([number of articles] x 25 columns)
  # Sys.sleep(n): Sleep the system for n seconds. To avoid "Error: Too Many Requests (HTTP 429)"
  Sys.sleep(1)
  return(data)
  }
)

### Process data ###
# The following transform the API response to data frame.
# (Comment by IO) 
# The following line creates an error "Error: $ operator is invalid for atomic vectors"
# To extract contents of each factor, we need to describe articles[[i]]$[factor][[y]]
# Convert keyword list to text separated by " | ":
#   paste(articles[[2]]$keywords[[1]]$value, collapse=" | ")
table_keyword <- function(tbl){
  keywords <- paste(tbl$value, collapse = " | ")
  return(keywords)
}

extraction <- function(tib) {
  
  # Extract column data
  id_l <-                      tib$"_id"
  headline.main_l <-           tib$headline.main
  snippet_l <-                 tib$snippet
  web_url_l <-                 tib$web_url
  word_count_l <-              tib$word_count
  source_l <-                  tib$source
  keywords_l <-                sapply(tib$keywords,table_keyword)
  type_of_material_l <-        tib$type_of_material
  document_type_l <-           tib$document_type
  pub_date_l <-                tib$pub_date
  byline.original_l <-         tib$byline.original
  new_desk_l <-                tib$new_desk
  section_name_l <-            tib$section_name
  headline.print_headline_l <- tib$headline.print_headline

  x <- data.frame(id=id_l,headline.main=headline.main_l,web_url=web_url_l,snippet=snippet_l,
                  word_count=word_count_l,source=source_l,keywords=keywords_l,
                  type_of_material=type_of_material_l,document_type=document_type_l,pub_date=pub_date_l,
                  byline.original=byline.original_l,new_desk=new_desk_l,section_name=section_name_l,
                  headline.print_headline=headline.print_headline_l
                  )
  return(x)
}

### Make data frame ###
df <- data.frame(matrix(rep(NA,14), nrow=1))[numeric(0), ]
colnames(df) <- c("id","headline.main","web_url","snippet",
                  "word_count","source","keywords",
                  "type_of_material","document_type","pub_date",
                  "byline.original","new_desk","section_name",
                  "print_headline")
for (i in 1:pages) {
  df_tmp <- extraction(articles[[i]])
  df <- rbind(df,df_tmp)
}

### Write the file with .csv format
write.csv(df, file = file_name, row.names = F)
