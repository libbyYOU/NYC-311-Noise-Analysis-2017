library(rvest)
library(ggplot2)
library(lubridate)


page1="http://www.nfl.com/stats/categorystats?archive=true&conference=null&statisticCategory=PASSING&season=2014&seasonType=PRE&experience=&tabSeq=0&qualified=false&Submit=Go"

p1 = page1 %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="result"]') %>%
  html_table()

## how to get html code and xpath?
## right click the table, and choose 'inspect', and then choose the line highlighting the whole table
## copy and choose the xpath

p1 = p1[[1]]
## ?sometimes this returns a list, and we have to identify the one we want

page2="http://www.nfl.com/stats/categorystats?tabSeq=0&season=2014&seasonType=PRE&Submit=Go&experience=&archive=true&d-447263-p=2&statisticCategory=PASSING&conference=null&qualified=false"
p2 = page2 %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="result"]') %>%
  html_table()

p2 = p2[[1]]


page3="http://www.nfl.com/stats/categorystats?tabSeq=0&season=2014&seasonType=PRE&experience=&Submit=Go&archive=true&conference=null&statisticCategory=PASSING&d-447263-p=3&qualified=false"
p3=page3 %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="result"]') %>%
  html_table()
p3 = p3[[1]]




### combine 3 tables together
  
data=rbind(p1, p2, p3)

## create a dotplot to show the top 10 players using the rate variable
library(dplyr)
top10 = data %>%
  arrange(-Rate) %>%
  slice(1:10)
  
ggplot(top10,aes(x=reorder(Player,-Rate),
                 y=Rate)) +
  geom_point()



### list of countries and dependencies by population
url="https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
pop = url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table()
pop=pop[[1]]

## p4$Population=as.numeric(p4$Population)
## need to get rid of the comma

library(stringr)
pop$Population = str_replace_all(string = pop$Population,
                pattern = ",",
                replacement = "")

## change the date into a readable date by R

pop$Date = pop$Date


top10 = p4 %>%
  arrange(-Population) %>%
  slice(1:10)

### twitter
library(twitteR)
# set up twitter authentication

api_key="MhD2UCYP36a28pIdrqXu3N60h"
api_secret="AhTQnfdop1OkiuYB0gfb9XBMaeUFMsX12UPXVzKgGdL1aBxyzT"
access_token="981344967502131200-0Hty5AV1uLNq8vyrJ764YunHnGvE4or"
access_token_secret="w12P4B9G3Iq2aJAPnN7As5Eg4n4j2ekkmo1Dpasr16UPD"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

### search for twitts about USC
uscTweets = searchTwitter("USC", n =100)
uscDF = twListToDF(uscTweets)


### what is USC Marshall is tweeting about?
library(tm) ## text mining
library(stringr)
library(wordcloud)
library(ggplot2)
library(dplyr)

user = getUser("uscmarshall")
usc = userTimeline(user,n=1000)
# but we only get 48, sometimes we could not get the exact number of what we want

tweets = twListToDF(usc)


## clean the data and visualize them using a wordcloud
nohandles = str_replace_all(tweets,
                            pattern = "@\\w+",
                            replacement = "")

wordCorpus = Corpus(VectorSource(nohandles)) %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords,stopwords("english")) %>%
  tm_map(stripWhitespace)
set.seed(100)
wordcloud(words = wordCorpus,
          random.order = FALSE,
          col="red")



## sentiment analysis for USCmarshall tweets
library(syuzhet)

uscSentiment = get_nrc_sentiment(tweets$text)

data = apply(uscSentiment,
             MARGIN = 2,
             FUN = sum)

data = data.frame(count = data)

data$emotion = rownames(data)

ggplot(data,
       aes(y=count,
           x=reorder(emotion,count))) +
  geom_col()








