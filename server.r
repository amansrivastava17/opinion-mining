library(twitteR)
library(stringr)
library(ROAuth)
library(RCurl)
library(ggplot2)
library(reshape)
library(tm)
library(RJSONIO)
library(wordcloud)
library(gridExtra)
library(plyr)
library(shinyIncubator)
library(shiny)
library(reshape2)
library(rCharts)
library(dplyr)

consumer_key <- "gbn5RWX1tye3e9cNY0fuLApet"
consumer_secret <- "UKeGx3SDvfBSm7LYpJnYoCF1RGBqxBaG7SucYAE1kZdHkS9FBf"
access_token <- "3178424654-d8N0ml5WOGHVbmhC3bfDMWWRaeGeOdJmjVHwNcl"
access_secret <- "Z4VvhJz2jlFsDFe2pdsLamS6FNFmzH2GHSb0xk3RXVcsK"


Stopwords <- function(){
  wordlist <-file('stopwords.txt',open='r')
  stopwords <- readLines(wordlist)
  close(wordlist)
  stopwords
}
unwantedhinglish <-function(){
  wordlist <-file('befaltuhinglish.txt',open='r')
  hinglish <- readLines(wordlist)
  close(wordlist)
  hinglish
}

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)


# Function to create a data frame from tweets
shinyServer(function(input, output,session) {
  
  # Function to clean tweets, Stanton 2013
  CleanTweets<-function(tweets)
  {
    # Remove redundant spaces
    tweets <- str_replace_all(tweets," "," ")
    # Get rid of URLs
    tweets <- str_replace_all(tweets, "http://t.co/[a-z,A-Z,0-9]*{8}","")
    # Take out retweet header, there is only one
    tweets <- str_replace(tweets,"RT @[a-z,A-Z]*: ","")
    # Get rid of hashtags
    tweets <- str_replace_all(tweets,"#[a-z,A-Z]*","")
    # Get rid of references to other screennames
    tweets <- str_replace_all(tweets,"@[a-z,A-Z]*","")
    # Get rid of unused characters
    tweets <- gsub("[_.+]+"," ",tweets)
    return(tweets)
    
  }
  
  #Search tweets and create a data frame 
  TweetFrame<-function(searchTerm, maxTweets)
  {
    twtList<-searchTwitter(searchTerm,n=maxTweets,lang="en")
    twtList1<- do.call("rbind",lapply(twtList,as.data.frame))
    twtList1$text<-iconv(twtList1$text, 'UTF-8', 'ASCII') #WILL THIS SOLVE THE UTF ENCODING PROBLEM: http://lists.hexdump.org/pipermail/twitter-users-hexdump.org/2013-May/000335.html
    twtList1$hour <-as.numeric(format(twtList1$created,"%H"))
    twtList1$hour <-twtList1$hour
    twtList1<-subset(twtList1,!is.na(twtList1$text))
    return(twtList1)
  }
    
  # function for word cloud 
  
  wordcloudentity<-function(entitycleantext)
  {
    tweetCorpus<-Corpus(VectorSource(CleanTweets(entitycleantext)))
    #tweetCorpus2<- tm_map(tweetCorpus, stemDocument, language = "english",lazy=TRUE)  
    tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,
                                                          stopwords=c(Stopwords(),unwantedhinglish()),
                                                          removeNumbers=TRUE,tolower=TRUE))
    tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
    sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE) # calculate row sum of each term and sort in descending order (high freq to low)
    cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)#extracting names from named list in prev command and binding together into a dataframe with frequencies - called cloudFrame, names in separate columns
    wcloudentity<-wordcloud(cloudFrame$word,cloudFrame$freq,max.words=100, colors=brewer.pal(8,"Dark2"),scale=c(8,1), random.order=F)
    print(wcloudentity)
  }
  
  score.sentiment = function(sentences, pos.words, neg.words,hours)
  {
    
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array ("a") of scores back, so we use 
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
      
      # clean up sentences with R's regex-driven global substitute, gsub():
      sentence = gsub('[[:punct:]]', '', sentence)
      sentence = gsub('[[:cntrl:]]', '', sentence)
      sentence = gsub('\\d+', '', sentence)
      # and convert to lower case:
      sentence = tolower(sentence)
      # split into words. str_split is in the stringr package
      word.list = str_split(sentence, '\\s+')
      # sometimes a list() is one level of hierarchy too much
      words = unlist(word.list)
      
      
      # compare our words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      
      # match() returns the position of the matched term or NA
      # we just want a TRUE/FALSE:
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      
      # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
      score = sum(pos.matches) - sum(neg.matches)
      
      return(score)
    }, pos.words, neg.words)
    scores.df = data.frame(score=scores, text=sentences, size=seq(length(scores)),hour=hours)
    return(scores.df)
  }
  
  sentimentalanalysis<-function(entity,nooftweets){
   
    positivewords=readLines("positive_words.txt")
    negativewords=readLines("negative_words.txt")
    entityscore = score.sentiment(CleanTweets(entity$text),positivewords,negativewords,entity$hour)
    entityscore$posi = sum(laply(entityscore$score,function(score){
                              if(score>0){
                                return(1)
                              }
                              return(0)}))
    entityscore$negi = sum(laply(entityscore$score,function(score){
                            if(score<0){
                              return(1)
                              }
                            return(0)}))
    entityscore$neu = nooftweets-(entityscore$posi+entityscore$negi)
    
    entityscore
    
  }   

    

  
  entityone<-reactive({
    entityone<-TweetFrame(input$entity1, input$maxTweets)
    })
  
  entitytwo<-reactive({
    entitytwo<-TweetFrame(input$entity2, input$maxTweets)
   } )
   
  
  
  
  entity1score <-function(){
      entity1<-entityone()
      entity1<-subset(entity1,!is.na(entity1$text))
      scores <-sentimentalanalysis(entity1,nrow(entity1))
      scores
  }
  
  entity2score <-function(){
    entity2<- entitytwo()
    entity2<-subset(entity2,!is.na(entity2$text))
    scores <-sentimentalanalysis(entity2,nrow(entity2))
    scores
  }
  
  
  output$plot1<-renderChart({
    entity1<-entity1score()
    #print(entity1)
    #entity1$entity <-0
    entity2<-entity2score()
    #print(entity2)
    #entity2$entity <-1
    #bindHourScore <- rbind(entity1,entity2)
    #HourScore <-dcast(bindHourScore,hour+entity~'score',sum,value.var='score')
    HourScore1 <-dcast(entity1,hour~'score',sum,value.var='score')
    HourScore2 <-dcast(entity2,hour~'score',sum,value.var='score')
    #print(HourScore1)
    #print(HourScore2)
    a <- Highcharts$new()
    a$yAxis(title = list(text = "Score"))
    a$series(name="Handle 1",data =HourScore1$score, type = "line")
    a$series(name="Handle 2",data =HourScore2$score, type = "line")
    a$addParams(dom = "plot1")
    return(a)
    #p2 <- xPlot(score ~ hour, group = 'entity', type = 'line', data = HourScore)
    #p2
    
  })
  output$plot2<-renderChart({
    entity1<-entity1score()
    entity2<-entity2score()
    entity1$entity <-0
    entity2$entity <-1
    g3<-rbind(entity1,entity2)
    g1<-group_by(g3,hour,entity)
    g1<-summarise(g1,tweets_count=n())
    print(g1)
    a <- Highcharts$new()
    a$chart(type = "column")
    a$yAxis(title = list(text = "Tweets Count"))
    a$xAxis(categories=g1$hour)
    a$data(g1)
    a$addParams(dom = "plot2")
    return(a)
    
  })
  output$plot4<-renderChart({
    a<-entity2score()
    h1 <- Highcharts$new()
    h1$title(text='Handle 2!!!')
    h1$data(x=c('positive','negative','neutral'),y=c(a$posi[1],a$negi[1],a$neu[1]),type="pie",name="No of tweets")
    h1$addParams(dom = "plot4")
    return(h1)
    
  })
  output$entity1wc<-renderText({
    
    input$entity1})
  output$entity1wcplot<-renderPlot({
    wordcloudentity(entityone()$text)})
  
  output$entity2wc<-renderText({
    input$entity2})
  output$entity2wcplot<-renderPlot({
    wordcloudentity(entitytwo()$text)})
  
  output$plot3<-renderChart({
    a<-entity1score()
    h1 <- Highcharts$new()
    h1$title(text='Handle 1!!!')
    h1$data(x=c('positive','negative','neutral'),y=c(a$posi[1],a$negi[1],a$neu[1]),type="pie",name="No of tweets")
    h1$addParams(dom = "plot3")
    return(h1)
    
  })
  output$tableentity1 <- renderTable({
    tab<-entityone()[1]})
  
  #tab 4: Raw tweets of entity 2
  
  output$tableentity2<-renderTable({
    tab<-entitytwo()[1]})
})
