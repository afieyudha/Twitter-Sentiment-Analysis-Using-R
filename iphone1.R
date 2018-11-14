library(stringr)
library(twitteR)
library(xlsx)
library(plyr)

api_key<- "UECOssH1hC4pvkyOaNvn0fxMN"
api_secret <- "HzLkXb00ahHSyyY9Rrktn8jv3WOkUe1oAp6sdPP0YZTmLJFIq3"
access_token <- "117336843-vYPgwn7UHEwgHWnNxuSj57fA6XtQC9sfwT3AhLv8"
access_token_secret <- "fAT8qpqI8paMe51mmD8SpepG4s6LWCnuhdefMbOaMuSzm"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

setwd('C:/Users/afiey_000/Documents')
neg = scan("kata_negatif.txt", what="character", comment.char=";")
pos = scan("kata_positif.txt", what="character", comment.char=";")

neg = c(neg, 'jelek')
pos = c(pos, 'bagus')

score.sentiment = function(tweets, pos.words, neg.words)
 
{
  
  require(plyr)
  require(stringr)
  
  scores = laply(tweets, function(tweet, pos.words, neg.words) {
  
  
  
  tweet = gsub('https://','',tweet) # mengahpus https://
  tweet = gsub('http://','',tweet) # menghapus http://
  tweet=gsub('[^[:graph:]]', ' ',tweet) ## menghapus karakter grafik 
  #seperti emot 
  tweet = gsub('[[:punct:]]', '', tweet) # menghapus tanda baca
  tweet = gsub('[[:cntrl:]]', '', tweet) # menghapus kontrol karakter
  tweet = gsub('\\d+', '', tweet) # menghapus nomor
  tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
  
  tweet = tolower(tweet) # membuat semua huruf menjadi kecil
  
  word.list = str_split(tweet, '\\s+') # membagi twit perkata ke dalam list
  
  words = unlist(word.list) # mengubah table menjadi vektor
  
  pos.matches = match(words, pos.words) ## pencocokan
  #nilai pada kata-kata ke dalam table 
  neg.matches = match(words, neg.words)
  
  pos.matches = !is.na(pos.matches) ## mengubah pencocokan nilai dari true/false
  neg.matches = !is.na(neg.matches)
  
  score = sum(pos.matches) - sum(neg.matches) # true/ false
  #diibaratkan sebagai 1 dan 0 sehinnga bisa ditambahkan nilainya
  
  return(score)
  
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  
  return(scores.df)
  
}

tweets = searchTwitter('iphone',lang="id",n=1000)
Tweets.text = laply(tweets,function(t)t$getText()) # ambil teks dari twit

analysis = score.sentiment(Tweets.text, pos, neg) # memanggil fungsi sentimen

hist(analysis$score)

write.xlsx(analysis, "iphone1.xlsx")
