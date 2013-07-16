
setwd("E:/R_learning/weibo")

library(Rweibo)
#registerApp(app_name = "pudding", "1477287520", "8514be56e2d530ce5612b0857adda960")
#listApp("pudding")
#roauth <- createOAuth(app_name ="pudding", access_name = "rweibo")
#res1 <- statuses.user_timeline(roauth,screen_name="布丁Nnn", count = 200,page=2)

registerApp(app_name = "PuddingNnn", "***", "***")

roauth <- createOAuth(app_name = "PuddingNnn", access_name = "rweibo")
res1 <- analysis.getUserTimeline(roauth,screen_name ="布丁Nnn",count=200)
#res2 <- statuses.user_timeline(roauth,uid=1635976784, count = 200)

#text=unlist(lapply(res1,function(xl) xl$text))
text=res1$text

#retweet_text=unlist(lapply(res1,function(xl) xl$retweeted_status$text))
retweet_text=res1$retweeted_text

text_split=unlist(lapply(text,strsplit,split="//@",perl=T))
ll=lapply(text_split,gregexpr,pattern=":",perl=T)
omit=unlist(lapply(ll,function(xl) xl[[1]][1]))
ind=which(omit>0&omit<=8)
tmp=text_split[ind]
text_split[ind]=substr(tmp,omit[ind]+1,max(nchar(tmp))+1)
text_split=text_split[text_split!=""]
save(text_split,file="text_split.rda")
save(retweet_text,file="retweet_text.rda")

##### change the defaulted version R 2.15.2
setwd("E:/R_learning/weibo")
stopwords=readLines("CH_stopwords.txt")
load("text_split.rda")
load("retweet_text.rda")



## Rwordseg : http://jliblog.com/app/rwordseg
library(Rwordseg)
importSogouScel("machine_learning.scel")

installDict("E:/R_learning/weibo/machine_learning.scel", dictname = "machine_learning.scel",
            dicttype = c("scel"))
installDict("E:/R_learning/weibo/stat.scel", dictname = "stat.scel",
            dicttype = c("scel"))
installDict("E:/R_learning/weibo/math.scel", dictname = "math.scel",
            dicttype = c("scel"))

retweet_text=retweet_text[!is.na(retweet_text)]
alltext=c(text_split,retweet_text)
res=unlist(segmentCN(alltext))
res=res[res!=""]
#res=res[grepl("[0-9A-Za-z]",res,perl=T)==F]
res=res[nchar(res)>1]
word_f=table(res)
word=names(word_f)

stopwords=c(stopwords,"http","cn","www","转发")
word=setdiff(word,stopwords)
a=grepl("[A-Za-z]",word,perl=T)&(nchar(word)<=3)|grepl("[0-9]",word,perl=T)
word=word[!a]
word_f=word_f[word]



library(wordcloud)
wordcloud(word, word_f, random.order=FALSE, ,min.freq=2,colors=brewer.pal(8, "Dark2"))






