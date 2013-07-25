
library(Rweibo)

registerApp(app_name = "PuddingNnn", "**", "***")

roauth <- createOAuth(app_name = "PuddingNnn", access_name = "rweibo")
roauth$login(username = "****", password = "****")

library(XML)
# get data from web
webpage <-'http://data.weibo.com/top/influence/famous?class=29&type=day'
tables <- readHTMLTable(webpage,stringsAsFactors = FALSE)
sports=tables[[1]][,c(1,2,3,6)]
names(sports)=c("rank","name","influence","description")

webpage <-'http://data.weibo.com/top/influence/famous?class=13&type=day'
tables <- readHTMLTable(webpage,stringsAsFactors = FALSE)
entertain=tables[[1]][,c(1,2,3,6)]
names(entertain)=c("rank","name","influence","description")

webpage <-'http://data.weibo.com/top/influence/famous?class=52&type=day'
tables <- readHTMLTable(webpage,stringsAsFactors = FALSE)
economic=tables[[1]][,c(1,2,3,6)]
names(economic)=c("rank","name","influence","description")

webpage <-'http://data.weibo.com/top/influence/famous?class=44&type=day'
tables <- readHTMLTable(webpage,stringsAsFactors = FALSE)
media=tables[[1]][,c(1,2,3,6)]
names(media)=c("rank","name","influence","description")

webpage <-'http://data.weibo.com/top/influence/famous?class=66&type=day'
tables <- readHTMLTable(webpage,stringsAsFactors = FALSE)
literature=tables[[1]][,c(1,2,3,6)]
names(literature)=c("rank","name","influence","description")

webpage <-'http://data.weibo.com/top/influence/famous?class=1074&type=day'
tables <- readHTMLTable(webpage,stringsAsFactors = FALSE)
fashion=tables[[1]][,c(1,2,3,6)]
names(fashion)=c("rank","name","influence","description")

webpage <-'http://data.weibo.com/top/influence/famous?class=60&type=day'
tables <- readHTMLTable(webpage,stringsAsFactors = FALSE)
IT=tables[[1]][,c(1,2,3,6)]
names(IT)=c("rank","name","influence","description")

webpage <-'http://data.weibo.com/top/influence/famous?class=1855&type=day'
tables <- readHTMLTable(webpage,stringsAsFactors = FALSE)
education=tables[[1]][,c(1,2,3,6)]
names(education)=c("rank","name","influence","description")

setwd("F:/weibo/data/719")
save(sports,file="sports.rda")
save(entertain,file="entertain.rda")
save(economic,file="economic.rda")
save(media,file="media.rda")
save(literature,file="literature.rda")
save(fashion,file="fashion.rda")
save(IT,file="IT.rda")
save(education,file="education.rda")


name=c(sports$name,entertain$name,economic$name,media$name,
       literature$name,fashion$name,IT$name,education$name)




res_719=list()
for (i in 1:200)
{
  show(i)
  res_719[[i]] <- tryCatch({
    user=web.search.user(name[i])
    a=web.user_timeline(roauth, uid=user$uid, pages = 1:3)
  }, error = function(err) {
    # warning handler picks up where error was generated
    print(paste("MY_ERROR:  ",err))
    user=0
    a=0
    return(a)
  }, finally = {
    print(dim(a))
  }) # END tryCatch
}

sapply(res_719,function(x) {return(is.null(dim(x)))})

