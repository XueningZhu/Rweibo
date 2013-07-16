setwd("E:/R_learning/weibo")

library(Rweibo)
registerApp(app_name = "PuddingNnn", "3991415724", "0403235fda063e792087c103515c1f32")
listApp("PuddingNnn")

roauth <- createOAuth(app_name = "PuddingNnn", access_name = "rweibo")
roauth

#roauth <- createOAuth(app_name = "PuddingNnn", access_name = "rweibo",
#                     login = TRUE, username = "zxn529@163.com", password = "zhuxuening")

## please set the login protection options first here: 
## http://account.weibo.com/settings/security/protect

roauth$login(username = "yonghuming", password = "password")

## check the roauth information
roauth

## check the limits for the roauth 
## http://open.weibo.com/wiki/Rate-limiting
roauth$getLimits(TRUE)

############### 1 ###############
## for retreving weibo information


## check the public weibo
res1 <- statuses.public_timeline(roauth, count = 1)

## check the following weibo (including oneself)
res2 <- statuses.friends_timeline(roauth, count = 5)

## check bilaterally following weibo
res3 <- statuses.bilateral_timeline(roauth, count = 5)

## check some user's weibo; uid also supported
res4 <- statuses.user_timeline(roauth, screen_name = "布丁Nnn", count = 1)
#res4 <- statuses.user_timeline(roauth, uid = 2530951134, count = 1)

## check the comments received by the user
res5 <- comments.to_me(roauth, count = 5)

## check the weibo which @ me
res6 <- statuses.mentions(roauth, count = 5)

## check the comments which @ me
res7 <- comments.mentions(roauth, count = 5)

############### 2 ###############
## for post status
## please check the doc by lijian for details

############### 3 ###############
## for searching
## the data frame is returned
## also support for some specific characters like " ~和或……" 
## parsing the web by XML due to the limit on OAuth
res11 <- web.search.content("统计", page = 2, combinewith = NULL)

## for more searching results
res12 <- web.search.content("R语言", page = 2, combinewith = res11)

## weibo after since some time
res14 <- web.search.content("Rweibo", since = "2012-10-01")

## for crawling other's infomation (updated after 2013.7.2)
u17 <- web.search.user("lijian001")
res17 <- web.user_timeline(roauth, uid = u17$uid, pages = 1:2)
res17[1,]
names(res17)

############### 3 ###############
## public api: http://open.weibo.com/wiki/%E5%BE%AE%E5%8D%9AAPI
res18 <- weibo.api(roauth,
                   URL = "https://api.weibo.com/2/statuses/public_timeline.json",
                   paramlist = list(count = 5, page = 1),
                   httpmethod = "GET")

############### 4 ###############
## for some analysis demands
## repost / comment / someone's all weibos
## dataframe is returned


ana1 <- analysis.getReposts(roauth, mid = "3598132098711063")
names(ana1)

ana2 <- analysis.getComments(roauth, mid = "3598132098711063")
names(ana2)

ana3 <- analysis.getUserTimeline(roauth,screen_name ="布丁Nnn")
names(ana3)



