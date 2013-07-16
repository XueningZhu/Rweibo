setwd("E:/R_learning/weibo/topic")

library(Rweibo)
#registerApp(app_name = "pudding", "1477287520", "8514be56e2d530ce5612b0857adda960")
#listApp("pudding")
#roauth <- createOAuth(app_name ="pudding", access_name = "rweibo")
#res1 <- statuses.user_timeline(roauth,screen_name="²¼¶¡Nnn", count = 200,page=2)

registerApp(app_name = "PuddingNnn", "***", "***")

roauth <- createOAuth(app_name = "PuddingNnn", access_name = "rweibo")
fris <- friendships.friends(roauth, uid=2530951134,count = 200,
                            cursor = 0)
save(fris,file="fris.rda")
ids=sapply(fris[[1]],function(x) {return(x$id)})

res=list()
for (i in 1:length(ids))
{
  show(i)
  res[[i]] <- tryCatch({
    a=web.user_timeline(roauth, uid=ids[i], pages = 1:2)
  }, error = function(err) {
    # warning handler picks up where error was generated
    print(paste("MY_ERROR:  ",err))
    a=0
    return(a)
  }, finally = {
    print(dim(a))
  }) # END tryCatch
}

res[[i]] <- tryCatch({
  a=web.user_timeline(roauth, uid=ids[i], pages = 1:2)
}, error = function(err) {
  # warning handler picks up where error was generated
  print(paste("MY_ERROR:  ",err))
  a=0
  return(a)
  
}, finally = {
  
  print(dim(a))
  
}) # END tryCatch
save(res,file="res.rda")

######### change the version to 2.15.2

