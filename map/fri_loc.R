

setwd("E:/R_learning/visualization/weibo")
library(Rweibo)
#roauth <- createOAuth(app_name ="pudding", access_name = "rweibo")

registerApp(app_name = "PuddingNnn", "***", "***")
roauth <- createOAuth(app_name ="PuddingNnn", access_name = "rweibo")

my_fri <- friendships.friends(roauth, uid=2530951134,count = 200,
                              cursor = 0)
save(my_fri,file="my_fri.rda")
fri=my_fri[[1]]

info1=lapply(fri,function(x) c(x$name,x$location,x$followers_count))
info=do.call(rbind,info1)
loc=strsplit(info[,2]," ")
a=do.call(rbind,loc)
a[,1][a[,1]=="台湾"]="台"
a[,2][a[,2]=="台湾"]="台"
a[,2][a[,2]=="其他"]=a[,1][a[,2]=="其他"]

myfri=data.frame(name=info[,1],province=a[,1],city=a[,2],loc=apply(a,1,paste,collapse=" ")
                 ,follower=as.numeric(info[,3]))
myfri=myfri[which(myfri$province!="其他"&myfri$province!="海外"),]




get.loc<-function(loc)
{
  pro=grepl(loc[1],zh_posi$loc)
  cit=grepl(loc[2],zh_posi$loc)
  match=which(pro&cit)
  show(match)
  return(c(mean(zh_posi$lon[match]),mean(zh_posi$lat[match])))
}

b=apply(myfri[,2:3],1,get.loc)
myfri$lon=b[1,]
myfri$lat=b[2,]








