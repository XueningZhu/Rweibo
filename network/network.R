

setwd("E:/R learning/weibo/network")
library(Rweibo)

roauth <- createOAuth(app_name ="pudding", access_name = "rweibo")


get.friends<-function(id,name,depth=3)
{
  fr=list()
  uid=list(id)
  name=list(name)
  d_id=NULL
  d_name=NULL
  count=0
  for (i in 1:depth)
  {
    t_id=NULL
    t_name=NULL
    for (j in 1:length(uid[[i]]))
    {
      cat("i:",i,"j:",j,"\n")
      query=paste0("friendships.friends.bilateral(roauth, uid=",uid[[i]][j],",count=200)")
      bi_fr=eval(parse(text=query))[[1]]
      count=count+1
      if (count>140) 
      {
        Sys.sleep(60*60+10)
        count=0
      }
      t_id=rbind(t_id,cbind(uid[[i]][j],unlist(lapply(bi_fr,function(x) x$id))))
      t_name=rbind(t_name,cbind(name[[i]][j],unlist(lapply(bi_fr,function(x) x$name))))
      fr=c(fr,bi_fr)       
    }
    uid[[i+1]]=t_id[,2]
    name[[i+1]]=t_name[,2]
    d_id=rbind(d_id,t_id)
    d_name=rbind(d_name,t_name)
  }
  return(list(Uid=uid,Name=name,D_id=d_id,D_name=d_name,Fr=fr))
}
all_fr2=get.friends(id=2530951134,name="²¼¶¡Nnn",depth=2)
ID=apply(all_fr2$D_id,2,as.character)
NAME=all_fr2$D_name

options(stringsAsFactors = FALSE)
fri=data.frame(u0=NAME[,1],id0=ID[,1],u1=NAME[,2],id1=ID[,2]) ##?????why


fri$u0[which(fri$u0=="")]=fri$id0[which(fri$u0=="")]
fri$u1[which(fri$u1=="")]=fri$id1[which(fri$u1=="")]
Uid=unlist(all_fr2$Uid)
Name=unlist(all_fr2$Name)
Name[which(Name=="")]=Uid[which(Name=="")]


library(igraph)
people=data.frame(name=unique(Name))
gg=graph.data.frame(d=fri[,c(1,3)],directed=F,vertices=people)
gg=simplify(gg)
dg=degree(gg)
gg = subgraph(gg, which(dg > 0) - 1)
plot(gg, layout = layout.fruchterman.reingold, vertex.size = 5, vertex.label = NA,
     edge.color = grey(0.5), edge.arrow.mode = "-")
com = walktrap.community(gg, steps = 8)


## subgroup
V(gg)$sg = com$membership + 1
V(gg)$color = rainbow(max(V(gg)$sg))[V(gg)$sg]
par(mar = c(0, 0, 0, 0))
set.seed(14)
plot(gg, layout = layout.fruchterman.reingold, vertex.size = 5,
     vertex.color = V(gg)$color, vertex.label =NA, edge.color = grey(0.5),
     edge.arrow.mode = "-")

sort(membership(com)[all_fr2[[2]][[2]]])

page=page.rank(gg,directed=F)
sort(page[[1]][all_fr2[[2]][[2]]])

edge=data.frame(Source=D_id[,1],Target=D_id[,2])
write.csv(edge,file="edge.csv")

label=membership(com)[people$name]
names(label)=NULL
vertice=data.frame(Id=unique(Uid),Label=label)[1:300,]
write.csv(vertice,file="vertice.csv")


com_stat<-function(id,uid)
{
  com_fr=list()
  for (i in 1:length(uid))
  {
    show(i)
    query <- paste0("friendships.friends.in_common(roauth, uid= ",id,",suid=",uid[i],
                    ",count=100,page=1)")
    com_fr[[i]] <- eval(parse(text=query))[[1]]
  }
  return(com_fr)
}

com_fr=com_stat(id=2530951134,uid=all_fr2[[1]][[2]])
len_com=unlist(lapply(com_fr,length))

com_friends=sort(len_com,decreasing=T)
names(com_friends)=all_fr2[[2]][[2]][order(len_com,decreasing=T)]
com_friends
