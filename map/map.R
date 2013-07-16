library(XML)

# get data from web
webpage <-'http://blog.csdn.net/svrsimon/article/details/8255051'
tables <- readHTMLTable(webpage,stringsAsFactors = FALSE)
raw <- tables[[1]]
zh_posi <- raw[-1,]
colnames(zh_posi)=c("province","city","county","lon","lat")
save(zh_posi,file="zh_posi.rda")
zh_posi$loc=apply(zh_posi[,1:3],1,paste,collapse=" ")
zh_posi[,4:5]=apply(zh_posi[,4:5],2,as.numeric)

library(sqldf)
myfri2=sqldf("select province,city, avg(lon) as m_lon,avg(lat) as m_lat, avg(follower) as m_fol from myfri group by province,city")
Encoding(myfri2$province)="UTF-8"
Encoding(myfri2$city)="UTF-8"


library(ggmap)
library(sna)
library(Hmisc)
#draw the map
setwd("E:/R_learning/visualization/weibo")
load("E:/R_learning/visualization/weibo/zh_posi.rda")
load("E:/R_learning/visualization/weibo/myfri.rda")
load("E:/R_learning/visualization/weibo/allEdges.rda")

edgeMaker <- function(whichRow, len = 1, curved = TRUE){
  fromC <- c(113.27, 23.13)  # Origin
  toC <- c(myfri2[whichRow,3],myfri2[whichRow,4]) # Terminus
  weight <- myfri2[whichRow, 5]  # Terminus
  
  # Add curve:
  graphCenter <- c(mean(myfri2$m_lon),mean(myfri2$m_lat))#colMeans(myfri[,1:2])  # Center of the overall graph
  bezierMid <- c(fromC[1], toC[2])  # A midpoint, for bended edges
  distance1 <- sum((graphCenter - bezierMid)^2)
  if(distance1 < sum((graphCenter - c(toC[1], fromC[2]))^2)){
    bezierMid <- c(toC[1], fromC[2])
  }  # To select the best Bezier midpoint
  bezierMid <- (fromC + toC + bezierMid) / 3  # Moderate the Bezier midpoint
  if(curved == FALSE){bezierMid <- (fromC + toC) / 2}  # Remove the curve
  
  edge <- data.frame(bezier(c(fromC[1], bezierMid[1], toC[1]),  # Generate
                            c(fromC[2], bezierMid[2], toC[2]),  # X & y
                            evaluation = len))  # Bezier path coordinates
  edge$Sequence <- 1:len  # For size and colour weighting in plot
  edge$weight <- weight
  edge$Group <- whichRow
  return(edge)
}
allEdges <- lapply(1:nrow(myfri2), edgeMaker, len = 100, curved = TRUE)
allEdges <- do.call(rbind, allEdges)  # a fine-grained path ^, with bend ^

#a=edgeMaker(1, len = 100)


china=get_map(location = c(lon = mean(myfri2$m_lon), lat = mean(myfri2$m_lat)), zoom=5,maptype= "roadmap")
p1=ggmap(china,extent='device',darken=0.2)

drawit<-function(i){
 p=p1+geom_path(data=allEdges[1:i,], aes(x = x, y = y,group = Group,  # Edges with gradient
                                        size=log(weight+1),color=Sequence),alpha=0.6,show_guide=F)+  # and taper
   scale_colour_gradient(low = "red3", high = "white", guide = "none")
 if (i>=100)
 {
   p=p+geom_point(data=myfri2[1:floor(i/100),],aes(x=m_lon,y=m_lat,size=log(m_fol+1)*1.3),alpha=0.5,show_guide=F,colour = "black") + 
     geom_point(data=myfri2[1:floor(i/100),],aes(x=m_lon,y=m_lat,size=(log(m_fol+1))),alpha=0.6,show_guide=F,colour="red3")
 }
 return(p)
}


print(drawit(3800))

library(animation)
saveMovie({
  ani.options(interval=.1,
              convert = shQuote('C:/Program Files/ImageMagick-6.8.5-Q16/convert.exe'))
  for( i in seq(50,3000,50)) print(drawit(i))
})

# can't run, don't know why¡­¡­


draw<-function(ll){
for( i in ll) print(drawit(i))}

oopts = ani.options(ffmpeg = "C:/Program Files/ImageMagick-6.8.5-Q16/ffmpeg.exe")
#oopts = ani.options(ffmpeg = "E:/R_learning/ffmpeg/bin/ffmpeg.exe")
if (.Platform$OS.type != "windows") ani.options(ffmpeg = "ffmpeg")
saveVideo({
  draw(seq(25,3000,25))
  ani.options(interval = 0.5, nmax = 300)
}, video.name = "weiboFens.mp4", other.opts = "-b 800k")



