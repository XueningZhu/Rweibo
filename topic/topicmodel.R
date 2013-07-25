
#library(Rweibo)
setwd("F:/weibo/data/719")
load("sports.rda")
load("entertain.rda")
load("economic.rda")
load("media.rda")
load("literature.rda")
load("fashion.rda")
load("IT.rda")
load("education.rda")
load("res_719.rda")

name=c(sports$name,entertain$name,economic$name,media$name,
       literature$name,fashion$name,IT$name,education$name)
cate=c(sports$cate,entertain$cate,economic$cate,media$cate,
       literature$cate,fashion$cate,IT$cate,education$cate)

sports$cate="sports"
entertain$cate="entertain"
economic$cate="economic"
media$cate="media"
literature$cate="literature"
fashion$cate="fashion"
IT$cate="IT"
education$cate="education"


aa=sapply(res_719,function(x) {return(!is.null(dim(x)))})
weibo=res_719[aa]

### word segment
library(Rwordseg)
installDict("F:/weibo/data/搜狗标准词库.scel", dictname = "biaozhun.scel",
            dicttype = c("scel"))
insertWords("微博")
stopwords=readLines("E:/R_learning/weibo/CH_stopwords.txt")
installDict("F:/weibo/data/word_14108.scel", dictname = "word1.scel",
            dicttype = c("scel"))

## my scel record, but failed
#PuddingScel=readLines("F:/weibo/data/dic/PuddingScel.txt",encoding="UTF-8")

#myScel=gsub("[a-z\\']+\\s","",PuddingScel)
#ll=nchar(myScel)
#myScel=myScel[ll>0&ll<=3]


#pattern=paste("(",paste(stopwords,collapse=")|("),")",sep="")
#myScel1=gsub("啊","",myScel[1:10],fixed=T)

#insertWords(myScel)

### for each weibo as a doc

Clean.Weibo.list<-function(x)
{
  weibos=c(x$Weibo,x$Forward)
  weibos=weibos[!is.na(weibos)]
  weibos1=gsub("(http://[a-z\\.\\/\\-0-9\\(\\)\\=]+)|(@[\u4e00-\u9fa5\\w]+\\s)|(//@[^\\s]+:)"," ",
               weibos,
               perl=T)
  weibos1=gsub('[[:punct:][:digit:]a-zA-Z\\-]+'," ",weibos1)
  seg_weibo=segmentCN(weibos1)
  seg_weibo1=lapply(seg_weibo,
                    function(x) 
                      {y=setdiff(x,stopwords);z=y[y!=""&nchar(y)>=2];
                       if (length(z)==0) return(0)
                       return(table(z))})
  ll=sapply(seg_weibo1,function(x) return(all(x==0)))
  if (all(ll)) return(0)
  return(seg_weibo1[!ll])
}
weibo_doc1=lapply(weibo,Clean.Weibo.list)
ll=sapply(weibo_doc1,function(x) {return(is.list(x))})
weibo_doc1=weibo_doc1[ll]

## sample the weibo 
Weibo.sample<-function(x)
{
  l=length(x)
  if (l<10) return(x)
  ind=sample(1:l,floor(l/3))
  return(x[ind])
}
weibo_doc1_sample=lapply(weibo_doc1,Weibo.sample)
weibo_doc2=unlist(weibo_doc1_sample,recursive=F)

#weibo_length=sapply(weibo_doc1,length)
#save(weibo_length,file="weibo_length.rda")



name=name[ll]
save(name,file="name.rda")
cate=cate[aa]
cate=cate[ll]
save(cate,file="cate.rda")
### get the doc-term matrix, denoise with the tf-idf 

library(Matrix)

library(slam)
library(tm)
library(topicmodels)

get.col<-function(com)
{
  col=unique(unlist(lapply(com,function(x) names(x))))
  return(col)
}

get.mat<-function(col,com,M=F)
{
  nrow=length(com);ncol=length(col)
  ijv=NULL
  for (i in 1:length(com))
  {
    cat(i,"\n")
    ii=which(is.element(col,names(com[[i]])))
    ijv=rbind(ijv,cbind(i,ii,as.vector(com[[i]])))
  }
  if (M==T)
    mat=sparseMatrix(ijv[,1],ijv[,2],ijv[,3],
                     dims=c(nrow,ncol))
  else
    mat=simple_triplet_matrix(ijv[,1],ijv[,2],ijv[,3], dimnames = NULL)
  
  colnames(mat)=col
  return(mat)
}
col_long=get.col(weibo_doc2)
weibo_mat_long=get.mat(col=col_long,weibo_doc2,M=F)
dtm <- as.DocumentTermMatrix(weibo_mat_long,weighting =weightTf,
                             control = list(stemming = TRUE, stopwords = TRUE, removePunctuation = TRUE,tolower=T))

term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
  log2(nDocs(dtm)/col_sums(dtm > 0))

ll=term_tfidf>=quantile(term_tfidf,0.01)
dtm <- dtm[,ll]
dtm <- dtm[row_sums(dtm) > 0,]

#tf=col_sums(dtm)
#nn=setdiff(order(tf,decreasing=T)[1:n],which(col=="comput"))

#term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
#  log2(nDocs(dtm)/col_sums(dtm > 0))
#summary(term_tfidf)




# col[order(term_tfidf)[1:1000]]
#col[order(tf,decreasing=T)[1:1000]]

### select the best k using the perplexity and loglikelihood rule

smp<-function(cross=5,n,seed)
{
  set.seed(seed)
  dd=list()
  aa0=sample(rep(1:cross,ceiling(n/cross))[1:n],n)
  for (i in 1:cross) dd[[i]]=(1:n)[aa0==i]
  return(dd)
}
selectK<-function(dtm,kv=seq(5,60,5),SEED=2013,cross=5,sp)
{
  per_gib=NULL
  log_gib=NULL
  for (k in kv)
  {
    per=NULL
    loglik=NULL
    for (i in 1:cross)
    {
      te=sp[[i]]
      tr=setdiff(1:nrow(dtm),te)
      Gibbs = LDA(dtm[tr,], k = k, method = "Gibbs",
                  control = list(seed = SEED, burnin = 1000,
                                 thin = 100, iter = 1000))
      per=c(per,perplexity(Gibbs,newdata=dtm[te,]))
      loglik=c(loglik,logLik(Gibbs,newdata=dtm[te,]))
    }
    
    per_gib=rbind(per_gib,per)
    log_gib=rbind(log_gib,loglik)
  }
  return(list(perplex=per_gib,loglik=log_gib))
}

sp=smp(n=nrow(dtm),seed=2013)
## time consuming
system.time((gibK=selectK(dtm=dtm,kv=seq(5,60,5),SEED=2013,cross=5,sp=sp)))
## draw the result
m_per=apply(gibK[[1]],1,mean)
m_log=apply(gibK[[2]],1,mean)
k=seq(5,60,5)
plot(x=k,y=m_per) 
k[which.min(m_per)] 
plot(x=k,y=m_log) 
k[which.max(m_log)]

## choose 50 topics to train the models
k <- 50
SEED <- 2013

VEM = LDA(dtm, k = k, control = list(seed = SEED))
VEM_fixed = LDA(dtm, k = k,
                control = list(estimate.alpha = FALSE, seed = SEED))
CTM = CTM(dtm, k = k,
          control = list(seed = SEED,
                         var = list(tol = 10^-4), em = list(tol = 10^-3)))

Gibbs = LDA(dtm, k = k, method = "Gibbs",
            control = list(seed = SEED, burnin = 1000,
                           thin = 100, iter = 1000))

terms(Gibbs,5)

## inference on every person as a doc
Clean.Weibo<-function(x)
{
  weibos=c(x$Weibo,x$Forward)
  weibos=weibos[!is.na(weibos)]
  weibos1=gsub("(http://[a-z\\.\\/\\-0-9\\(\\)\\=]+)|(@[\u4e00-\u9fa5\\w]+\\s)|(//@[^\\s]+:)"," ",
               weibos,
               perl=T)
  seg_weibo=unlist(segmentCN(weibos1))
  seg_weibo1=setdiff(seg_weibo,stopwords)
  seg_weibo2=gsub('[[:punct:][:digit:]a-zA-Z\\-]+',"",seg_weibo1)
  seg_weibo2=seg_weibo2[seg_weibo2!=""&nchar(seg_weibo2)>=2]
  if (length(seg_weibo2)==0) return(0)
  return(table(seg_weibo2))
}
weibo_doc=lapply(weibo,Clean.Weibo)
ll=sapply(weibo_doc,function(x) {return(!all(x==0))})
weibo_doc=weibo_doc[ll]
col=get.col(weibo_doc)
weibo_mat=get.mat(col=col,weibo_doc,M=F)
dtm <- as.DocumentTermMatrix(weibo_mat,weighting =weightTf,
                             control = list(stemming = TRUE, stopwords = TRUE, removePunctuation = TRUE,tolower=T))
dist=posterior(Gibbs,newdata=dtm)[[2]]

### cluster using some topics
library(cluster)
library(proxy)
dist1=dist[,c(2,5,7,9,12,17,18,21,22,24,28,31,36,38,44,50)]
dtm1 <- as.DocumentTermMatrix(dist1,weighting =weightTf,
                             control = list(stemming = TRUE, stopwords = TRUE, removePunctuation = TRUE,tolower=T))

dist_dtm <- dissimilarity(dtm1, method = 'cosine')
dist_dtm_tf <-dissimilarity(dtm, method = 'cosine')

hc <- hclust(dist_dtm, method = 'ward')
hc_tf<-hclust(dist_dtm_tf, method = 'ward')

library(ggplot2)
heatmap(as.matrix(dist_dtm),labRow = F, labCol = F)
heatmap(as.matrix(dist_dtm_tf),labRow = F, labCol = F)


## show the topic of every cluster

colnames(dist1)=c("电子","娱乐","城管","地震","新闻","机场","广告","影音",
               "体育","政治","时尚","文化","法律","情感","青春","家庭")

k=2
while(max(sapply(rect.hclust(hc, k=k),length))>60)
  {k=k+1;     
   show(k)}

hc_res=rect.hclust(hc, k=10) 
hc_mean=lapply(1:length(hc_res),function(x) 
  {return(data.frame(group=as.factor(x),topic=as.factor(colnames(dist1)),value=apply(dist1[hc_res[[x]],],2,mean)))})
hc_mean_d=do.call(rbind,hc_mean)

p <- ggplot(hc_mean_d, aes(x=topic,y=value)) +
  geom_bar(stat="identity",fill = topic,col="black")
p <-p+ facet_wrap(~ group, ncol = 2,drop=F)
p=p + theme(axis.text.y = element_text(size=rel(1.5),colour="black"),
            axis.text.x = element_text(size=rel(1.7),colour="black",angle=45),
            axis.title.y = element_text(size = rel(1.5), angle = 90,face="bold"),
            axis.title.x = element_text(size = rel(1.5),face="bold"),
            plot.title = element_text(size = rel(1.8),face="bold"),
            strip.text = element_text(size = 15, face="bold",
                                      hjust = 0.5, vjust = 0.5))

p + scale_fill_brewer()

hc_name=lapply(hc_res,function(x) {return(name[x])})

