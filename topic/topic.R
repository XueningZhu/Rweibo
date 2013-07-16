
setwd("E:/R_learning/weibo/topic")
load("fris.rda")
load("res.rda")
stopwords=readLines("E:/R_learning/weibo/CH_stopwords.txt")

## Rwordseg : http://jliblog.com/app/rwordseg
library(Rwordseg)
importSogouScel("E:/R_learning/weibo/machine_learning.scel")

installDict("E:/R_learning/weibo/machine_learning.scel", dictname = "machine_learning.scel",
            dicttype = c("scel"))
installDict("E:/R_learning/weibo/stat.scel", dictname = "stat.scel",
            dicttype = c("scel"))
installDict("E:/R_learning/weibo/math.scel", dictname = "math.scel",
            dicttype = c("scel"))
insertWords("转发微博")


library(Matrix)
library(slam)
library(tm)

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

get.forward<-function(wei)
{
  if (!is.null(dim(wei)))
  {
    forward=wei$Forward
    forward=forward[!is.na(forward)]
    
    forward_seg=unlist(segmentCN(forward))
    forward_seg=forward_seg[forward_seg!=""]
    #res=res[grepl("[0-9A-Za-z]",res,perl=T)==F]
    forward_seg=forward_seg[nchar(forward_seg)>1]
    word_f=table(forward_seg)
    
    stopwords=c(stopwords,"http","cn","www","转发","转发微博")
    word=setdiff(forward_seg,stopwords)
    
    a=grepl("[A-Za-z]",word,perl=T)&(nchar(word)<=3)|grepl("[0-9]",word,perl=T)
    word=word[!a]
    word_f=word_f[word]
    return(word_f)
  }
  return(0)
}

res_forward=lapply(res,get.forward)
ll=sapply(res_forward,function(x) return(is.null(dim(x))))
res_forward1=res_forward[!ll]
res_forward_mat=get.mat(col=get.col(res_forward1),res_forward1,M=F)
forward_col=get.col(res_forward1)

library(topicmodels)

dtm <- as.DocumentTermMatrix(res_forward_mat,weighting =weightTf,
                             control = list(stemming = TRUE, stopwords = TRUE, removePunctuation = TRUE,tolower=T))

#tf=col_sums(dtm)

term_tfidf <-tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) *
  log2(nDocs(dtm)/col_sums(dtm > 0))
#summary(term_tfidf)
nn=which(is.element(forward_col,forward_col[order(term_tfidf)[1:50]])|
           is.element(forward_col,forward_col[order(tf,decreasing=T)[1:100]]))
dtm <- dtm[,-nn]
dtm <- dtm[row_sums(dtm) > 0,]

k <- 5
SEED <- 2013
TM <- list(VEM = LDA(dtm, k = k, control = list(seed = SEED)),
           VEM_fixed = LDA(dtm, k = k,
                           control = list(estimate.alpha = FALSE, seed = SEED)),
           Gibbs = LDA(dtm, k = k, method = "Gibbs",
                       control = list(seed = SEED, burnin = 1000,
                                      thin = 100, iter = 1000)),
           CTM = CTM(dtm, k = k,
                     control = list(seed = SEED,
                                    var = list(tol = 10^-4), em = list(tol = 10^-3))))


Topic <- topics(TM[["VEM"]], 2)

#most frequent terms for every topic
Terms <- terms(CTM, 5)
Terms



