
library(tm)
#install.packages("tm")
library(stringr)
#library(wordcloud)
# ONCE: install.packages("Snowball")
## NOTE Snowball is not yet available for R v 3.5.x
## So I cannot use it  - yet...
##library("Snowball")
##set working directory
## ONCE: install.packages("slam")
library(slam)
library(quanteda)
## ONCE: install.packages("quanteda")
## Note - this includes SnowballC
library(SnowballC)
library(arules)
##ONCE: install.packages('proxy')
library(proxy)
library(cluster)
library(stringi)
library(proxy)
library(Matrix)
library(tidytext) # convert DTM to DF
library(plyr) ## for adply
library(ggplot2)
library(factoextra) # for fviz
library(mclust) # for Mclust EM clustering

library(naivebayes)
library(e1071)
library(caret)


Novels_dtm<- read.csv("CV_Abstracts_64Freq.csv", header = TRUE)


#(inspect(Novels_dtm))  ## This takes a look at a subset - a peak
DTM_mat <- as.matrix(Novels_dtm)
class(Novels_dtm)
class(DTM_mat)

#########################################################
######### OK - Pause - now the data is vectorized ######
## Its current formats are:
## (1) Novels_dtm is a DocumentTermMatrix R object
## (2) DTM_mat is a matrix
#########################################################

#Novels_dtm <- weightTfIdf(Novels_dtm, normalize = TRUE)
#Novels_dtm <- weightTfIdf(Novels_dtm, normalize = FALSE)

## Look at word freuqncies out of interest
(WordFreq <- colSums(as.matrix(Novels_dtm)))

(head(WordFreq))
(length(WordFreq))
ord <- order(WordFreq)
(WordFreq[head(ord)])
(WordFreq[tail(ord)])
## Row Sums
(Row_Sum_Per_doc <- rowSums((as.matrix(Novels_dtm))))

## Copy of a matrix format of the data
Novels_M <- as.matrix(Novels_dtm)
#(Novels_M[1:13,1:5])

## Normalized Matrix of the data
Novels_M_N1 <- apply(Novels_M, 1, function(i) round(i/sum(i),3))
#(Novels_M_N1[1:13,1:5])
## NOTICE: Applying this function flips the data...see above.
## So, we need to TRANSPOSE IT (flip it back)  The "t" means transpose
Novels_Matrix_Norm <- t(Novels_M_N1)
#(Novels_Matrix_Norm[1:13,1:10])

############## Always look at what you have created ##################
## Have a look at the original and the norm to make sure
(Novels_M[1:13,1:10])
(Novels_Matrix_Norm[1:13,1:10])

######### Next - you can convert a matrix (or normalized matrix) to a DF
Novels_DF_From_Matrix_N<-as.data.frame(Novels_Matrix_Norm)


## I am going to make copies here. 
m  <- Novels_M
m_norm <-Novels_Matrix_Norm
(str(m_norm))

###############################################################################
################# Build distance MEASURES using the dist function #############
###############################################################################
## Make sure these distance matrices make sense.
distMatrix_E <- dist(m, method="euclidean")
#print(distMatrix_E)
distMatrix_C <- dist(m, method="cosine")
#print("cos sim matrix is :\n")
#print(distMatrix_C)
#print("L2 matrix is :\n")
#print(distMatrix_E)
distMatrix_C_norm <- dist(m_norm, method="cosine")
#print("The norm cos sim matrix is :\n")
#print(distMatrix_C_norm)
###########################################################################

############# Clustering #############################
## Hierarchical

## Euclidean
groups_E <- hclust(distMatrix_E,method="ward.D")
plot(groups_E, cex=0.9, hang=-1, main = "hclust Ward", xlab = "Abstracts", ylab = "Euclidean Distance:Ward")
rect.hclust(groups_E, k=4)


## Cosine Similarity
groups_C <- hclust(distMatrix_C,method="ward.D")
plot(groups_C, cex=0.9, hang=-1, main = "hclust Cos", xlab = "Abstracts", ylab = "Cosine Similarity")
rect.hclust(groups_C, k=7)


## Cosine Similarity for Normalized Matrix
groups_C_n <- hclust(distMatrix_C_norm,method="ward.D")
plot(groups_C_n, cex=0.9, hang=-1, main = "hclust Cos (norm)", xlab = "Abstracts", ylab = "Cosine Similarity")
rect.hclust(groups_C_n, k=7)

### NOTES: Cosine Sim works the best. Norm and not norm is about
## the same because the size of the novels are not sig diff.


