working <- "~/Desktop/Rdata/"
setwd(working)

#check the WD
getwd()
data <- read.csv(file="~/Downloads/lastfm-matrix-germany.csv")

############################
#  Item Based Similarity   #
############################   

data.ibs <- (data[,!(names(data) %in% c("user"))])

get_Cosine <- function(x,y) 
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

mat <- matrix(NA, nrow=ncol(data.ibs),ncol=ncol(data.ibs),dimnames=list(colnames(data.ibs),colnames(data.ibs)))
data.ibs.similarity <- as.data.frame(mat)

# Lets fill in those empty spaces with cosine similarities
for(i in 1:ncol(data.ibs)) {
  for(j in 1:ncol(data.ibs)) {
    data.ibs.similarity[i,j]= get_Cosine(data.ibs[i],data.ibs[j])
  }
}

write.csv(data.ibs.similarity,file="final-germany-similarity.csv")

# Get the top 10 neighbours for each
data.neighbours <- matrix(NA, nrow=ncol(data.ibs.similarity),ncol=11,dimnames=list(colnames(data.ibs.similarity)))

for(i in 1:ncol(data.ibs)) 
{
  data.neighbours[i,] <- (t(head(n=11,rownames(data.ibs.similarity[order(data.ibs.similarity[,i],decreasing=TRUE),][i]))))
}

write.csv(file="final-germany-item-neighbours.csv",x=data.neighbours[,-1])


############################
# User Based               #
############################    

get_Score <- function(history, similarities)
{
  x <- sum(history*similarities)/sum(similarities)
  x
}

# A placeholder matrix
mat <- matrix(NA, nrow=nrow(data),ncol=ncol(data)-1,dimnames=list((data$user),colnames(data[-1])))

# Loop through the users (rows)
for(i in 1:nrow(mat)) 
{
  # Loops through the products (columns)
  for(j in 1:ncol(mat)) 
  {
    # Get the user's name and th product's name
    # We do this not to conform with vectors sorted differently 
    user <- rownames(mat)[i]
    product <- colnames(mat)[j]
    
    # We do not want to recommend products you have already consumed
    # If you have already consumed it, we store an empty string
    if(as.integer(data[data$user==user,product]) == 1)
    { 
      mat[i,j]<-""
    } else {
      
      topN<-((head(n=11,(data.ibs.similarity[order(data.ibs.similarity[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      topN.similarities<-topN.similarities[-1]
      topN.names<-topN.names[-1]
      
      topN.purchases<- data[,c("user",topN.names)]
      topN.userPurchases<-topN.purchases[topN.purchases$user==user,]
      topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("user"))])
      
      mat[i,j]<-get_Score(similarities=topN.similarities,history=topN.userPurchases)
      
    } 
  }  
}

data.user.scores <- mat
write.csv(file="final-user-scores.csv",data.user.scores)

data.user.scores.mat <- matrix(NA, nrow=nrow(data.user.scores),ncol=100,dimnames=list(rownames(data.user.scores)))
for(i in 1:nrow(data.user.scores)) 
{
  data.user.scores.mat[i,] <- names(head(n=100,(data.user.scores[,order(data.user.scores[i,],decreasing=TRUE)])[i,]))
}

write.csv(file="final-user-recommendations.csv",data.user.scores.mat)