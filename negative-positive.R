
##find self-interacting-protein from positive PPI

ppi.exel<- read.csv("C:/Users/roozane/Desktop/PSSM/IntAct_PPI.csv")
ppi.mat <- as.matrix(ppi.exel)


spi.mat<- matrix(nrow=0,ncol=2)

for(r in 1:nrow(ppi.mat)){
  if(as.character(ppi.mat[row,1])==as.character(ppi.mat[row,2]))
   {
    print(r)
    #rbind(spi.mat,c(ppi.mat[row,col],ppi.mat[row,col+1]))
   }
}

##there is no self-interacting-protein

#****************************************************************************************
##making negative PPI from positive PPI

  
rand.mat <- matrix(sample(ppi.mat[,1]),nrow = 1500,ncol = 2)


for(i in 1:nrow(rand.mat)){
  if(j<-match(rand.mat[i,1],ppi.mat[,1])){
    if(rand.mat[i,2]==ppi.mat[j,2]){
      rand.mat<- rand.mat[-i,]
      
    }
  }
} 


View(rand.mat)
View(ppi.mat)

rand.mat<- rand.mat[1:1000,]
ppi.mat <- ppi.mat[1:1000,]


## chek if rand.mat contain self-interacting-protein
for(r in 1:nrow(rand.mat)){
  if(as.character(rand.mat[r,1])==as.character(rand.mat[r,2]))
  {
    print(r)
    #rbind(spi.mat,c(rand.mat[row,col],rand.mat[row,col+1]))
  }
}


rand.mat[,1]

##delete column 1

##write un tabel
write.csv(ppi.mat,"C:/Users/roozane/Desktop/PSSM/positive-1000.csv")
write.csv(ppi.mat,"C:/Users/roozane/Desktop/PSSM/negative-1000.csv") 






  
  
  
  
  
  
  
  
  
  
  
  
  
  











  