rm(list=ls())
func<- function(n,comb,mat){
  #print(n)
  if(n<1){
    mat = rbind(mat,comb)
    return(mat) 
  }
  comb[n] = 0
  mat = func(n-1,comb,mat)
  comb[n] = 1
  mat = func(n-1,comb,mat)
  return(mat)
  
}
patterns<-function(mat,mat2,flag,s=7){
  for(i in 2:dim(mat)[1]){
    if(flag){
      if(sum(mat[i,]) & sum(mat[i,])<=3){
        mat2 = rbind(mat2,mat[i,])
      }
    }
    else{
      if(sum(mat[i,]) == s){
        mat2 = rbind(mat2,mat[i,])
      }
      
    }
    
  }
  return(mat2)
  
}
conCheck  <- function(te){
  f = FALSE
  i = 1
  while(i<=length(te)){
    n = i+6
    if(sum(te[i:n])>3){
      f = TRUE
    }
    i = i+7
  }
  return(f)
  
}
costFun<-function(temp,c){
  cost = 0
  count = 0
  for(i in 1:dim(temp)[1]){
    if(!conCheck(temp[i,])){
      for(j in 1:dim(temp)[2]+1){
        if(j==dim(temp)[2]+1 || temp[i,j]==0){
          if(count == 1){
            if((j-1)%%7==0 || j%%7==0){
              cost = cost + c[2]
            }
            else{
              cost = cost + c[1]
            }
          }
          else if(count ==2){
            if((j-1)%%7==0){
              cost = cost + c[2] + c[3]
            }
            else{
              cost = cost + 2*c[1]
            }
          }
          else if(count ==3){
            if((j-2)%%7==0){
              cost = cost + c[2] + c[3] + c[4]
            }
            else if(j%%7==0){
              cost = cost + 2*c[1] + c[6]
            }
            else{
              cost = cost + 3*c[1]
            }
          }
          else if(count ==4){
            if((j-3)%%7==0){
              cost = cost + c[2] + c[3] + c[4] + c[7]
            }
            else if((j-4)%%7==0){
              cost = cost + c[2] + c[1] + c[1] + c[8]
            }
            else{
              cost = cost + 999999
            }
            
          }
          else if(count>=4){
            cost = cost + 999999
          }
          count = 0
          
        }
        else{
          count = count + 1
        }
        
      }
    }
    else{
      cost = cost +999999
    }
    
  }
  return(cost)
  
}

#hpatterns


s = 7
mat2 = matrix(0,ncol = s)
mat = matrix(0,ncol = s)
comb = integer(s)
mat = func(s,comb,mat)
mat2 = patterns(mat,mat2,TRUE)
hpatterns = mat2[-1,]
hpatterns = hpatterns[-35,]
rm(list = c("mat","mat2","comb","s"))



R = c(5,5,5,5,5,3,3,5,5,5,5,8,7,3,6,6,7,7,5,3,3)
C = c(1,1.5,2,1.25,1.5,2,1.5,1.5)
W = c()
val = 0
maxi = 0
for(i in 1:22){
  if((i-1)%% 7 == 0){
    val = ceiling(val/3)
    maxi = max(val,maxi)
    val = 0
    
  }
  if(i<=21)
    val = val +R[i]
}
W = maxi
rm(list = c("i","maxi","val"))
#vpatterns
P = matrix(0,nrow = W,ncol = length(R))
min_cost = 99999
temp = P
for(i in 1:length(R)){
  s = W
  mat2 = matrix(0,ncol = s)
  mat = matrix(0,ncol = s)
  comb = integer(s)
  mat = func(s,comb,mat)
  mat2 = patterns(mat,mat2,FALSE,R[i])
  mat2 = mat2[-1,]
  for(j in 1:dim(mat2)[1]){
    temp[,i] = t(mat2[j,])
    cos = costFun(temp,C)
    if(cos<min_cost){
      P = temp
      min_cost = cos
    }
    else{
      temp = P
    }
  }
  min_cost = 99999
  
  rm(list = c("mat","mat2","comb","s"))
}
print(P)
print("Cost:")
print(costFun(P,C))





