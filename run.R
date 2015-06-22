# run grangerlasso.R first
# input matrix
refinedtest = test[,c(4:8,10:21)]
input = t(refinedtest)
N = dim(input)[1]
# run lasso granger
lambda = 1e-2;
L = 10;      

cause = matrix(0,N,N)
for (i in 1:N){
  if((i+1)<=N && (i-1)>=1){
    index = c(i, 1:(i-1), (i+1):N)
  }
  if(i==1){
    index = c(1:N)
  }
  if(i==N){
    index = c(N, 1:(N-1))
  }
  temp = grangerlasso(input[index, ], L, lambda, 'l')
  if(i==1){
    cause[i, ] = t(temp[c( 1, (i+1):N),])
  }
  if(i==N){
    cause[i, ] = t(temp[c(2:N, 1),])
  }
  if((i+1)<=N && (i-1)>=1){
    cause[i, ] = t(temp[c(2:i, 1, (i+1):N),])
  }
  
}

