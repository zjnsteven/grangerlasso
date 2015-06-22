#INPUTS:
# 'series': A NxT matrix 
# 'P':  	Length of the lag
# 'lambda': 	Value of the penalization parameter in Lasso
# 'type': 	'l' Lasso with the given Lambda
#  require r package glmnet
# based on the matlab version 
grangerlasso <-function(series, P, lmd, type)
{
  N = dim(series)[1]
  T = dim(series)[2]
  
  Am = matrix(0,T-P,P*N)
  bm = matrix(0,T-P,1)
  
  for (i in (P+1):T){
    bm[i-P] = series[1, i];
    Am[i-P,] = matrix(series[,(i-1):(i-P)],1,N*P)
  }
 
  fit = glmnet( Am, bm, lambda=lmd,family="gaussian",alpha=1)
  vals2 = fit$beta
  
  n1Coeff = matrix(0,N,P)
 
  for (i in 1:N){
    n1Coeff[i,] = vals2[((i-1)*P+1): (i*P)]
  }
 
  sumCause = matrix(rowSums(abs(n1Coeff)),N,1)
  cause = (sumCause > 0)*sumCause;
  
  return(cause)
  
}