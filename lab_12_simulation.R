generate_data = function(n,p){
  
  mat = matrix( rnorm(n*p,mean=0,sd=1), n, p) 
  vec = rnorm(n,mean=0,sd=1)
  result = list(covariates = mat, responses = vec)
  return(result)
  
  
}








