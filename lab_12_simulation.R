generate_data = function(n,p){
  
  mat = matrix( rnorm(n*p,mean=0,sd=1), n, p) 
  vec = rnorm(n,mean=0,sd=1)
  result = list(covariates = mat, responses = vec)
  return(result)
  
  
}


model_select = function(covariates, responses, cutoff)
{
  index = c() #storing response column index
  df = data.frame(cov = covariates, res = responses)
  model = lm(formula = res ~ cov.1 + cov.2 + cov.3 + cov.4 + cov.5 + cov.6, data = df)
  summ = summary(model)
  for (i in 23:28)
  {
    if (summ[[4]][i] <= cutoff) 
    {
      index = c(index, (i - 22)) #index of the column to keep
    }
  }
  if (length(index) == 0)
  {
    return(index)
  }
  lm_new = lm(formula = responses ~ covariates[, index])
  mod.sum = summary(lm_new)
  return(mod.sum$coefficients[,4])
}





