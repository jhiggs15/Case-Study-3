library(polynom)
library(ggplot2)
library(reshape2)
library(dplyr)

PCA = function(df){
  stdrd = matrix(nrow = nrow(df), ncol = ncol(df))
  numeric_cols = c()
  non_numeric = c()
  
  for(c in (1 : ncol(df))){
    if(is.numeric(df[,c])){
      stdrd[,c] = (df[,c] - mean(df[,c]))
      numeric_cols = c(numeric_cols, c)
    }
    else{
      stdrd = stdrd[, -c]
      non_numeric = c(non_numeric, c)
    }
    
  }

  eigens = eigen(cov(stdrd,  method = "kendall"))
  eigen_vals = eigens$values
  
  for(r in (1 : length(stdrd[,1])) ){
    stdrd[r, ] = t(eigens$vectors) %*% stdrd[r,]
  }
  
  col_names = c()
  for(c in (1 : ncol(stdrd)) ){
    col_names = c(col_names, paste("PC", c, sep = ""))
  }
  
  
  colnames(stdrd) = col_names
  
  correlations = data.frame(t(cor(stdrd, df[,numeric_cols], method = "pearson")))
  
  stdrd = cbind(stdrd, df[non_numeric])
  
  list(cor = correlations, eigen_vals = eigen_vals, new_vals = as.data.frame(stdrd), nn = non_numeric, n = numeric_cols)
  
}