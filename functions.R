source("libraries.R")

# operador para concatenar texto:
"%+%" <- function(a,b) paste(a,b,sep="")

# transformacion minmax
minmax <- function(x) (x-min(x))/(max(x)-min(x))

# saca n_rem outliers de data.frame
remove_outliers = function(df, n_rem) {
  good_i = MASS::cov.rob(df, cor=F, quantile.used=nrow(df)-n_rem)$best
  out = df[good_i, ]
  return(out)
}

# calcular distancias euclideas de conjunto de filas (matrix o dataframe) contra un vector
rowdist <- function(mat, vec, rownames=NULL) {
  out = apply(mat, 1, function(x) sqrt(sum((x-vec)^2)) )
  names(out) = rownames
  return(out)
}

# ALL duplicated indexes in a vector
every_dup = function(x) duplicated(x)|duplicated(x,fromLast=T)

# pairwise euclidean distances entre filas de dos dataframes o matrices
pwdist <- function(x, y, names_x=NULL, names_y=NULL) { 
  # fuente: https://www.r-bloggers.com/pairwise-distances-in-r/
    # tiene lo de 10e15 si no redondea y se va a cero
  xm = as.matrix(x) * 1e11 
  n_x = nrow(xm)
  ym = as.matrix(y) * 1e11
  n_y = nrow(ym)
  # sumas de cuadrados por fila 
  x_sc = apply(xm, 1, function(vec) crossprod(vec,vec)) 
  y_sc = apply(ym, 1, function(vec) crossprod(vec,vec)) 
  # sumas de cuadrados de x en n_y columnas 
  tmp1 = matrix(rep(x_sc,n_y), nrow=n_x)
  # sumas de cuadrados de y en n_x filas 
  tmp2 = matrix(rep(y_sc, n_x), nrow=n_x, byrow=TRUE)
  # (no entiendo)
  tmp = tmp1 + tmp2 
  # (no entiendo PERO FUNCIONA)
  out = sqrt(tmp - 2*tcrossprod(xm,ym)) / 1e11
  dimnames(out) = list(names_x, names_y)
  return(out)
}

