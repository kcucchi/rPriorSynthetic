
#'loading parameters from a csv file
#'
#'@param pathCsv path to csv file
#'@param iLine index corrsponding to the line to read
#'@return a named vector containing parameters of configuration setup
#'@export
csv2vec <- function(pathCsv,iLine){

  # save csv content in dataframe
  df_param <- read.csv(file = pathCsv,header = T,sep = ';')

  # save line iLine in vector
  vec_param <- as.list(x = df_param[iLine,])
  # define names of parameters
  names(vec_param) <- names(df_param)

  # additional adjustement
  # transform indices from numeric to integer
  # locations of indices in vector
  where_idx <- grep(pattern = 'idx',x = names(vec_param))
  # transform to integer
  vec_param[where_idx] <- as.integer(vec_param[where_idx])

  return(vec_param)

}



