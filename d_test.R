d_test <- function(){
  set.seed(19)
  n <- 20
  mtx <- matrix(data = rnorm(n = n * 3), nrow = n,ncol = 3)
  df <- data.frame(mtx)
  colnames(df) <- paste0("nm_",1:3)
  df <- cbind(df,
              cat_1 = as.factor(sample(x = letters[1:3],size = n,replace = T)),
              bin_1 = as.factor(sample(x = c("Y","N"), size = n, replace = T)),
              id = paste0("id_",1:nrow(df)))
  
  test_d <- list()
  
  # Order check
  test_d$order <- list(old = df, new = df[sample(1:nrow(df)), sample(1:ncol(df))])
  
  # attributes check
  test_d$attrs <- list(same = list(old = df, new = df ))
  test_d$attrs$same$new$nm_1[1] <- rnorm(n = 1)
  test_d$attrs$same$new$nm_2[1] <- rnorm(n = 1)
  test_d$attrs$same$new$nm_3[1] <- rnorm(n = 1)
  test_d$attrs$same$new$id[1] <- "id_01"
  test_d$attrs$same$new$cat_1[1] <- "b"
  test_d$attrs$same$new$bin_1[1] <- "N"
  
  test_d$attrs$different <- list(col_same = NA, col_diff = NA)
  
  test_d$attrs$different$col_same <- list(old = df, new = df )
  attr(x = test_d$attrs$different$col_same$new, which = "key_1") <-"value_1" 
  
  
  test_d$attrs$different$col_diff <- list(old = df, new = cbind(df,cat_2 = sample(df$cat_1)))
  
  test_d$identical <- list(old = df, new = df)
  
  test_d$records_added <- list(old = df)
  
  m<- n/2
  mtx <- matrix(data = rnorm(n = m * 3), nrow = m,ncol = 3)
  mtx <- rbind(mtx, matrix(data = rnorm(n = m * 3,mean = 5), nrow = m,ncol = 3))
  df1 <- data.frame(mtx)
  colnames(df1) <- paste0("nm_",1:3)
  df1 <- cbind(df1,
              cat_1 = as.factor(sample(x = letters[1:3],size = n,replace = T)),
              bin_1 = as.factor(sample(x = c("Y","N"), size = n, replace = T)),
              id = paste0("id_",n+1:nrow(df1)))
  
  
  test_d$records_added$new <- rbind(df,df1)

  
  return(test_d)
}


# # Added and deleted
# 
# 
# compareDF::compare_df(df_new = test_d$order$old, df_old = test_d$order$new)
