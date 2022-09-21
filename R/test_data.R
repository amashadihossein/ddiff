#' generate two datasets for comparison
#'
#' @description Generate two datasets for comparison. The comparison are made on
#'  two main aspects : form and information
#' @import fdapace
#' @return data 1 and data 2.
#' @examples
#' d = test_data()
#' data.tree::FromListSimple(d)
#' @export
#'
test_data <- function(){

  dat <- medfly25
  n = nrow(dat)
  set.seed(19)
  mtx <- matrix(data = rnorm(n = n * 3), nrow = n, ncol = 3)
  df <- data.frame(mtx)
  colnames(df) <- paste0("nm_",1:3)
  df <- cbind(dat, df,
              cat_1 = as.factor(sample(x = letters[1:3], size = n, replace = T)),
              cat_2 = as.factor(sample(x = letters[4:6], size = n, replace = T)),
              bin_1 = as.factor(sample(x = c("Y","N"), size = n, replace = T)),
              bin_2 = as.factor(sample(x = c("M","F"), size = n, replace = T)))
  test_d <- list()

  # Order check
  test_d$order <- list(old = df, new = df[sample(1:nrow(df)), sample(1:ncol(df))])

  # attributes check
  test_d$attrs <- list(same = list(old = df, new = df ))
  test_d$attrs$same$new$nm_1[1] <- rnorm(n = 1)
  test_d$attrs$same$new$nm_2[1] <- rnorm(n = 1)
  test_d$attrs$same$new$nm_3[1] <- rnorm(n = 1)
  #test_d$attrs$same$new$id[1] <- "id_01"
  test_d$attrs$same$new$cat_1[1] <- "b"
  test_d$attrs$same$new$bin_1[1] <- "N"

  test_d$attrs$different <- list(col_same = NA, col_diff = NA)

  test_d$attrs$different$col_same <- list(old = df, new = df )
  attr(x = test_d$attrs$different$col_same$new, which = "key_1") <-"value_1"


  test_d$attrs$different$col_diff <- list(old = df, 
                                          new = cbind(df,cat_2 = sample(df$cat_1)))

  test_d$identical <- list(old = df, new = df)

  df1 <- df[df$Days <= 15, ]

  test_d$records_added <- list(old = df1)

  test_d$records_added$new <- df
  return(test_d)
}
