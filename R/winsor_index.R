#' WINSOR INDEX
#'
#' This function helps users to manage outliers in the purchase task variables through
#' winsorization techniques. The winsorization options are identical to the options
#' available in the `winsor_price()` function. Specifically, i) Option 1 replaces
#' outliers with the maximum non-outlying price rounded up; ii) Option 2 replaces
#' outliers with a price 1 higher than highest (or 1 lower than the lowest) non-outlying
#' value; or iii) outlying values are replaced with 1 value above the next highest
#' non-outlying value to maintain order.
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param index_var The name of the index variable to winsorize.
#' @param z_val The absolute z-value to define outliers.
#' @param type Type of winsorization, specifically one of: c("option1","option2","option3")
#' @param delta used to retain winsorization order when using winsorization type "option3"
#' @examples
#' \dontrun{
#' pt2 <- winsor_index(pt, index_var = "Intensity", type = "option3")
#' }
#' @export

winsor_index <- function(pt, index_var, z_val = 3.99, type = "option3", delta = 0.001) {

  pt2 <- pt[!is.na(pt[,c(index_var)]),]
  z_pt <- scale(pt2[,c(index_var)], center = TRUE, scale = TRUE)
  above_pos_z_val <- unique(pt2[,c(index_var)][z_pt > z_val])
  below_neg_z_val <- unique(pt2[,c(index_var)][z_pt < -z_val])
  pt_outliers <- append(above_pos_z_val, below_neg_z_val)

  if(type == "option1") {

    if (length(above_pos_z_val)>0){
      for (pos_z_val in sort(above_pos_z_val)){
        pt2[,c(index_var)][pt2[,c(index_var)] == pos_z_val] <- max(pt2[,c(index_var)][z_pt < z_val])
      }
    }
    if (length(below_neg_z_val)>0){
      for (neg_z_val in sort(below_neg_z_val,decreasing = TRUE)){
        pt2[,c(index_var)][pt2[,c(index_var)] == neg_z_val] <- min(pt2[,c(index_var)][z_pt > -z_val])
      }
    }

  } else if(type=="option2"){

    if (length(above_pos_z_val)>0){
      for (pos_z_val in sort(above_pos_z_val)){
        pt2[,c(index_var)][pt2[,c(index_var)] == pos_z_val] <- max(pt2[,c(index_var)][z_pt < z_val]) + 1
      }
    }
    if (length(below_neg_z_val)>0){
      for (neg_z_val in sort(below_neg_z_val,decreasing = TRUE)){
        pt2[,c(index_var)][pt2[,c(index_var)] == neg_z_val] <- min(pt2[,c(index_var)][z_pt > -z_val]) - 1
      }
    }

  } else if (type=="option3"){

    if (length(above_pos_z_val)>0){
      q <- 1
      for (pos_z_val in sort(above_pos_z_val)){
        pt2[,c(index_var)][pt2[,c(index_var)] == pos_z_val] <- max(pt2[,c(index_var)][z_pt < z_val]) + q*delta
        q <- q + 1
      }
    }
    if (length(below_neg_z_val)>0){
      for (neg_z_val in sort(below_neg_z_val,decreasing = TRUE)){
        q <- 1
        pt2[,c(index_var)][pt2[,c(index_var)] == neg_z_val] <- min(pt2[,c(index_var)][z_pt > -z_val]) - q*delta
        q <- q + 1
      }
    }
  }
  pt_out <- pt2[,c(index_var)]
  pt[,c(index_var)] <- replace(pt[,c(index_var)], !is.na(pt[,c(index_var)]), pt_out)
  return(pt)

}

