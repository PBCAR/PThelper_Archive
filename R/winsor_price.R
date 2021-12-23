#' WINSOR PRICE
#'
#' This function is used as outlier management at the price level via winsorization.
#' Outliers are identified by z-scores higher than the `z_val` or lower than the
#' negative `z_val`. There are 3 different winsorization types: i) Option 1 replaces
#' outliers with the maximum non-outlying price rounded up; ii) Option 2 replaces
#' outliers with a price 1 higher than highest (or 1 lower than the lowest) non-outlying
#' value; or iii) outlying values are replaced with 1 value above the next highest
#' non-outlying value to maintain order.
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier (ID) as identified in the data frame.
#' @param type The type of outlier management via winsorization. One of c("option1","option2","option3").
#' @param z_val The absolute z-value to identify outlying responses above/ below the negative z-score.
#' @param table If set to TRUE, an html table is provided, which is especially helpful for large data
#' sets. The default is FALSE, and a table in the console will be printed.
#' @examples
#' \dontrun{
#' pt <- winsor_price(pt, id_var = "ID", type = "option3")
#' }
#' @export

winsor_price <- function(pt, id_var, type, z_val = 3.99, table = FALSE) {

  pt_names <- names(pt)
  prices <- pt_names[pt_names!=id_var]
  names(pt)[names(pt) == id_var] <- "id"
  pt2 <- pt

  # TRANSFORM into z-scores
  z_pt <- pt
  z_pt[c(prices)] <- scale(pt[c(prices)], center = TRUE, scale = TRUE)

  ##### MODIFIED Price List (if final price array not reached)
  price.count <- colSums(pt[prices])
  price_df <- data.frame(price.count,prices)
  mod.prices <- price_df$prices[price_df$price.count!=0]

  ### OPTION 1 Winsorization
  if(type == "option1") {

    for (price in mod.prices){
      pt2[z_pt[,price]> z_val,price] <- ceiling(z_val*stats::sd(pt2[,price])+
                                                  mean(pt2[,price]))
      pt2[z_pt[,price]< -z_val,price] <- floor(-z_val*stats::sd(pt2[,price])+
                                                 mean(pt2[,price]))
    }

    ### OPTION 2 Winsorization
  } else if(type == "option2") {
    for (price in mod.prices){
      pt2[z_pt[,price]> z_val,price] <- max(pt2[z_pt[,price]< z_val,price]) + 1
      pt2[z_pt[,price]< -z_val,price] <- min(pt2[z_pt[,price]> -z_val,price]) - 1
    }

    ### OPTION 3 Winsorization
  } else if(type == "option3") {

    for (price in mod.prices){
      above.zval <- unique(z_pt[z_pt[,price]> z_val,price])
      below.neg.zval <- unique(z_pt[z_pt[,price]< -z_val,price])
      if (length(above.zval)>0){
        for (q in seq(1, length(above.zval), by=1)){
          if (length(above.zval)>1){
            quantity.zs <- above.zval[order(above.zval)][q]
          } else if (length(above.zval)==1) {
            quantity.zs <- above.zval[q]
          }
          pt2[z_pt[,price]==quantity.zs,price] <- max(pt2[z_pt[,price]< z_val,price]) + q
        }
      }
      if (length(below.neg.zval)>0){
        for (q in seq(1, length(below.neg.zval), by=1)){
          if (length(below.neg.zval)>1){
            quantity.zs <- below.neg.zval[rev(order(below.neg.zval))][q]
          } else if (length(below.neg.zval)==1) {
            quantity.zs <- below.neg.zval[q]
          }
          pt2[z_pt[,price]==quantity.zs,price] <- min(pt2[z_pt[,price]> -z_val,price]) - q
        }
      }
    }

  }

  # IDENTIFY IDs with winsorization changes
  pt_winsor <- data.frame(ID=character(),
                          Price=numeric(),
                          Original_Value=integer(),
                          Winsorized_Value=integer())

  i = 1
  for (id_num in pt$id){
    for (price in prices){
      orig = pt[pt$id == id_num,price]
      new = pt2[pt2$id == id_num,price]
      if (orig != new){
        pt_winsor[i,1] <- id_num
        pt_winsor[i,2] <- price
        pt_winsor[i,3] <- orig
        pt_winsor[i,4] <- new
        i = i + 1
      }
    }
  }

  names(pt2)[names(pt2) == "id"] <- id_var


  if(table==TRUE) (
    print(DT::datatable(pt_winsor, options = list(pageLength = 10, columnDefs = list(list(className = 'dt-center', targets = "_all"))), rownames = F, fillContainer = F))
  )

  if(table==FALSE)(
    print(knitr::kable(pt_winsor))
  )

  return(pt2)

}
