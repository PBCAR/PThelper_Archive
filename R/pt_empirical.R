#' `pt_empirical()`
#'
#' This function extracts the empirical demand indices (Intensity, Breakpoint, Omax, and Pmax) from the purchase task.
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier as identified in the data frame.
#' @examples
#'
#' ##### Load Data
#' data("cpt_data")
#'
#' ##### Prep Data
#' pt <- price_prep(cpt_data, id_var = "ID", vars = c(paste0("cpt",1:15)),
#' prices = c("0","0.05","0.10","0.20","0.30","0.40","0.50", "0.75","1","2","3","4","5","7.5","10"))
#'
#' pt2 <- pt_prep(pt, id_var = "ID", remove0 = TRUE, max_val = 99)
#' pt3 <- pt_qc(pt2, id_var = "ID", type = "partial", bounce_type = "p2p")
#'
#' ##### Function Example
#' pt4 <- pt_empirical(pt3$data,id_var = "ID")
#'
#' @return A data frame
#' @export

pt_empirical <- function(pt, id_var){

  pt_orig <- pt
  pt_names <- names(pt)
  prices <- pt_names[pt_names!=id_var & pt_names!="Q0" & pt_names!="Alpha" & pt_names!="R2"]
  names(pt)[names(pt) == id_var] <- "id"

  pt <- pt[c("id",prices)]

  ### --- Calculate Empirical Q0 (Intensity) and BP

  pt$q0 <- as.numeric(pt[,prices[1]])
  pt$bp <- apply(pt[ ,prices], 1, function(x) {names(x)[minval(which(x == 0))] })
  pt$bp <- ifelse(pt[,prices[length(prices)]]!=0,as.numeric(prices[length(prices)])+1,as.numeric(pt$bp))

  ### RESHAPE to long to calculate Empirical Omax & Pmax

  pt_long <- stats::reshape(as.data.frame(pt), idvar = "id",
                            varying = prices,
                            v.names = c("q"), timevar = c("c"), sep = "", direction = "long")

  pt_long <- pt_long[order(pt_long$id),]
  pt_long$c <- as.numeric(prices)
  pt_long$expenditure <- pt_long$c*pt_long$q

  ### --- Calculate Empirical OMAX and PMAX

  pt_all <- data.frame(id = NULL, omax = NULL, pmax = NULL)

  for(id_num in pt$id){

    pt_dat_i <- pt_long[(pt_long$id == id_num),]

    omax_i <- max(pt_dat_i$expenditure)
    colnames(pt_summ_dat) <- c("id","omax")

    pmax_i <- min(pt_dat_i$c[pt_dat_i$expenditure==omax_i])

    dat_i <- data.frame(id = id_num, omax = omax_i, pmax = pmax_i)

    pt_all <- rbind(pt_all,dat_i)
  }

  ### --- Calculate Empirical OMAX and PMAX: NOT CORRECT FOR THOSE WITH ZERO CONSUMPTION

  # pt_omax <- stats::aggregate(pt_long[c("expenditure")], by = list(id = pt_long[,"id"]), function(x) max(x, na.rm = T))
  # pt_omax$omax <- pt_omax$expenditure
  #
  # pt_long <- merge(pt_long,pt_omax[c("id","omax")], by = "id", all.x = T)
  # pt_long$max_price <- ifelse(pt_long$omax==pt_long$expenditure,min(pt_long$c[pt_long$expenditure==pt_long$omax]),NA)
  # pt_pmax <- stats::aggregate(pt_long[c("max_price")], by = list(id = pt_long[,"id"]), function(x) max(x, na.rm = T))
  # pt_pmax$pmax <- pt_pmax$max_price
  # pt_max <- merge(pt_omax[c("id","omax")],pt_pmax[c("id","pmax")], by = "id", all.x = T)

  ### MERGE INDICATORS in final data set
  pt_final <- merge(pt[c("id","q0","bp")],pt_all, by = "id", all.x = T)
  colnames(pt_final) <- c(id_var,"Intensity","Breakpoint","Omax","Pmax")

  pt_final <- merge(pt_orig, pt_final, by = id_var, all.x = T)

  return(pt_final)

}
