#' PT QC
#'
#' This function helps users to conduct quality control on purchase task data by
#' using the 3-Criterion Algorithm by Stein et al. (2015) to remove non-systematic data:
#' Specifically, this function identifies and removes IDs with: i) trend violations;
#' ii) excessive bounce ratios; and iii) excessive reversals in responses.
#'
#' Stein, J. S., Koffarnus, M. N., Snider, S. E., Quisenberry, A. J., & Bickel, W. K. (2015).
#' Identification and management of nonsystematic purchase task data: Toward best practice.
#' Experimental and clinical psychopharmacology, 23(5), 377.
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier (ID) as identified in the data frame.
#' @param bounce_val Expressed as a proportion, the bounce value is used as a
#' threshold to identify excessive inconsistencies in responses. The default bounce
#' value is 0.1. IDs exceeding this bounce value are removed.
#' @param rev_n The number of acceptable reversals from zero. The default number is 1.5,
#' which removes IDs with 2 or more reversals from zero.
#' @param cons_n The number of consecutive zeroes to signify a reversal from zero using
#' the {beezdemand} package. The default number of consecutive zeroes is 2.
#' @examples
#' \dontrun{
#' ## Using default values:
#' pt1 <- pt_qc(pt, id_var = "ID")
#' ## Customizing the third criterion (number of reversals):
#' ## This identifies and removes IDs with any reversals (with a reversal defined as 1 zero value).
#' pt2 <- pt_qc(pt, id_var = "ID", rev_n = 0, cons_n = 1)
#' }
#' @export

pt_qc <- function(pt, id_var, bounce_val = 0.1, rev_n = 1.5, cons_n = 2){

  # WARNING: NA values should have been changed to 0 as outlined in the `pt_prep()` function
  if(any(is.na(pt))) stop("IDs with missing values")

  pt_names <- names(pt)
  prices <- pt_names[pt_names!=id_var]
  names(pt)[names(pt) == id_var] <- "id"

  # IDENTIFY & REMOVE IDs with a trend violation
  remove.id.trend = {}
  for (id_num in pt$id){
    if ((pt[pt$id == id_num,prices[1]]>0) &
        (pt[pt$id == id_num,prices[1]] <= pt[pt$id == id_num,prices[length(prices)]]) ){
      pt <- pt[!pt[,"id"] %in% c(id_num),]
      remove.id.trend <- append(remove.id.trend,id_num)
    }
  }

  # IDENTIFY & REMOVE IDs
  remove.id.bounce <- {}
  for (id_num in pt$id){
    num.bounces <- 0
    for (j in seq(1,length(prices)-1,1)){
      if (pt[pt$id == id_num,prices[j]] < pt[pt$id == id_num,prices[j+1]]){
        num.bounces <- num.bounces + 1
      }
    }
    if (num.bounces/(length(prices)-1) > bounce_val){
      pt <- pt[!pt[,"id"] %in% c(id_num),]
      remove.id.bounce <- append(remove.id.bounce,id_num)
    }
  }

  # IDENTIFY & REMOVE IDs with reversals from zero
  pt_long <- stats::reshape(as.data.frame(pt), idvar = "id", varying = prices,
                     v.names = "y", timevar = "x", sep = "", direction = "long")

  pt_long <- pt_long[order(pt_long$id),]
  pt_long$x <- prices

  check.unsys <- beezdemand::CheckUnsystematic(dat = pt_long, deltaq = -0.01, bounce = bounce_val,
                                               reversals = rev_n, ncons0 = cons_n)

  remove.id.list <- check.unsys$ReversalsPass=="Fail"
  remove.id.reversal <- check.unsys$id[check.unsys$ReversalsPass=="Fail"]
  keep.id.list <- check.unsys$id[!remove.id.list]

  pt_long2 <- pt_long[!is.na(match(pt_long$id,keep.id.list)),]

  pt_wide <- stats::reshape(as.data.frame(pt_long2), idvar = "id", v.names = "y", timevar = "x", direction = "wide")

  names(pt_wide) <- gsub("y.", "", names(pt_wide))

  if(length(remove.id.trend)==0) (remove.id.trend <- "NULL")
  if(length(remove.id.bounce)==0) (remove.id.bounce <- "NULL")
  if(length(remove.id.reversal)==0) (remove.id.reversal <- "NULL")

  cat(" IDs with a trend violation: ",remove.id.trend,
      "\n","IDs with a bounce violation: ", remove.id.bounce,
      "\n","IDs with a reversal violation: ", remove.id.reversal,"\n")

  names(pt_wide)[names(pt_wide) == "id"] <- id_var
  return(pt_wide)
}
