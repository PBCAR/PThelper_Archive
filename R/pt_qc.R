#' PT QC
#'
#' This function helps users to conduct quality control on purchase task data to remove non-systematic data. Specifically, this function
#' identifies and removes IDs with: i) trend violations; ii) excessive bounce ratios; and iii) excessive reversals in responses. Quality
#' control options follow the proposed 3-criterion method by Stein et al. (2015), but also allow for customization, as well as use of
#' another option for bounce criterion used at PBCAR.
#'
#' Stein, J. S., Koffarnus, M. N., Snider, S. E., Quisenberry, A. J., & Bickel, W. K. (2015).
#' Identification and management of nonsystematic purchase task data: Toward best practice.
#' Experimental and clinical psychopharmacology, 23(5), 377.
#'
#' @param pt A list consisting of two data frames: "data" which consists of the `id_var` and purchase task variables, and the "qc_data" which provides details on
#' the results of the quality control for all IDs.
#' @param id_var The name of the unique identifier (ID) as identified in the data frame.
#' @param delta_q The log10-unit reduction in consumption from the first to last price. The default is set to 0.025 (suggested by Stein et al., 2015),
#' with values lower than 0.025 deemed non-systematic.
#' @param bounce_type The type of bounce criterion applied, one of c("initial","p2p"). The default is "initial", which follows the criterion put forth by
#' Stein et al. (2015). This method defines what constistutes a `jump`, with all subsequent prices in the purchase task being compared to the consumption
#' at the first price. The "p2p" option is a proposed alternative, which considers any increases in consumption from one price to the next to be a `jump`
#' in consumption. Both methods make use of the `bounce_val` which will only exclude participants above a set threshold of jumps.
#' @param jump Expressed as a proportion, the jump value is the percent increase in consumption at first price used to define an excessive increase in
#' consumption. The default is 0.25 (25% as suggested by Stein et al., 2015), meaning that any consumption 25% higher than consumption at first price
#' would be considered an excessive jump in consumption.
#' @param bounce_val Expressed as a proportion, the bounce value is used as a threshold to identify excessive inconsistencies in responses. The
#' default bounce value is 0.1. IDs exceeding this bounce value are removed.
#' @param rev_n The number of acceptable reversals from zero, one of c(0,1). The default number is 0, meaning no reversals from zero are allowed.
#' @param cons_0 The number of consecutive zeroes to signify a reversal from zero, one of c(1,2). The default is 1. Previously named `cons_n`.
#' the {beezdemand} package. The default number of consecutive zeroes is 2.
#' @examples
#' \dontrun{
#' ## Using default values:
#' pt1 <- pt_qc(pt, id_var = "ID")
#' ## Customizing the third criterion (number of reversals):
#' ## This identifies and removes IDs with any reversals (with a reversal defined as 1 zero value).
#' pt2 <- pt_qc(pt, id_var = "ID", cons_n = 1)
#' }
#' @export

pt_qc <- function(pt, id_var, delta_q = 0.025, bounce_type = "initial", jump = 0.25, bounce_val = 0.1, rev_n = 0, cons_0 = 1){

  ### WARNING: NA values should have been changed to 0 as outlined in the `pt_prep()` function
  if(any(is.na(pt))) stop("IDs with missing values")

  pt_names <- names(pt)
  prices <- pt_names[pt_names!=id_var]
  names(pt)[names(pt) == id_var] <- "id"

  ##### ----- IDENTIFY & REMOVE IDs with a trend violation

  remove.id.trend = {}

  pt$dq_1 <- as.numeric(pt[,prices[1]])+0.001 ### quantity at price 1
  pt$dq_n <- as.numeric(pt[,prices[length(prices)]])+0.001 ### quantity at price n
  pt$dp_1 <- as.numeric(prices[1])+0.001 ### price 1 (intensity)
  pt$dp_n <- as.numeric(prices[length(prices)])+0.001 ## price n

  ### FORMULA: FORMULA: deltaQ = (log10(quantity at price 1) - log10(quantity at price n)) / (log10(price n) - log10(price 1))
  ### allow those with zero consumption at price 1 to pass (by making delta_q the value considered systematic)

  pt$delta_q <- (log10(pt$dq_1)-log10(pt$dq_n))/(log10(pt$dp_n)-log10(pt$dp_1))
  pt$delta_q <- ifelse(pt[,prices[1]]==0,delta_q,pt$delta_q)

  remove.id.trend <- pt$id[pt$delta_q<delta_q]


  ##### ----- IDENTIFY & REMOVE IDs with a bounce violation


  remove.id.bounce <- {}

  if(bounce_type=="initial"){

    pt$first_cons <- pt[,prices[1]]*(1+jump)
    pt$jumps <-  rowSums(pt[c(prices)] > pt$first_cons)

    pt$bounce_val <- pt$jumps/(length(prices)-1)

    remove.id.bounce <- pt$id[pt$bounce_val> bounce_val]
  }

  if(bounce_type == "p2p"){

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

  }

  ##### ----- REMOVE IDS WITH REVERSALS

  remove.id.reversal <- {}

  ### calculate number of zero-consumption responses present for each participant
  pt$zero_count <- rowSums(pt[c(prices)]==0, na.rm = T)

  ## use instead of min function to surpress warning
  minval <- function(x) {if (length(x)>0) min(x) else Inf}

  ### get breakpoint price
  pt$bp <- apply(pt[ ,prices], 1, function(x) {names(x)[minval(which(x == 0))] })

  ### get number of price items at and after bp
  ### and determine if non-zero values exist between breakpoint and end price

  pt$bp_items <- sapply(pt$bp, function(x) sum(as.numeric(prices)>=as.numeric(x), na.rm = T))
  pt$postbp_val <- sapply(seq_along(pt[,1]), function(x) {pt[,prices][x, prices[(match(pt$bp,prices)+1)][x]]})

  pt$postbp_val[as.numeric(pt$bp)==max(as.numeric(prices)) | is.na(pt$bp)] <- "NO BP"
  pt$postbp_val <- unlist(as.character(pt$postbp_val))
  pt$postbp_val[pt$postbp_val=="NO BP"] <- NA
  pt$postbp_val <- as.numeric(pt$postbp_val)

  ## need to find the number of zeroes that exist after the first breakpoint (or minus the breakpoint)
  ## this plus breakpoint will determine if there are additional reversals

  pt$final0 <- apply(pt[,prices], 1, function(x) sum(cumsum(rev(x)) == 0))

  ### THREE OPTIONS FOR REVERSALS FROM ZERO:

  ### i) No reversals allowed:
  ### zero_count =  bp_items

  if(rev_n==0){
    pt$reversals <- ifelse(pt$zero_count==pt$bp_items,FALSE,TRUE)
  }

  ### ii) 1 reversal of a single 0 allowed:
  ### zero_count = final0 + 1

  if(rev_n==1 & cons_0==1){
    pt$reversals <- ifelse(pt$zero_count==pt$final0+1,FALSE,TRUE)
  }
  ### iii) 1 reversal of two consecutive 0s allowed:
  ### zero_count = final0 + 1

  if(rev_n==1 & cons_0==2){
    pt$reversals <- ifelse(pt$zero_count==pt$final0+2 & pt$postbp_val==0,FALSE,TRUE)
  }

  remove.id.reversal <- pt$id[pt$reversals==TRUE]

  qc <- pt[c("id","delta_q","bounce_val","reversals")]

  remove.id <- c(remove.id.trend,remove.id.bounce,remove.id.reversal)

  pt <- pt[c("id",prices)]
  pt <- pt[(!pt$id %in% remove.id),]
  names(pt)[names(pt) == "id"] <- id_var

  pt_final <- list(data = as.data.frame(pt), qc_data = as.data.frame(qc))

  if(length(remove.id.trend)==0) (remove.id.trend <- "NULL")
  if(length(remove.id.bounce)==0) (remove.id.bounce <- "NULL")
  if(length(remove.id.reversal)==0) (remove.id.reversal <- "NULL")

  cat(" IDs with a trend violation: ",remove.id.trend,
      "\n","IDs with a bounce violation: ", remove.id.bounce,
      "\n","IDs with a reversal violation: ", remove.id.reversal,"\n")

  return(pt_final)
}

