#' PRICE PREP
#'
#' This function helps users efficiently rename their variables as prices, which is essential
#' to processing purchase task data. At the same time, it will return only the variables required
#' for purchase task processing -- the unique identifier and the purchase task variables themselves,
#' making it easy for non-R users to select the variables they need.
#'
#' @param pt A data frame which includes the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier (ID) as identified in the data frame.
#' @param vars The names of variables which make up the purchase task.
#' @param prices The prices associated with each `vars` **in the same order**.
#' @examples
#' \dontrun{
#' pt <- price_prep(pt, id_var = "ID",
#' vars = c("cpt0","cpt1","cpt2","cpt3","cpt4","cpt5","cpt6",
#' "cpt7","cpt8","cpt9","cpt10"),
#' prices = c("0","0.05","0.10","0.25","0.50","0.75","1","2","5","10"))
#' }
#' @export

price_prep <- function(pt, id_var, vars, prices) {

  if(length(vars)!=length(prices)) stop("Number of variables and prices do not match!")

    pt <- pt[c(id_var,vars)]
    colnames(pt) <- c(id_var,prices)

  return(pt)
}

