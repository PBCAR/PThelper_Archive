#' PT PREP
#'
#' This function helps users prep their data by i) assigning zero values for remaining
#' items not administered; ii) identifying missing data by ID; and (optional)
#' iii) re-assigning the maximum value allowed at any price point.
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier (ID) as found in the data frame.
#' @param max_val Optional identification of a maximum allowed response for any given price point.
#' @examples
#' \dontrun{
#' pt <- pt_prep(pt, id_var = "ID", max_val = 99)
#' }
#' @export


pt_prep <- function(pt, id_var, max_val = NULL) {

  names(pt)[names(pt) == id_var] <- "id"

  dupe_id <- unique(pt$id)

  if(length(dupe_id)!=length(pt$id)) stop("Duplicate IDs detected!")

  ### FUNCTION WITH MAX VALUE
  if(!is.null(max_val)){

    # ASSIGN zero values
    for (id_num in pt$id){
      if (pt[pt[,"id"]==id_num,][max(which(!is.na(pt[pt[,"id"]==id_num,])))] == 0){
        pt[pt[,"id"]==id_num,][is.na(pt[pt[,"id"]==id_num,])] <- 0
      }
    }

    # RE-CODE values > the max value as == to the max value
    pt[,2:ncol(pt)][pt[,2:ncol(pt)] > max_val] <- max_val

    # IDENTIFY & REMOVE IDs with missing values
    missing.id <- {}
    for (id_num in pt$id){
      if (sum(is.na(pt[pt[,"id"]==id_num,])) > 0){
        missing.id <- append(missing.id, id_num)
      }
    }

    pt2 <- pt[!(pt$id %in% missing.id),]

    ### FUNCTION WITHOUT MAX VALUE
  } else if(is.null(max_val)){

    # ASSIGN zero values
    for (id_num in pt$id){
      if (pt[pt[,"id"]==id_num,][max(which(!is.na(pt[pt[,"id"]==id_num,])))] == 0){
        pt[pt[,"id"]==id_num,][is.na(pt[pt[,"id"]==id_num,])] <- 0
      }
    }

    # IDENTIFY & REMOVE IDs with missing values
    missing.id <- {}
    for (id_num in pt$id){
      if (sum(is.na(pt[pt[,"id"]==id_num,])) > 0){
        missing.id <- append(missing.id, id_num)
      }
    }

    pt2 <- pt[!(pt$id %in% missing.id),]
  }

  if(length(missing.id)!=0){
    cat(" IDs with Missing Values: ", missing.id, "\n")
  } else if(length(missing.id)==0) {
    cat(" IDs with Missing Values: ","NULL","\n")
  }

  names(pt2)[names(pt2) == "id"] <- id_var
  return(pt2)

}
