utils::globalVariables(c("Elasticity","Q0_derived","Omax_derived","Pmax_derived",
                         "alpha_sens","id","x","x1","x2","y","y2"))

#' ELASTICITY CURVE
#'
#' Models the elasticity curve, and calculates other values of interest such as
#' derived omax and pmax values, by fitting the purchase task data using the
#' {beezdemand} package. The overall sample curve is visualized, with the option
#' to visualize each individual curve on the same plot, identifying thoes with
#' extreme sensitivity to price (i.e. high elasticity values > a z-score of 3).
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier as identified in the data frame.
#' @param k_span The k-values to test for curve fitting, with the default set to c(2,3,4)
#' @param eq_type Equation type to fit elasticity curve using the {beezdemand} package.
#' One of c("hs","koff"), with "hs" for Hursh and Silberbeg (2008) or Koffarnus,
#' Franck, Stein, and Bickel (2015).
#' @param agg_type Aggregate type to fit elasticity curve using the {beezdemand} package.
#' One of c("mean","pooled"), with the default set to "mean".
#' @param id_curve If set to TRUE, each individual curve is visualized on a single plot, with
#' those sensitive to price increases (i.e. show more elasticity) highlighted. Specifically,
#' highlighted individuals are those greater than 3 standard deviations away from the mean (z-score > 3).
#' @param id_label If set to TRUE, the ID labels of those identified as sensitive to price increases
#' are labelled on the plot. If FALSE, then the ID labels are provided as a print out in the console.
#' By default,id_label is set to TRUE.
#' @examples
#' \dontrun{
#' ## Basic calculation of elasticity curve and derived values using default settings
#' pt2 <- elasticity_curve(pt, id_var = "ID")
#' }
#' @export

elasticity_curve <- function(pt, id_var, k_span = c(2,3,4), eq_type = "koff", agg_type = "mean", id_curve = FALSE, id_label = TRUE) {

  pt_names <- names(pt)
  prices <- pt_names[pt_names!=id_var]
  names(pt)[names(pt) == id_var] <- "id"

  pt_long <- stats::reshape(as.data.frame(pt), idvar = "id",
                     varying = prices,
                     v.names = c("y"), timevar = c("x"), sep = "", direction = "long")

  pt_long <- pt_long[order(pt_long$id),]
  pt_long$x <- prices

  pt_empirical <- beezdemand::GetEmpirical(dat = pt_long)
  colnames(pt_empirical) <- c("id","Intensity","BP0","BP1","Omax","Pmax")

  R2.val.k <- {}

  for (k_value in k_span){
    mean.curve <- beezdemand::FitCurves(dat = pt_long, equation = eq_type,
                                        k = k_value, agg = agg_type)

    R2.val.k <- append(R2.val.k, mean.curve$R2)
  }


  # SHARED K-VALUE used for Individual Elasticity calculations
  # Ties are broken by choosing the lower k-value

  k.value.final <- min(k_span[R2.val.k == max(R2.val.k)])

  mean.curve <- beezdemand::FitCurves(dat = pt_long, equation = eq_type,
                                      k = k.value.final, agg = agg_type,  detailed = T)

  mean.curve.final <- mean.curve[["dfres"]]
  mean.curve.final <- mean.curve.final[,c("Q0d","Alpha","R2","Omaxd","Pmaxd")]

  colnames(mean.curve.final) <- c("Q0_derived","Elasticity","R2","Omax_derived","Pmax_derived")

  part.curve <- beezdemand::FitCurves(dat = pt_long, equation = eq_type,
                                      k = k.value.final, agg = NULL)

  spec.curve <- part.curve[,c("id","Q0d","K","Alpha","R2","Omaxd","Pmaxd","Notes")]
  colnames(spec.curve) <- c("id","Q0_derived","k","Elasticity","R2","Omax_derived","Pmax_derived","Notes")

  pt_results <- merge(pt_empirical, spec.curve, by = "id", all = T)
  pt_results <- pt_results[order(pt_results$id),]

  # CREATE proper breakpoint variable
  pt_results$Breakpoint <- pt_results$BP0
  for (id_num in pt$id){
    pt.sum <- sum(pt[pt$id==id_num,prices], na.rm = FALSE)
    last.amount <- pt[pt$id==id_num,length(prices)+1]
    if(is.na(pt_results$BP0[pt_results$id==id_num]) & (pt.sum==0)){
      pt_results$Breakpoint[pt_results$id==id_num] <- as.numeric(min(prices))
    } else if (is.na(pt_results$BP0[pt_results$id==id_num]) & (last.amount>0)){
      pt_results$Breakpoint[pt_results$id==id_num] <- as.numeric(prices)[length(prices)]+1
    }
  }

  # REDEFINE breakpoints where there were reversals to 1st 0 consumption reached
  check.unsys2 <- beezdemand::CheckUnsystematic(dat = pt_long, deltaq = -0.01, bounce = 0.1, reversals = .01, ncons0 = 1)
  one.rev.list <- check.unsys2[check.unsys2$ReversalsPass=="Fail",]$id
  one.rev.list <- one.rev.list[one.rev.list %in% pt$id]
  if(length(one.rev.list!=0)){
    for (id_num in one.rev.list){
      cons.vals <- pt[pt$id==id_num,]
      for (price in prices){
        if (cons.vals[,price]==0){
        pt_results[pt_results$id==id_num,]$Breakpoint <- as.numeric(price)
        break
        }
      }
    }
  }

  if(length(one.rev.list)==0) (one.rev.list <- "NULL")

  pt_results <- merge(pt, pt_results, by = "id")
  pt_results <- pt_results[,c("id",prices,"Q0_derived", "R2", "Omax_derived", "Pmax_derived",
                              "Breakpoint","Elasticity","Intensity","Omax","Pmax","Notes")]

  cat(" Selected k-value: ", k.value.final, "\n")
  cat(" IDs with changes to Breakpoint: ", one.rev.list, "\n")

  zero_val <- (pt_results[,prices[1]]==0)|(pt_results[,prices[2]]==0)
  zero_id <- pt_results[zero_val,]$id

  # IDENTIFY IDs who had a 0 value in one or both of their first 2 responses

  if(length(zero_id>0)){
    pt_results[(pt_results$id %in% zero_id),][,c("Q0_derived","Omax_derived","Pmax_derived","Elasticity","Omax","Pmax")] <- NA

    cat(" IDs with a zero consumption value in the first and/ or second price: ", zero_id,sep=" ","\n")

  } else if(length(zero_id)==0) {

    cat(" IDs with a zero consumption value in the first and/ or second price: NULL \n")

    }

  ##### ----- ELASTICITY CURVE OF SAMPLE

  # AGG TYPE - Capitalize first letter for plot
  splt <- strsplit(agg_type, "")[[1]]
  splt1 <- toupper(splt[1])
  splt <- paste0(splt[-1],collapse = "")

  agg_type <- paste0(splt1,splt)

  # SUMMARIZE Sample curve data
  dat_pt <- data.frame(x1 = mean.curve$dfres$Pmaxd,
                       x2 = mean.curve$dfres$Pmaxd,
                       y1 = min(as.numeric(prices)),
                       y2_koff =  mean.curve$dfres$Q0d * 10^(mean.curve$dfres$K * (exp(-mean.curve$dfres$Alpha * mean.curve$dfres$Q0d * mean.curve$dfres$Pmaxd) - 1)),
                       y2_hs = 10^((log(mean.curve$dfres$Q0d)/log(10)) + mean.curve$dfres$K * (exp(-mean.curve$dfres$Alpha * mean.curve$dfres$Q0d * mean.curve$dfres$Pmaxd) - 1)))

  # ASSIGN Y2 value based on equation type selected
  dat_pt$y2[eq_type=="koff"] <- dat_pt$y2_koff
  dat_pt$y2[eq_type=="hs"] <- dat_pt$y2_hs

  # EXTRACT Data from {beezdemand} `FitCurves()` object
  point_dat <- mean.curve$adfs$mean
  est_dat <- mean.curve$newdats$mean

  if(min(prices)==0) {
    point_dat$x[point_dat$x==0] <- 0.0001
    est_dat$x[est_dat$x==0] <- 0.0001
  }

  # MEAN CURVE PLOT
  mean_curve_plot <- ggplot2::ggplot(point_dat, ggplot2::aes(x=x,y=y)) +
    ggplot2::geom_line(est_dat, mapping = ggplot2::aes(x = x, y = y), colour = "#999999", size = 1.5) +
    ggplot2::geom_segment(dat_pt, mapping = ggplot2::aes(x = x1, y = min(point_dat$y),
                                                         xend = x2, yend = y2), show.legend = F, size = 0.75, linetype = "dashed") +
    ggplot2::geom_point(size = 3, show.legend=F, colour = "#000000", stroke = 0.5) +
    ggplot2::scale_x_log10(breaks=c(0.0001,  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                           labels=c(paste0(min(prices)),  0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
    ggplot2::scale_y_log10(breaks=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                           labels=c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
    ggplot2::theme_classic() + ggplot2::xlab("Price (Log)") + ggplot2::ylab("Consumption (Log)") +
    ggplot2::ggtitle(paste0(agg_type," Aggregate Demand Curve & Derived Values")) +
    ggplot2::geom_text(data = mean.curve.final,
                       ggplot2::aes(label = paste0("\n Elasticity: ", round(Elasticity, digits = 4),
                                                   "\n Q0: ", round(Q0_derived, digits = 2),
                                                   "\n Pmax: ", round(Pmax_derived, digits = 2),
                                                   "\n Omax: ", round(Omax_derived, digits = 2)),
                                    x = Inf, y = Inf, hjust = 1, vjust = 1),
                       size = 5, fontface = "bold", show.legend = F) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold"),
                   plot.subtitle = ggplot2::element_text(size = 15, face = "italic"),
                   axis.title = ggplot2::element_text(size = 15, face = "bold"),
                   axis.text = ggplot2::element_text(size = 13),
                   axis.line = ggplot2::element_line(colour = "black", size = 1))

  ##### ----- ELASTICITY CURVE OF EACH ID
  if(id_curve==TRUE) {

    pt_long$remove <- ifelse(pt_long$id %in% zero_id,"remove","keep")
    pt_long <- pt_long[(pt_long$remove=="keep"),]

    part.curve2 <- beezdemand::FitCurves(dat = pt_long, equation = eq_type, k = k.value.final, agg = NULL, detailed = T)

    id_dat_pt <- part.curve2$dfres

    if(eq_type=="koff") {
      id_dat_pt$y2 <-  id_dat_pt$Q0d * 10^(id_dat_pt$K * (exp(-id_dat_pt$Alpha * id_dat_pt$Q0d * id_dat_pt$Pmaxd) - 1))

    } else if(eq_type=="hs") {
      id_dat_pt$y2 <- 10^((log(id_dat_pt$Q0d)/log(10)) + id_dat_pt$K * (exp(-id_dat_pt$Alpha * id_dat_pt$Q0d * id_dat_pt$Pmaxd) - 1))
    }

    id_est_dat <- do.call(rbind.data.frame, part.curve2$newdats)

    if(min(prices)==0) {
      id_est_dat$x[id_est_dat$x==0] <- 0.0001
    }

    # Those more sensitive to price (i.e. those with greater elasticity) are
    # highlighted, as defined as 3+ standard deviations away from the mean.

    id_dat_pt$z_alpha <- abs(scale(id_dat_pt$Alpha))

    # IDENTIFY those more sensitive to price:
    id_alpha_sens <- id_dat_pt$id[id_dat_pt$z_alpha>3]

    id_est_dat$id <- rownames(id_est_dat)
    id_est_dat$id <- strsplit(id_est_dat$id,".", fixed = T)
    id_est_dat$id <- lapply(id_est_dat$id, `[`, 1)
    id_est_dat$id <- as.character(id_est_dat$id)

    id_est_dat$alpha_sens <- ifelse(id_est_dat$id %in% id_alpha_sens, "highlight", "none")

    alpha_sens_labels_x <- stats::aggregate(id_est_dat$x[id_est_dat$alpha_sens=="highlight"], by = list(id_est_dat$id[id_est_dat$alpha_sens=="highlight"]), FUN = max)
    names(alpha_sens_labels_x)[names(alpha_sens_labels_x)=="Group.1"] <- "id"

    alpha_sens_labels_y <- stats::aggregate(id_est_dat$y[id_est_dat$alpha_sens=="highlight"], by = list(id_est_dat$id[id_est_dat$alpha_sens=="highlight"]), FUN = min)
    names(alpha_sens_labels_y)[names(alpha_sens_labels_y)=="Group.1"] <- "id"
    names(alpha_sens_labels_y)[names(alpha_sens_labels_y)=="x"] <- "y"

    alpha_sens_labels <- merge(alpha_sens_labels_x, alpha_sens_labels_y, by = "id")
    alpha_sens_labels$alpha_sens <- "highlight"

    # ID_CURVE PLOT
    if(id_label==TRUE) {
    id_curve_plot <- ggplot2::ggplot(id_est_dat, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_line(id_est_dat[(id_est_dat$alpha_sens=="none"),],
                         mapping = ggplot2::aes(x = x, y = y, group = id, colour = alpha_sens),show.legend = F) +
      ggplot2::geom_line(id_est_dat[(id_est_dat$alpha_sens=="highlight"),],
                         mapping = ggplot2::aes(x = x, y = y, group = id, colour = alpha_sens), show.legend = F) +
      ggrepel::geom_label_repel(alpha_sens_labels, mapping = ggplot2::aes(x = x, y = y, label = id, colour = alpha_sens),
                                min.segment.length = 0, position = ggrepel::position_nudge_repel(x = 0.5), max.overlaps = Inf,
                                segment.size = 0.2, hjust = 1, show.legend = F) +
      ggplot2::scale_colour_manual(values = c("#000000","#BEBEBE")) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::scale_x_log10(breaks = c(0.0001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                             labels = c(paste0(min(prices)), 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                             expand = c(0.01,0.01,0.1,0.1)) +
      ggplot2::scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                             labels = c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
      ggplot2::theme_classic() + ggplot2::xlab("Price (Log)") + ggplot2::ylab("Consumption (Log)") +
      ggplot2::labs(title = paste0(agg_type," Aggregate Demand Curve & Derived Values"),
                    subtitle = "Labelled IDs with Elasticity z-scores > 3") +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold"),
                     plot.subtitle = ggplot2::element_text(size = 15, face = "italic"),
                     axis.title = ggplot2::element_text(size = 15, face = "bold"),
                     axis.text = ggplot2::element_text(size = 13),
                     axis.line = ggplot2::element_line(colour = "black", size = 1))

    } else if(id_label==FALSE) {

      cat(" IDs with extreme price sensitivity (Elasticity values > z-score of 3): ", id_alpha_sens, "\n")

      id_curve_plot <- ggplot2::ggplot(id_est_dat, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_line(id_est_dat[(id_est_dat$alpha_sens=="none"),],
                           mapping = ggplot2::aes(x = x, y = y, group = id, colour = alpha_sens),show.legend = F) +
        ggplot2::geom_line(id_est_dat[(id_est_dat$alpha_sens=="highlight"),],
                           mapping = ggplot2::aes(x = x, y = y, group = id, colour = alpha_sens), show.legend = F) +
        ggplot2::scale_colour_manual(values = c("#000000","#BEBEBE")) +
        ggplot2::scale_x_log10(breaks = c(0.0001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                               labels = c(paste0(min(prices)), 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                               expand = c(0.01,0.01,0.01,0.01)) +
        ggplot2::scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                               labels = c(0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
        ggplot2::theme_classic() + ggplot2::xlab("Price (Log)") + ggplot2::ylab("Consumption (Log)") +
        ggplot2::labs(title = paste0(agg_type," Aggregate Demand Curve & Derived Values"),
                      subtitle = "Highlighted Curves with Elasticity z-scores > 3") +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold"),
                       plot.subtitle = ggplot2::element_text(size = 15, face = "italic"),
                       axis.title = ggplot2::element_text(size = 15, face = "bold"),
                       axis.text = ggplot2::element_text(size = 13),
                       axis.line = ggplot2::element_line(colour = "black", size = 1))

    }

    graphics::par(ask=F)
    print(mean_curve_plot)
    graphics::par(ask=T)
    print(id_curve_plot)
    graphics::par(ask=F)

  } else if(id_curve==FALSE) {

    print(mean_curve_plot)
  }
  names(pt_results)[names(pt_results) == "id"] <- id_var

  return(pt_results)

}
