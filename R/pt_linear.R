#' `pt_linear()`
#'
#' This function fits a linear demand curve in order to calculate eta. It also allows for the addition of covariates.
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier as identified in the data frame.
#' @param type The level for fitting the demand curves, one of c("overall","group","individual"). The default is "overall" which will calculate
#' overall demand for the entire data frame. For `type` "group", equation-derived demand indicators for each of the groups as identified by the `group_var`
#' are visualized. When `type` equals "individual", equation-derived demand indicators are calculated for each individual as identified by `id_var`.
#' @param zero_val The value to substitute zero values in the price and consumption data (default is 0.001).
#' @param group_var The name of the grouping variable when `type` equals "group".
#' @param co_var Optional. A vector of covariates to add to the linear model.
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
#' pt4 <- pt_linear(pt3$data, id_var = "ID", type = "individual")
#'
#' @return A ggplot2 graphical object; For `type` "individual", the original pt data frame plus the derived values for each individual is returned.
#' @export

pt_linear <- function(pt, id_var, type, zero_val = 0.001, group_var = NULL, co_var = NULL){

  pt_names <- names(pt)

  if(type == "overall" | type == "individual"){

    prices <- pt_names[pt_names!=id_var & pt_names!="Intensity" & pt_names!="Breakpoint" & pt_names!="Omax" & pt_names!="Pmax"]

  }

  if(type == "group"){

    prices <- pt_names[pt_names!=id_var & pt_names!=group_var & pt_names!="Intensity" & pt_names!="Breakpoint" & pt_names!="Omax" & pt_names!="Pmax"]
    names(pt)[names(pt) == group_var] <- "group"

  }

  names(pt)[names(pt) == id_var] <- "id"

  ### Calculate acceptable non-zero value to replace 0 price points for plotting
  zero_conv <- (as.numeric(prices[2])-as.numeric(prices[1]))/10

  ### Calculate log10 label breaks
  log_labels <- 10^(log10(10^ceiling(log10(zero_conv))):100 * 1)


  pt_long <- stats::reshape(as.data.frame(pt), idvar = "id",
                            varying = prices,
                            v.names = c("q"), timevar = c("c"), sep = "", direction = "long")

  pt_long <- pt_long[order(pt_long$id),]
  pt_long$c <- as.numeric(prices)

  if(is.null(co_var)){
    equation <- "log10(q) ~ log10(c)"
  }

  if(!is.null(co_var)){
    co_eq <- paste(co_var, collapse = " + ")
    equation <- paste0("log10(q) ~ log10(c) + ", co_eq)
  }

  ### --- OVERALL ETA

  if(type=="overall"){

  ### grab mean values of q for every price (c); then calculate max value minus min value
  pt_mean <- stats::aggregate(pt_long[c("q")], by = list(c = pt_long[,"c"]), function(x) mean(x, na.rm = T))
  pt_mean$c[pt_mean$c==0] <- zero_val
  pt_mean$q[pt_mean$q==0] <- zero_val

  pt_mod <- stats::lm(equation, data = pt_mean)

  eta <- 10^as.numeric(stats::coef(pt_mod)[2])
  rsquared <- summary(pt_mod)$r.squared

  pt_mean$pred <- stats::predict(pt_mod)
  pt_mean$pred[pt_mean$pred==0] <- zero_val
  pt_mean$pred[pt_mean$pred<0] <- NA
  pt_mean$q <- log10(pt_mean$q)

  pt_plot <- ggplot2::ggplot(pt_mean, ggplot2::aes(x = c, y = q)) +
    ggplot2::geom_line(ggplot2::aes(y = pred), lineend = "round", linewidth = 2, colour = "#999999") +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_y_log10(breaks = c(log_labels), labels = c(log_labels)) +
    ggplot2::scale_x_log10(breaks = c(log_labels), labels = c(log_labels)) +
    ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") + ggplot2::ylab("Consumption (Log) \n") +
    ggplot2::ggtitle("Mean Demand Curve") +
    ggplot2::geom_text(x = Inf, y = Inf, ggplot2::aes(label = paste0(
                         "\n \u03B7: ", signif(eta),"     ",
                         "\n R\u00b2: ", signif(as.numeric(rsquared)),"     ")), hjust = 1, vjust = 1,
                       size = 7, fontface = "bold", show.legend = F) +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 25, face = "bold", hjust = 0.5),
                   axis.title = ggplot2::element_text(size = 22, face = "bold"),
                   axis.text = ggplot2::element_text(size = 19),
                   axis.ticks.length = ggplot2::unit(2,"mm"),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(linewidth = 1.5))

  }

  ### --- GROUP ELASTICITY

  if(type == "group"){

    ### grab mean values of y for every price (x) by group; then calculate max value minus min value
    pt_group_mean <- stats::aggregate(pt_long[c("q")], list(c = pt_long[,"c"], group = pt_long[,"group"]), function(x) mean(x, na.rm = T))

    pt_group_mean$c[pt_group_mean$c==0] <- zero_val
    pt_group_mean$q[pt_group_mean$q==0] <- zero_val
    pt_group_mean$group <- as.factor(pt_group_mean$group)

    ### Create loop for each group
    group_uniq <- unique(pt_group_mean$group)

    c_est <- c(seq(zero_val,as.numeric(prices[length(prices)]), zero_conv))
    pt_group_fit <- data.frame(group = NULL, c = NULL, pred = NULL)
    pt_group_elast <- data.frame(group = NULL, eta = NULL, r2 = NULL)

    for(group_u in group_uniq){

      pt_g <- pt_group_mean[(pt_group_mean$group == group_u),]

      pt_mod_g <- stats::lm(equation, data = pt_g)

      pt_elast <- data.frame(group = group_u, eta = 10^as.numeric(stats::coef(pt_mod_g)[2]), r2 = summary(pt_mod_g)$r.squared)
      pt_group_pred <- data.frame(c = c_est)

      suppressWarnings({
        pt_group_pred$pred <- stats::predict(pt_mod_g, pt_group_pred)
      })

      pt_group_pred$group <- group_u

      pt_group_fit <- rbind(pt_group_fit,pt_group_pred)
      pt_group_elast <- rbind(pt_group_elast,pt_elast)

    }

    pt_group_fit$pred[pt_group_fit$pred==0] <- zero_val
    pt_group_fit$pred[pt_group_fit$pred<0] <- NA
    pt_group_mean$q <- log10(pt_group_mean$q)

    pt_plot <- ggplot2::ggplot(pt_group_mean, ggplot2::aes(x = c, y = q, group = group, colour = group)) +
      ggplot2::geom_line(pt_group_fit, mapping = ggplot2::aes(y = pred), lineend = "round", alpha = 1, linewidth = 2) +
      ggplot2::geom_point(alpha = 0.8, size = 3.5) +
      ggplot2::scale_y_log10(breaks = c(zero_val,log_labels), labels = c(zero_val,log_labels)) +
      ggplot2::scale_x_log10(breaks = c(zero_val,log_labels), labels = c(zero_val,log_labels)) +
      ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") + ggplot2::ylab("Consumption (Log) \n") +
      ggplot2::ggtitle("Mean Demand Curve") +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 25, face = "bold", hjust = 0.5),
                     axis.title = ggplot2::element_text(size = 22, face = "bold"),
                     axis.text = ggplot2::element_text(size = 19),
                     axis.ticks.length = ggplot2::unit(2,"mm"),
                     strip.background = ggplot2::element_blank(),
                     strip.text = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(linewidth = 1.5),
                     legend.position = "top",
                     legend.title = ggplot2::element_text(size = 17, face = "bold", vjust = 0.5),
                     legend.text = ggplot2::element_text(size = 15, vjust = 0.5)) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = group_var, override.aes = list(alpha = 1, linewidth = 5)))

    }

  if(type == "individual"){

    pt_elast <- data.frame(id = NULL, Eta = NULL, R2 = NULL)

    pt_long$c[pt_long$c==0] <- zero_val
    pt_long$q[pt_long$q==0] <- zero_val

    for(id_num in pt$id){

      pt_i <- pt_long[(pt_long$id == id_num),]

      pt_mod <- stats::lm(equation, data = pt_i)

      eta_i <- 10^as.numeric(stats::coef(pt_mod)[2])
      r2_i <- summary(pt_mod)$r.squared

      id_dat <- data.frame(id = id_num, Eta = eta_i, R2 = r2_i)

      pt_elast <- rbind(pt_elast,id_dat)

    }

  }

  message(rlang::format_error_bullets(c(i = "\u03B7 represents unit elasticity (the % decrease in consumption associated with a 1% increase in price).")))

  if(type == "overall" | type == "group"){
    ### Suppress warning about only 1 observation in the facet_grid
    suppressWarnings({
      print(pt_plot)
    })
  }


  if(type == "individual"){

    pt_final <- merge(pt[c("id",pt_names[pt_names!=id_var])], pt_elast, by = "id", all.x = T)
    names(pt_final)[names(pt_final) == "id"] <- id_var
    return(pt_final)
  }

}
