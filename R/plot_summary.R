utils::globalVariables(c("price","group","label"))

#' PLOT SUMMARY
#'
#' This function helps users to inspect the price-level purchase task data by
#' visualizing the mean and standard error of both consumption and expenditure.
#'
#' @param pt A data frame consisting of the purchase variables for each `id_var`
#' @param id_var The name of the unique identifier (ID) as found in the data frame.
#' @examples
#' \dontrun{
#' plot_summary(pt, id_var = "ID")
#' }
#' @export

plot_summary <- function(pt, id_var) {

  pt_names <- names(pt)
  prices <- pt_names[pt_names!=id_var]
  names(pt)[names(pt) == id_var] <- "id"

  pt_long <- stats::reshape(as.data.frame(pt), idvar = "id", varying = prices,
                     v.names = "consume", timevar = "price", sep = "", direction = "long")

  pt_long <- pt_long[order(pt_long$id),]
  pt_long$price <- prices

  se <- function(x) sqrt(stats::var(x,na.rm = T)/length(x))


  # SUMMARY STATISTICS: Original

  pt_consume_mean <- stats::aggregate(pt_long$consume, by = list(pt_long$price), FUN = mean)
  pt_consume_se <- stats::aggregate(pt_long$consume, by = list(pt_long$price), FUN = se)

  names(pt_consume_mean)[names(pt_consume_mean)=="x"] <- "mean"
  names(pt_consume_mean)[names(pt_consume_mean)=="Group.1"] <- "price"

  names(pt_consume_se)[names(pt_consume_se)=="x"] <- "se"
  names(pt_consume_se)[names(pt_consume_se)=="Group.1"] <- "price"

  pt_consume <- merge(pt_consume_mean, pt_consume_se , by = "price")
  pt_consume$price <- as.numeric(pt_consume$price)
  pt_consume$group <- "Consumption"

  # expenditure calculation:
  pt_long$spend <- as.numeric(pt_long$price)*pt_long$consume

  pt_spend_mean <- stats::aggregate(pt_long$spend, by = list(pt_long$price), FUN = mean)
  pt_spend_se <- stats::aggregate(pt_long$spend, by = list(pt_long$price), FUN = se)

  names(pt_spend_mean)[names(pt_spend_mean)=="x"] <- "mean"
  names(pt_spend_mean)[names(pt_spend_mean)=="Group.1"] <- "price"

  names(pt_spend_se)[names(pt_spend_se)=="x"] <- "se"
  names(pt_spend_se)[names(pt_spend_se)=="Group.1"] <- "price"

  pt_spend <- merge(pt_spend_mean, pt_spend_se, by = "price")
  pt_spend$price <- as.numeric(pt_spend$price)
  pt_spend$group <- "Expenditure"

  pt_summ  <- rbind(pt_consume, pt_spend)

  pt_anno <- data.frame(
    label = c(paste0("Max Consumption: ", round(max(pt_summ$mean[pt_summ$group=="Consumption"]), digits = 1)),
              paste0("Max Expenditure: ", round(max(pt_summ$mean[pt_summ$group=="Expenditure"]), digits = 1))),
    group = c("Mean +/- Std Error of Consumption","Mean +/- Std Error of Expenditure"),
    price = c(pt_summ$price[pt_summ$group=="Consumption" & pt_summ$mean==max(pt_summ$mean[pt_summ$group=="Consumption"])],
              pt_summ$price[pt_summ$group=="Expenditure" & pt_summ$mean==max(pt_summ$mean[pt_summ$group=="Expenditure"])]),
    mean = c(max(pt_summ$mean[pt_summ$group=="Consumption" & pt_summ$price==min(pt_summ$price)]),
             max(pt_summ$mean[pt_summ$group=="Expenditure"])))

  pt_summ$group <- paste0("Mean +/- Std Error of ",pt_summ$group)

 plot <- ggplot2::ggplot(pt_summ, ggplot2::aes(x = factor(price), y = mean, group = group)) +
   ggplot2::geom_pointrange(ggplot2::aes(ymin = mean - se, ymax = mean + se), colour = "#000000", show.legend = F) +
   ggplot2::theme_classic() + ggplot2::xlab("Price") +
   ggplot2::scale_x_discrete(breaks = factor(pt_spend$price),
                             expand = c(0.025,0.025,0.025,0.025)) +
   ggplot2::facet_wrap(~group, scales = "free_y", ncol = 1) +
   ggplot2::theme(strip.text = ggplot2::element_text(size = 17,face = "bold", hjust = 0.5),
         strip.background = ggplot2::element_blank(),
         plot.title = ggplot2::element_blank(),
         axis.title.x = ggplot2::element_text(size = 13, face = "bold"),
         axis.title.y = ggplot2::element_blank(),
         axis.text.x = ggplot2::element_text(size = 13, angle = 60, vjust = 0.5),
         axis.line = ggplot2::element_line(colour = "black", size = 1),
         axis.text.y = ggplot2::element_text(size = 13))

 plot +
   ggplot2::geom_segment(pt_anno[(pt_anno$group=="Mean +/- Std Error of Consumption"),],
                         mapping = ggplot2::aes(x = factor(price), xend = factor(price), y = -Inf, yend = mean), size = 0.25, linetype = "dashed") +
   ggplot2::geom_segment(pt_anno[(pt_anno$group=="Mean +/- Std Error of Consumption"),],
                         mapping = ggplot2::aes(x = -Inf, xend = factor(price), y = mean, yend = mean), size = 0.25, linetype = "dashed") +
   ggplot2::geom_segment(pt_anno[(pt_anno$group=="Mean +/- Std Error of Expenditure"),],
                       mapping = ggplot2::aes(x = factor(price), xend = factor(price), y = -Inf, yend = mean), size = 0.25, linetype = "dashed") +
   ggplot2::geom_segment(pt_anno[(pt_anno$group=="Mean +/- Std Error of Expenditure"),],
                         mapping = ggplot2::aes(x = -Inf, xend = factor(price), y = mean, yend = mean), size = 0.25, linetype = "dashed") +
   ggplot2::scale_colour_manual(values = c("#BEBEBE","#BEBEBE")) +
   ggplot2::geom_point(pt_anno, mapping = ggplot2::aes(x = factor(price), y = mean, colour = group),
                       size = 1.75, show.legend = F) + ggplot2::coord_cartesian(clip = "off") +
   ggplot2::geom_text(pt_anno, mapping = ggplot2::aes(label = label, colour = group),
                       colour = "#B0B0B0", size = 5, x = Inf, y = Inf, hjust = 1, vjust = 1, show.legend = F)

  }
