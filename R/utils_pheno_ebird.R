#' Preparing data for PLOTTING SPECIES' RELATIVE NUMBER OF OBSERVATIONS BY WEEKS AND MONTHS
#'
#' @param obs_site   a dataframe containing all observations of interest for one site
#' @param bats_pheno a datframe of taxon's phenology
#' @param ordre      order of in which the taxon are presented on the y axis
#'
#' @import 
#' @return a dataframe in the long format containing the normalized number of observation per Taxon for each week of each month of the year.
#' @export
#'
#'
get_weekly_one_bat <- function(obs_site, bats_pheno_site, ordre = NULL) {
  bats_pheno
  #-------------------------------------------------------------------------------
  # Format df and count taxon observations per week
  #-------------------------------------------------------------------------------
  weekly_one_bat <-
    obs_site |>
    # # Select data chosen by user
    # dplyr::filter(lubridate::year(date_obs) == annee) |>
    dplyr::mutate(date_fmt = lubridate::ymd(date_obs)) |>
    #dplyr::filter(obs_species.taxa_name == taxa) |>
    dplyr::select(Taxon = obs_species.taxa_name, date_fmt) |>
    #dplyr::filter(Taxon == "Eptesicus fuscus") |>
    # Get wk and mth
    dplyr::mutate(wk = lubridate::week(date_fmt)) |>
    dplyr::select(Taxon, wk) |>
    ## Add weekly count of observations per Taxon
    dplyr::group_by(Taxon) |>
    dplyr::add_count(wk) |>
    ## Format df
    dplyr::ungroup() |>
    dplyr::distinct() |>
    ## complete missing weeks
    tidyr::complete(wk = 1:53, Taxon, fill=list(n=0)) |> 
    ## Add month
    dplyr::nest_by(wk)
  
  # Add month
  weekly_one_bat$mth <- lubridate::month(seq.Date(lubridate::dmy("01-01-2000"), lubridate::dmy("31-12-2000"), by="week"), abbr = TRUE, label = TRUE)
  
  # Clean df
  weekly_one_bat <- weekly_one_bat |>  
    tidyr::unnest(cols = c(data)) |>
    dplyr::ungroup()
  
  
  weekly_one_bat$mth <- as.character(weekly_one_bat$mth)
    # Add french months
  weekly_one_bat <- weekly_one_bat |>
    dplyr::mutate(mth_en = mth,
                  mth = dplyr::case_when(mth_en == "Jan" ~ "Janvier",
                                         mth_en == "Feb" ~ "Février",
                                         mth_en == "Mar" ~ "Mars",
                                         mth_en == "Apr" ~ "Avril",
                                         mth_en == "May" ~ "Mai",
                                         mth_en == "Jun" ~ "Juin",
                                         mth_en == "Jul" ~ "Juillet",
                                         mth_en == "Aug" ~ "Août",
                                         mth_en == "Sep" ~ "Septembre",
                                         mth_en == "Oct" ~ "Octobre",
                                         mth_en == "Nov" ~ "Novembre",
                                         mth_en == "Dec" ~ "Décembre"))
  
  weekly_one_bat$mth <- ordered(weekly_one_bat$mth, levels = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre","Octobre", "Novembre", "Décembre"))
  
  # Add min_yd and pres to df (needed for ordering)
  bats_pheno_ord <- bats_pheno_site |>
    dplyr::select(Taxon, min_yd, pres)
  weekly_one_bat <- weekly_one_bat |>
    dplyr::left_join(bats_pheno_ord, by = "Taxon")
  
  # Order taxon
  # if(ordre == "premiere_obs") {
  #   weekly_one_bat$Taxon <- reorder(weekly_one_bat$Taxon, -weekly_one_bat$min_yd)
  # } else if(ordre == "jours_de_presence") {
  #   weekly_one_bat$Taxon <- reorder(weekly_one_bat$Taxon, weekly_one_bat$pres)
  # }

 # weekly_one_bat$Taxon <- droplevels(weekly_one_bat$Taxon)
  
  
  # Final data formatting
  weekly_one_bat <- weekly_one_bat |>
    dplyr::mutate(n_std = n/max(n)) |>
    dplyr::select(wk, mth, Taxon, n_std) |>
    as.data.frame()
  
  return(weekly_one_bat)
}


#'  PLOTTING SPECIES' RELATIVE NUMBER OF OBSERVATIONS BY WEEKS AND MONTHS
#'  eBird-like histogram
#'
#' @param dat                data.frame. Should be long format.
#' @param val.colname        character. Column name for the value to plot
#' @param var.y.colname      character. Column name for the y facet.
#' @param var.x1.colname     character. Column name for the x facet.
#' @param var.x2.colname     character. Column name for the x axis.
#' @param col.fill           character. Color for the geom_col fill aes
#' @param strip.fill         character. Color for the header background
#' @param row.fill           character. Color for the alternating row. 
#'                           Default is strip.fill with an alpha of 0.1.
#' @param arrange.widths     Numeric. Proportions of the left and right side plots.
#'                           Adjust according to y facet names & number of x facet's columns
#' @import ggplot2
#' 
#' @return A "ebird-like histogram" using \code{ggiraph}
#' 
#' @examples
#' canards <- readRDS("canards15.rds")
#' gg.obs <- ggObsTable(canards, "relObs", "species", "month", "week")
#' gg.obs
#' ggsave("plot.pdf", plot = gg.obs, width = 11, height = 5)
ggObsTable <- function(dat, val.colname, 
                       var.y.colname, var.x1.colname, var.x2.colname,
                       col.fill = "#7AD151FF", # from viridis::viridis(1, begin = 0.8)
                       bg.fill = "#482576FF", # from viridis::viridis(1, begin = 0.1)
                       row.alpha = 0.1, row.fill = NULL,
                       arrange.widths = c(0.2, 0.80)){
  # Shorten display of var.y.colname
  dat[,var.y.colname] <- stringr::str_wrap(dat[,var.y.colname], 30)
  # needs to be factors else colours won't match later
  if(class(dat[,var.y.colname])!="factor") 
    dat[,var.y.colname] <- as.factor(dat[,var.y.colname])
  # prep data for geoms
  dat.rect <- unique(dat[, c(var.y.colname, var.x1.colname)])
  dat.col <- rbind(dat, `[<-`(dat,,val.colname, 0-dat[,val.colname]))
  dat.lab <- unique(`[<-`(dat.rect,,var.x1.colname, ""))
  #-------------------------------------------------------------------------------
  # palette for the altenating rows colors
  #-------------------------------------------------------------------------------
  # adjust row.fill
  if(missing(row.fill)) row.fill <- ggplot2::alpha(bg.fill, 0.1)
  else if(!missing(row.alpha)) row.fill <- ggplot2::alpha(row.fill, row.alpha)
  
  # by using cbind, it repeats for the number of y values
  pal.rows <- suppressWarnings(
    cbind(val = c("white", row.fill), 
          nm = levels(dat.lab[,var.y.colname]))
  )
  pal.rows <- with(data.frame(pal.rows), `names<-`(val, nm))
  #-------------------------------------------------------------------------------
  # RIGHT SIDE PLOT
  # for the rel. observation data
  #-------------------------------------------------------------------------------
  gg.right <- ggplot2::ggplot() +
    # alternating background color for rows 
    # has to be before to not go over the observations columns
    ggplot2::geom_rect(data = dat.rect,
                       ggplot2::aes_string(fill = var.y.colname),
              xmin = -Inf, xmax = Inf,
              ymin = -Inf, ymax = Inf,
              show.legend = FALSE) +
    # columns of the relative observation
    # rbind the negative values of observations so the columns are centred around zero
    ggplot2::geom_col(data = dat.col,
                      ggplot2::aes_string(y = val.colname, x = var.x2.colname), 
             fill = col.fill) +
    # next line should be adjusted to the data if the proportion of observations isn't between 0 and 1
    ggplot2::scale_y_continuous(limits = c(-1, 1), expand = ggplot2::expansion(0,0))
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(0,0))
  #-------------------------------------------------------------------------------
  # LEFT SIDE PLOT
  # for the species names : done this way so the background colour of 
  #-------------------------------------------------------------------------------
  # the header goes all the way; can be omited if don't mind it's absence 
  # (see note at bottom and in ggSetup)
  gg.left <- ggplot2::ggplot(dat.lab) +
    # alternating background color for rows
    ggplot2::geom_rect(ggplot2::aes_string(fill = var.y.colname), 
              xmin = 0,xmax = 1,
              ymin = 0, ymax = 1,
              show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes_string(label = var.y.colname),
              x = 0.1, y = 0.5, 
              hjust = 0, vjust = 0.5, 
              fontface = "plain",
              size = 3) +
    # need these next lines so text isn't cropped
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = ggplot2::expansion(0,0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = ggplot2::expansion(0,0))
  #-------------------------------------------------------------------------------
  # PLOT SET UP & PRETTIFYING
  # wrapped facet, axis & theme setup in a function since to reuse for both sides
  #-------------------------------------------------------------------------------
  gg.setup <- function(gg, pal = pal.rows){
    gg + 
      ggplot2::scale_fill_manual(values = pal) +
      ggplot2::facet_grid(as.formula(paste(var.y.colname, "~", var.x1.colname)), space = "free_x", scales = "free_x") +
      # remove axis, grid lines, etc.
      ggplot2::theme_void(base_size = 10, base_rect_size = 1) +
      ggplot2::theme(legend.position = "none",
            panel.spacing = ggplot2::unit(0,"pt"),
            # remove next line if omitting left side
            strip.text.y = ggplot2::element_blank(),
            strip.text.x.top = ggplot2::element_text(color = "white", face = "plain", 
                                            margin = ggplot2::margin(t = 4, b = 4)),
            strip.background.x = ggplot2::element_rect(fill = bg.fill, color = "white", size = 1),
            panel.border = ggplot2::element_rect(fill = NA, color = "white", size = 1)
      )
  }
  ggpubr::as_ggplot(gridExtra::grid.arrange(
    gg.setup(gg.left), gg.setup(gg.right),
    nrow=1, widths = arrange.widths
  ))
  #===============================================================================
  # # NOTE :
  # # if you don't want the top left corner you can use this to alternate
  # # panel background color
  # ggGrob <- ggplotGrob(gg)
  # for(i in grep("strip-l-", ggGrob$layout$name)[strtoi(gsub("strip-l-", "", grep("strip-l-", ggGrob$layout$name, value = TRUE)))%%2==0]){
  #   ggGrob$grobs[[i]]$grobs[[1]]$children[[1]]$gp$fill <- "white"
  # }
  # ggplotify::as.ggplot(ggGrob)
}
