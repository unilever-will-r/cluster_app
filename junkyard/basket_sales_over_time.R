cleanTripType <- function(dt, type_col = "type_id") {
  filteredDt <- dt[type_id %in% c("STORE PICK-UP",
                                  "NONE"
                                  )]
                                  # "SHIP FROM STORE", 
                                  # "SHIPT")]
  return(filteredDt)
}


capVar <- function(v, lb = .05, ub = .95) {
  pmin(quantile(v, ub), v) %>%
    pmax(quantile(v, lb), .)
}

# create factor level
splitBins <- function(v, n_bins = 10) {
  # v <- as.numeric(v)
  min_val <- min(v)
  max_val <- max(v)
  bin_width <- (max_val - min_val) / (n_bins)
  bin_edges <- seq(min_val, max_val, by = as.numeric(bin_width))
  bin_labels <- mapply(bin_edges[1 : n_bins], bin_edges[2 : (n_bins + 1)],
                       FUN = function(x, y) {paste0("[", round(x, 2), ", ", round(y, 2), "]")})
  if (inherits(v, "Date")) {
    v <- as.Date(v)
  }
  bin_indices <- cut(v, breaks = bin_edges, labels = bin_labels, include.lowest = TRUE)
  return(bin_indices)
}

plotSecondary <- function(plotDt,
                          bar_axis_name, 
                          line_axis_name,
                          n_bins = 10,
                          use_facet = NULL) {
  # Fix the factor names
  ggDt <- copy(plotDt)
  
  # force facet to be named "facet"
  if (is.null(use_facet)) {
    facetName <- "facet"
    ggDt[, facet := 1]
  } else {
    facetName <- use_facet
    if(!(facetName %in% names(ggDt))) {
      stop("facet is not a column in input")
    }
    setnames(ggDt, old = use_facet, new = "facet")
  }
  
  setnames(ggDt, 
           old = c(bar_axis_name, 
                   line_axis_name),
           new = c("bar_name", 
                   "line_name"))
  

  # aggregate on the factor level, name the factor levels
  ggDt[, bar_level := splitBins(bar_name, n_bins = n_bins)]
  
  ggDt <- ggDt[, .(bar_counts = .N,
                   mean_y = mean(line_name)), 
               by = .(bar_level, facet)]
  
  trans_factor <- ggDt[, max(mean_y) / max(bar_counts)]
  g <- ggplot(ggDt) +
    geom_bar(aes(x = bar_level, y = bar_counts), stat = "identity") + 
    geom_line(aes(x = bar_level, y = mean_y / trans_factor, group = 1), color = "red") +
    scale_y_continuous(sec.axis = sec_axis(~.*trans_factor, name = paste0(line_axis_name, " (in red)"))) +
    xlab(bar_axis_name) + 
    ylab(paste0(bar_axis_name, " (Frequency)")) +
    theme(axis.text.x = element_text(angle = -45, vjust = .5, hjust = 0))
  
  if (ggDt[, uniqueN(facet)] > 1) {
    g <- g + facet_wrap(~facet)
  }
  
  return(
    g
  )
}

plotPvo <- function(plotDt,
                    bar_axis_name, 
                    dep_var_name,
                    pred_var_name,
                    n_bins = 10,
                    use_facet = NULL) {
  # Fix the factor names
  ggDt <- copy(plotDt)
  
  # force facet to be named "facet"
  if (is.null(use_facet)) {
    facetName <- "facet"
    ggDt[, facet := 1]
  } else {
    facetName <- use_facet
    if(!(facetName %in% names(ggDt))) {
      stop("facet is not a column in input")
    }
    setnames(ggDt, old = use_facet, new = "facet")
  }
  
  setnames(ggDt, 
           old = c(bar_axis_name, 
                   dep_var_name,
                   pred_var_name),
           new = c("bar_name", 
                   "dep_var",
                   "pred_var"))
  # aggregate on the factor level, name the factor levels
  ggDt[, bar_level := splitBins(bar_name, n_bins = n_bins)]
  
  ggDt <- ggDt[, .(bar_counts = .N,
                   mean_y_expected = mean(pred_var),
                   mean_y_observed = mean(dep_var)), 
               by = .(bar_level, facet)]
  
  trans_factor <- ggDt[, max(pmax(mean_y_expected, mean_y_observed)) / max(bar_counts)]
  g <- ggplot(ggDt) +
    geom_bar(aes(x = bar_level, y = bar_counts), stat = "identity", alpha = .7) + 
    geom_line(aes(x = bar_level, y = mean_y_expected / trans_factor, group = 1), color = "red") +
    geom_line(aes(x = bar_level, y = mean_y_observed / trans_factor, group = 1), color = "black") +
    scale_y_continuous(sec.axis = sec_axis(~.*trans_factor, name = dep_var_name)) +
    xlab(bar_axis_name) + 
    ylab(paste0(bar_axis_name, " (Frequency)")) +
    theme(axis.text.x = element_text(angle = -45, vjust = .5, hjust = 0))
  
  if (ggDt[, uniqueN(facet)] > 1) {
    g <- g + facet_wrap(~facet)
  }
  
  return(
    g
  )
}

addTripTypeFeatures <- function(guestDt) {
  guestDt[, guest_lifetime_trips := sum(n_trips), keyby = .(guest_id)]
  propDt <- dcast(guestDt, guest_id+guest_lifetime_trips~type_id, value.var = "n_trips", sum)
  oldPropNames <- rightOnly(c("guest_id", "guest_lifetime_trips"), names(propDt))
  newPropNames <- paste0("type_", seq(1, length(oldPropNames)))
  setnames(propDt, old = oldPropNames, new = newPropNames)
  propDt[, (newPropNames) := lapply(.SD, FUN = function(x) x / guest_lifetime_trips), .SDcols = newPropNames]
  regDt <- guestDt[, .(n_stores = uniqueN(store_id),
                       start_date = min(first_date),
                       end_date = max(last_date)), 
                   keyby = .(guest_id)] %>%
    .[, guest_n_weeks := length(start_date : end_date) / 7, by = .(start_date, end_date)] %>%
    merge(propDt, by = "guest_id")
  return(
    regDt
  )
}


if (FALSE) {
  rm(list = ls(all.names = TRUE)); gc();
  setwd("~/r_code")
  source("00_initialize.R")
  config <- initializeProject(configPath = "~/r_code/config.yaml")
  setwd("C:/Users/William.Roberts/Downloads/basket_dump/")
  
  cTripDt <- fread("all_data.csv") %>%
    filterToState(state_name = c("California")) %>%
    cleanTripType
  mnTripDt <- fread("sample_mn.csv") %>%
    cleanTripType
  
  arTripDt <- fread("sample_ar.csv") %>%
    cleanTripType
  
  regDtMn <- addTripTypeFeatures(mnTripDt)
  regDtAr <- addTripTypeFeatures(arTripDt)
  regDtNy <- addTripTypeFeatures(nyTripDt)
  regDtCa <- addTripTypeFeatures(cTripDt)
  
  # plot with facet var def
  plotSecondary(copy(regDt) %>%
                  .[start_date > "2021-01-01"] %>%
                  .[guest_lifetime_trips > 10] %>%
                  .[, xvar := capVar(guest_n_weeks, .01, .99)] %>%
                  .[, f_var := fifelse(type_2 > 0,
                                       "No Store Pick-Ups",
                                       "At least one Store Pick-up") ],
                bar_axis_name = "xvar",
                line_axis_name = "n_stores", 
                n_bins = 10, 
                use_facet = "f_var") + 
    xlab("guest lifetime duration in weeks") + 
    ylab("# of guests") + 
    ggtitle("# of Unique Stores split by guest purchase history length", 
            "Filtered to store visits in New York, filtered to guests with at least 10 trips.")
  
  # plot with no facet var def
  plotSecondary(regDt %>%
                  .[guest_lifetime_trips > 10],
                bar_axis_name = "type_2",
                line_axis_name = "n_stores",
                n_bins = 100)
  
  regDt[, has_pickuped := fifelse(type_2 > 0, 1, 0)]
  
  lm1 <- lm(formula = log(n_stores)~start_date+log(guest_lifetime_trips)+type_2, 
            copy(regDt) %>%
              .[start_date < "2029-06-01"] %>%
              .[guest_lifetime_trips > 10] %>%
              .[start_date > "2020-02-01"])
  
  pvoDt <- regDt[, predicted := predict(lm1, regDt) %>% exp] %>%
    .[guest_lifetime_trips > 10] %>%
    .[start_date > "2020-06-01"]
  
  plotPvo(copy(pvoDt) %>%
            .[, xvar := capVar(type_2, .0001, .999)], 
          bar_axis_name = "xvar",
          dep_var_name = "n_stores",
          pred_var_name = "predicted",
          n_bins = 20) + xlab("% of orders == \"STORE PICK-UP\"")
 }

if (FALSE) {
  ggplot(dateDt %>%
           .[type_id %in% c("NONE", "SHIP FROM STORE", "STORE PICK-UP")]) +
    geom_line(aes(color = type_id, y = real_cherry_dollar, x = guest_acquired_date)) + 
    xlab("Guest Acquired Date") + 
    ylab("Total Dollar") + 
    theme(legend.position = "top",
          legend.title = element_blank())
}


