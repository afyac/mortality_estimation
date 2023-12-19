#...............................................................................
### ++++++++++ RECONSTRUCTING POPULATION DENOMINATORS IN SOMALIA +++++++++++ ###
#...............................................................................

#...............................................................................
## -------------- R SCRIPT TO SPECIFY FUNCTIONS FOR ANALYSIS ---------------- ##
#...............................................................................

                          # LSHTM, SIMAD University (August 2023)
                          # francesco.checchi@lshtm_ac.uk 



#...............................................................................   
### Function to generate time series of district-months
#...............................................................................

f_gen_ts <- function(admin2_f = admin2, when_f = when) {
  
  # create a time unit variable tm (from month 1 to month T of period) 
  tm <- seq(1, (( when_f$y_end + when_f$burn_out_period - when_f$y_start + 
    when_f$burn_in_period ) * 12 + when_f$m_end - when_f$m_start + 1 ), 1)
  
  # Create a time series of district-year-months
  ts <- expand.grid(sort(unique(admin2_f$district)), tm)
  colnames(ts) <- c("district", "tm")
  
  # Add regions and mega-regions
  ts <- merge(ts, admin2_f, by = "district", all.x = TRUE)
  
  # Work out corresponding year, month and date values
  ts$y <- floor( (ts$tm + when_f$m_start - 2) / 12) + (when_f$y_start - 
    when_f$burn_in_period)
  ts$m <- (ts$tm + when_f$m_start - 1) - (ts$y - (when_f$y_start - 
    when_f$burn_in_period) ) * 12
  ts$date <- lubridate::ymd(paste(ts$y, ts$m, "1", sep = "-"))    

  # Sort time series
  ts <- ts[order(ts[, "district"], ts[, "tm"]), ]
}



#............................................................................... 
### Function to compute RMSE and log likelihood of any reconstruction run
#...............................................................................

###FC: UNRESOLVED Q: can we treat DTM training data as independent observations?

f_gof <- function(pop_out_f = pop_out, dtm_f = dtm, 
  strata_f = unique(admin2$stratum)) {
  
  # Merge output of run with DTM fitting data
  out_mrg <- merge(dtm_f, pop_out_f, by = c("district", "tm"), all.x = TRUE)
  
  # Omit NA values (will happen for population sources that are missing a few
    # districts - esp. polio 2018)
  out_mrg <- na.omit(out_mrg)
  
  # Initialise output
  out_gof <- data.frame(stratum = strata_f, rmse = NA, ll = NA)
  out_gof[, c("implausible_pop", "implausible_prop_idp")] <- NA
    
  # Record random values of parameters
  x <- unique(pop_out_f[, c("stratum", "prop_stay", "ret_time")])
  out_gof <- merge(out_gof, x, by = "stratum")
  
  # For each stratum...
  for (i in 1:nrow(out_gof)) {
    
    # subset data
    x <- subset(out_mrg, stratum == out_gof[i, "stratum"])
    
    # compute root mean square error, weighted for each observation's size
      # (target: proportion of returnees among all returnees and IDPs)
    out_gof[i, "rmse"] <- sqrt(weighted.mean((x$prop_ret_obs - x$prop_ret)^2, 
      x$wt_rmse))

    # compute log likelihood - grouped binomial data (1/N to account for
      # unequal-sized training data observations), weighted for observation size
      # (target: proportion of returnees among all returnees and IDPs)
    out_gof[i, "ll"] <- sum((dbinom(x = x$n_ret, size = (x$n_ret + x$n_idp), 
      prob = x$prop_ret_obs, log = TRUE) / (x$n_ret + x$n_idp)) * x$wt_ll)

    # check what proportion of implausible data have been outputted during
      # population reconstruction
      # subset data
      x <- subset(pop_out_f, stratum == out_gof[i, "stratum"])
      x <- na.omit(x)
      
      # proportion of negative population values
      out_gof[i, "implausible_pop"] <- sum(x$pop < 0) / length(x$pop)
      
      # proportion of predictions in which the proportion of IDPs is <>0-100%
      out_gof[i, "implausible_prop_idp"] <- sum(! between(x$n_idp/x$pop, 0, 1))/
        length(x$pop)
  }
    
  # Output
  out_gof <- out_gof[order(out_gof$stratum), ]
  return(out_gof)
}



#...............................................................................  
### Function to compute a penalty for the negative log-likelihood
    # based on a log barrier function (below/above 'barrier', negLL worsens)
#...............................................................................

f_pen <- function(out_f, prop_stay_bar_lo_f = prop_stay_bar_lo, 
  ret_time_bar_lo_f = ret_time_bar_lo, prop_stay_bar_hi_f = prop_stay_bar_hi,
  ret_time_bar_hi_f = ret_time_bar_hi, prop_stay_grid_f = prop_stay_grid,
  ret_time_grid_f = ret_time_grid) {
  
  # Identify values of both parameters
  prop_stay_val <- out_f["prop_stay"]
  ret_time_val <- out_f["ret_time"]
  
  # Identify max and min of parameter grid ranges
  prop_stay_min <- min(prop_stay_grid_f)
  prop_stay_max <- max(prop_stay_grid_f)
  ret_time_min <- min(ret_time_grid_f)
  ret_time_max <- max(ret_time_grid_f)
  
  # Penalties are 0 by default...
  prop_stay_pen <- 0
  ret_time_pen <- 0
  
  # ...unless values are below the barrier...
  if (prop_stay_val < prop_stay_bar_lo_f) 
    {prop_stay_pen <- -log(abs(prop_stay_val - prop_stay_min) / 
    abs(prop_stay_bar_lo_f - prop_stay_min)) }
  if (ret_time_val < ret_time_bar_lo_f) 
    {ret_time_pen <- -log(abs(ret_time_val - ret_time_min) / 
    abs(ret_time_bar_lo_f - ret_time_min)) }
  
  # ...or above the barrier:
  if (prop_stay_val > prop_stay_bar_hi_f) 
    {prop_stay_pen <- -log(1 - abs(prop_stay_val - prop_stay_bar_hi_f) / 
    abs(prop_stay_bar_hi_f - prop_stay_max)) }
  if (ret_time_val > ret_time_bar_hi_f) 
    {ret_time_pen <- -log(1 - abs(ret_time_val - ret_time_bar_hi_f) / 
    abs(ret_time_bar_hi_f - ret_time_max)) }
  
  # So the final penalty is:
  pen <- prop_stay_pen + ret_time_pen
  return(pen)
}    



#...............................................................................  
### Function to reconstruct population and displaced compartments for any inputs
#...............................................................................

f_pop <- function(ts_f = ts, ts_w_f = ts_w, admin2_f = admin2, 
  pop_sources_f = pop_sources, which_source_f = "pop_polio_2018", 
  prmn_f = prmn, idp_unpess_prop_f = idp_unpess_prop, 
  m_unpess_idp_f = m_unpess_idp, y_unpess_idp_f = y_unpess_idp, 
  refugees_w_f = refugees_w, g_f = g,
  pars_f = pars_i) {

  #.........................................
  ## Preparatory steps

    # Read random parameter values for each district (by stratum)
    x <- unique(admin2_f$stratum)
    pars <- data.frame(stratum = x, prop_stay = NA, ret_time = NA)
    for (i in 1:length(x) ) {
      pars[which(pars$stratum == x[i]), "prop_stay"] <- pars_f[i*2 - 1]
      pars[which(pars$stratum == x[i]), "ret_time"] <- pars_f[i*2]
    }
    colnames(pars) <- c("stratum", "prop_stay", "ret_time")
    pars$ret_rate <- 1 / pars$ret_time
    pars <- merge(admin2_f, pars, by = "stratum", all.x = TRUE)
    pars <- pars[order(pars$district), ]
    
    # Select population source
    which_source <-
      pop_sources_f[which(pop_sources_f$worksheet == which_source_f), ]

    # Get reconstruction dataset with single known data point
    N_i <- get(paste(which_source$worksheet, "_w", sep = ""))  

    # Figure out sequence of time points to reconstruct forward or backward
    forward <- (which_source$tm + 1) : max(unique(ts_f$tm))
    backward <- (which_source$tm - 1) : 1

    # Initialise IDP and returnee compartments
    I_i <- ts_w_f # IDPs who will eventually return
    S_i <- ts_w_f # IDPs who will never return
    R_i <- ts_w_f # IDPs who have returned (IDP returnees)
    
    # Set IDP and returnee compartments to 0 pre-2016 (no PRMN data) - 
      # but will add UNPESS survey IDPs later
    tm_prmn <- unique(ts_f[which(ts_f$y == 2015 & ts$m == 12), "tm"])
    I_i[, as.character(1:(tm_prmn-1))] <- 0
    S_i[, as.character(1:(tm_prmn-1))] <- 0
    R_i[, as.character(1:(tm_prmn-1))] <- 0
    
    # Initialise time-varying matrix of eventually-returning IDPs from/to: 
      # all 0 at start
    x <- data.frame(expand_grid(unique(ts_f$district), unique(ts_f$district)),0)
    I_ji <- x
    colnames(I_ji) <- c("district_dep", "district_arr", "I")
    
    # Initialise time-varying matrix of never-returning IDPs from/to: 
      # all 0 at start
    S_ji <- x
    colnames(S_ji) <- c("district_dep", "district_arr", "S")

    # Initialise time-varying matrix of returnees from/to: all 0 at start
    R_ij <- x
    colnames(R_ij) <- c("district_dep", "district_arr", "R")
    
  #.........................................
  ## Reconstruct evolution of IDP and returnee compartments from 1 Jan 2016
  
  for (tm_i in (tm_prmn):max(unique(ts_f$tm)) ) {
    # PRMN movements for this time step
    prmn_tm <- subset(prmn_f, tm == tm_i)
    
    # IDPs who will return: add natural growth, subtract returning IDPs, add 
      # new IDPs, aggregate
    I_ji$I <- I_ji$I * (1 + g_f)
    x <- I_ji$I * pars$ret_rate
    I_ji$I <- I_ji$I - x + (1 - pars$prop_stay) * prmn_tm$n_idp
    I_i[, as.character(tm_i)] <- aggregate(I_ji$I, by = list(I_ji$district_arr), 
      FUN = sum)$x
    
    # IDPs who will not return: add natural growth and new IDPs, aggregate
    S_ji$S <- (1 + g_f) * S_ji$S + pars$prop_stay * prmn_tm$n_idp
    S_i[, as.character(tm_i)] <- aggregate(S_ji$S, by = list(S_ji$district_arr), 
      FUN = sum)$x
      
    # Returnees: add natural growth and new returnees, aggregate
    R_ij$R <- (1 + g_f) * R_ij$R + x
    R_i[, as.character(tm_i)] <- aggregate(R_ij$R, by = list(R_ij$district_dep), 
      FUN = sum)$x
  }

  #.........................................
  ## Reconstruct population

    # Forward calculation from known time point
    for (tm_i in forward) {
      
      # PRMN movements for this time step, by district of departure
      prmn_tm <- subset(prmn_f, tm == tm_i)
      dep <- aggregate(prmn_tm$n_idp, by = list(prmn_tm$district_dep),
        FUN = sum)$x
      
      # how many never-displaced people were there at start of time point?
      x <- N_i[, as.character(tm_i - 1)] - I_i[, as.character(tm_i - 1)] -
        S_i[, as.character(tm_i - 1)] - R_i[, as.character(tm_i - 1)]
      x[which(x < 0)] <- 0 # cannot be less than 0
      
      # grow the never-displaced population, remove leaving IDPs
        # and add the other compartments
      N_i[, as.character(tm_i)] <- (1 + g_f) * x - dep + 
        refugees_w_f[, as.character(tm_i)] + I_i[, as.character(tm_i)] + 
        S_i[, as.character(tm_i)] + R_i[, as.character(tm_i)]
    }
    
    # Back-calculation from known time point
    for (tm_i in backward) {
      
      # PRMN movements for this time step, by district of departure
      prmn_tm <- subset(prmn_f, tm == tm_i)
      dep <- aggregate(prmn_tm$n_idp, by = list(prmn_tm$district_dep),
        FUN = sum)$x
       
      # how many never-displaced people are there at the next time point?
      x <- N_i[, as.character(tm_i + 1)] - I_i[, as.character(tm_i + 1)] -
        S_i[, as.character(tm_i + 1)] - R_i[, as.character(tm_i + 1)]
      x[which(x < 0)] <- 0 # cannot be less than 0
      
      # ungrow the never-displaced population, add future new IDPs
        # and add the other compartments
      N_i[, as.character(tm_i)] <- x / (1 + g_f) + dep -
        refugees_w_f[, as.character(tm_i)] + I_i[, as.character(tm_i)] + 
        S_i[, as.character(tm_i)] + R_i[, as.character(tm_i)]
    }

    # Add 'old' IDPs from 2014 UNPESS survey (all assumed to be staying, 
      # i.e. part of S)
    S_iold <- ts_w_f
    
      # sort UNPESS IDP dataset
      idp_unpess_prop_f <- idp_unpess_prop_f[order(idp_unpess_prop_f$district),]
    
      # figure out time point that UNPESS IDP dataset refers to
      tm_unpess_idp <- unique(ts[which(ts$m == m_unpess_idp_f & 
        ts$y == y_unpess_idp_f), "tm"])
      
      # calculate prevalent IDPs at UNPESS survey
      S_iold[, as.character(tm_unpess_idp)] <- idp_unpess_prop_f$prop_idps * 
        N_i[, as.character(tm_unpess_idp)]
      
      # forward calculation from known time point
      for (tm_i in (tm_unpess_idp+1):max(unique(ts_f$tm))) {
        S_iold[, as.character(tm_i)] <- (1 + g_f) * 
        S_iold[, as.character(tm_i - 1)]
      }
    
      # back-calculation from known time point
      for (tm_i in (tm_unpess_idp - 1):1) {
        S_iold[, as.character(tm_i)] <- S_iold[, as.character(tm_i + 1)] / 
          (1 + g_f)
      }
      
      # add to other staying IDPs
      S_i[, 2:ncol(S_i)] <- S_i[, 2:ncol(S_i)] + S_iold[, 2:ncol(S_iold)]

  #.........................................
  ## Bind and output results
        
    # Reshape long again
    N_i <- melt(N_i, id.vars = "district", variable.name = "tm", 
      value.name = "pop")
    I_i <- melt(I_i, id.vars = "district", variable.name = "tm", 
      value.name = "n_idp_ret")
    S_i <- melt(S_i, id.vars = "district", variable.name = "tm", 
      value.name = "n_idp_sty")
    R_i <- melt(R_i, id.vars = "district", variable.name = "tm", 
      value.name = "n_ret")

    # Merge together and compute total number of prevalent IDPs
    out <- Reduce(function(df1, df2) {merge(df1, df2, by = c("district", "tm"), 
      all = TRUE)}, list(N_i, I_i, S_i, R_i))
    out$n_idp <- out$n_idp_ret + out$n_idp_sty
    out <- out[order(out$district, out$tm), ]
    
    # Round all to integers
    out[, c("pop", "n_idp_ret", "n_idp_sty", "n_idp", "n_ret")] <- 
      apply(out[, c("pop", "n_idp_ret", "n_idp_sty", "n_idp", "n_ret")], 2, 
        as.integer)
    
    # Also compute the ratio of returnees to (returnees + IDPs) -> to fit to
    out$prop_ret <- out$n_ret / (out$n_ret + out$n_idp)
    
    # Also record random parameter values
    out <- merge(out, pars, by = "district", all.x = TRUE)
    
    # Output
    return(out)
}   
    


#...............................................................................
### Function to compute point estimates and 2-dim. profile confidence intervals
  # for a single metric of goodness of fit and population source; includes plots
#...............................................................................

f_prof <- function(out_f = out, metric_f = "negll_pen", 
  pop_source_f = "pop_afripop_2015", palette_cb_f = palette_cb) {

  #.........................................
  ## Initialise output

    # Estimates and confidence intervals
    out_all <- expand.grid(unique(out_f$stratum), c("prop_stay", "ret_time"))
    colnames(out_all) <- c("stratum", "parameter")
    out_all$pop_source <- pop_source_f
    out_all[, c("best", "lci_025", "uci_975")] <- NA
    out_all <- out_all[order(out_all$stratum, out_all$parameter), ]
    
    # Plots
    plots_out <- vector("list", length = 2 * length(unique(out_f$stratum)))
    names(plots_out) <- c(
      paste("proportion who never return", " (", 
        unique(out_f$stratum), ")", sep = "") ,
      paste("mean time to return", " (", 
        unique(out_f$stratum), ")", sep = "")
    )  
    if (metric_f == "rmse") {plot_y_lab <- "root mean square error"}
    if (metric_f == "negll") {plot_y_lab <- "negative log likelihood"}
    if (metric_f == "negll_pen") 
      {plot_y_lab <- "penalised negative log likelihood"}
    
    
  #.........................................
  ## Profile analysis for each stratum
  for (i in unique(out_f$stratum)) {

    # Select data
    out_f_str <- subset(out_f, stratum == i & pop_source == pop_source_f)

    # Eliminate non-finite output
    out_f_str <- out_f_str[which(is.finite(out_f_str[, metric_f])), ]

    # If no output is left, move to next loop
    if (nrow(out_f_str) == 0) {next}
    
    # Compute likelihood/RSME profiles through slicing method
      # profile for prop_stay
      prop_stay_prof <- data.frame(prop_stay = unique(out_f_str$prop_stay))
      for (j in 1:nrow(prop_stay_prof)) {
        prop_stay_prof[j, metric_f] <-
          min(out_f_str[which(out_f_str$prop_stay ==
            prop_stay_prof[j, "prop_stay"]), metric_f])
      }
      prop_stay_prof <-
        prop_stay_prof[order(prop_stay_prof$prop_stay), ]

      # profile for ret_time
      ret_time_prof <- data.frame(ret_time = unique(out_f_str$ret_time))
      for (j in 1:nrow(ret_time_prof)) {
        ret_time_prof[j, metric_f] <-
          min(out_f_str[which(out_f_str$ret_time ==
            ret_time_prof[j, "ret_time"]), metric_f])
      }
      ret_time_prof <- ret_time_prof[order(ret_time_prof$ret_time), ]
      
    # Smooth profiles
      # prop_stay
      # smooth <- smooth.spline(x = prop_stay_prof$prop_stay,
      #   y = prop_stay_prof[, metric_f], spar = 0.9)
      # prop_stay_prof$smooth <- predict(smooth, x = prop_stay_prof$prop_stay)$y

      smooth <- loess(as.formula(paste(metric_f, " ~ ", "prop_stay", sep = "")),
        data = prop_stay_prof, span = 0.70, degree = 2)
      prop_stay_prof$smooth <- predict(smooth, x = prop_stay_prof$prop_stay)
      
      # ret_time
      # smooth <- smooth.spline(x = ret_time_prof$ret_time,
      #   y = ret_time_prof[, metric_f], spar = 0.9)
      # ret_time_prof$smooth <- predict(smooth, x = ret_time_prof$ret_time)$y

      smooth <- loess(as.formula(paste(metric_f, " ~ ", "ret_time", sep = "")),
        data = ret_time_prof, span = 0.70, degree = 2)
      ret_time_prof$smooth <- predict(smooth, x = ret_time_prof$ret_time)
      
    # Compute estimates for prop_stay
      # best estimate
      best <- prop_stay_prof[which.min(prop_stay_prof$smooth), "prop_stay"]
      
    if (metric_f == "negll") {  
      # take lower branch of likelihood profile, go up half a Chi-squared
      prop_stay_prof_lower <- subset(prop_stay_prof, prop_stay < best)
      ll_lci <- min(prop_stay_prof$smooth) + qchisq(p = 0.95, df = 1) / 2
      lci <- try(approx(x = prop_stay_prof_lower$smooth, 
        y = prop_stay_prof_lower$prop_stay, xout = ll_lci)$y, silent = TRUE)
      if (inherits(lci, "try-error")) {
        warning(paste("WARNING: for stratum ", i, 
          " no lower branch of prop_stay profile", sep = "") );
        lci <- NA}

      # take upper branch of likelihood profile, go up half a Chi-squared
      prop_stay_prof_upper <- subset(prop_stay_prof, 
        prop_stay >= best)
      ll_uci <- min(prop_stay_prof$smooth) + qchisq(p = 0.95, df = 1) / 2
      uci <- try(approx(x = prop_stay_prof_upper$smooth, 
        y = prop_stay_prof_upper$prop_stay, xout = ll_uci)$y, silent = TRUE)
      if (inherits(uci, "try-error")) {
        warning(paste("WARNING: for stratum ", i, 
          " no upper branch of prop_stay profile", sep = "") );
          uci  <- NA}
    }
    
    if (metric_f %in% c("negll_pen", "rmse") ) {lci <- NA; uci <- NA}  
        
      # collect estimates  
      out_all[which(out_all$stratum == i & out_all$parameter == "prop_stay"), 
        c("best", "lci_025", "uci_975")] <- c(best, lci, uci) 

      # plot profile
      if (nrow(prop_stay_prof) > 0) {  
        plots_out[[paste("proportion who never return (", i, ")", sep = "")]] <- 
          ggplot(data = prop_stay_prof, aes(x = prop_stay, 
            y = eval(as.name(metric_f)))) +
          geom_point(colour = palette_cb[4], alpha = 0.7) +
          geom_line(aes(y = smooth), colour = palette_cb[7], linewidth = 1) +
          theme_bw() +
          scale_x_continuous("proportion who never return") +
          scale_y_continuous(plot_y_lab)+
          ggtitle(i) +
          geom_vline(xintercept = c(best, lci, uci), lty = "21",
            linewidth = 0.75, colour = palette_cb[1])
      }
    
    # Compute estimates for ret_time
      # best estimate
      best <- ret_time_prof[which.min(ret_time_prof$smooth), "ret_time"]

    if (metric_f == "negll") {  
      # take lower branch of likelihood profile, go up half a Chi-squared
      ret_time_prof_lower <- subset(ret_time_prof, ret_time < best)
      ll_lci <- min(ret_time_prof$smooth) + qchisq(p = 0.95, df = 1) / 2
      lci <- try(approx(x = ret_time_prof_lower$smooth, 
        y = ret_time_prof_lower$ret_time, xout = ll_lci)$y, silent = TRUE)
      if (inherits(lci, "try-error")) {
        warning(paste("WARNING: for stratum ", i, 
          " no lower branch of ret_time profile", sep = "") );
        lci <- NA}

      # take upper branch of likelihood profile, go up half a Chi-squared
      ret_time_prof_upper <- subset(ret_time_prof, 
        ret_time >= best)
      ll_uci <- min(ret_time_prof$smooth) + qchisq(p = 0.95, df = 1) / 2
      uci <- try(approx(x = ret_time_prof_upper$smooth, 
        y = ret_time_prof_upper$ret_time, xout = ll_uci)$y, silent = TRUE)
      if (inherits(uci, "try-error")) {
        warning(paste("WARNING: for stratum ", i, 
          " no upper branch of ret_time profile", sep = "") );
          uci  <- NA}
    }
    
    if (metric_f %in% c("negll_pen", "rmse") ) {lci <- NA; uci <- NA}  

      # collect estimates  
      out_all[which(out_all$stratum == i & out_all$parameter == "ret_time"), 
        c("best", "lci_025", "uci_975")] <- c(best, lci, uci) 

      # plot profile
      if (nrow(ret_time_prof) > 0) {  
        plots_out[[paste("mean time to return (", i, ")", sep = "")]] <- 
          ggplot(data = ret_time_prof, aes(x = ret_time, 
            y = eval(as.name(metric_f)))) +
          geom_point(colour = palette_cb[6], alpha = 0.7) +
          geom_line(aes(y = smooth), colour = palette_cb[7], linewidth = 1) +
          theme_bw() +
          scale_x_continuous("mean time to return (months)") +
          scale_y_continuous(plot_y_lab)+
          ggtitle(i) +
          geom_vline(xintercept = c(best, lci, uci), lty = "21",
            linewidth = 0.75, colour = palette_cb[1])
      }
  }

  #.........................................
  ## Return output
    
    # Combined plot
    plot <- ggarrange(plotlist = plots_out, nrow = 2, ncol = 3,labels = NA)
    
    # List to return
    out_list <- list(out_all, plot)
    names(out_list) <- c("est", "plot")
    return(out_list)
}


#...............................................................................
### ENDS
#...............................................................................