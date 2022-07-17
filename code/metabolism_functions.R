#'
#'
#'
F_to_C = function(fahr){
  (fahr - 32) * 5/9
}


#'
#'
named_group_split = function (.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), 
                                              sep = " / ")))
  grouped %>% group_split() %>% rlang::set_names(names)
}

#'
#'
#'
merge_temps = function(o2Df = NULL, tempDf = NULL, tempCol = 'temp_c', maxDiff = 2){
  
  o2_timeCol = do.call("c",o2Df[,sapply(o2Df, function(x) inherits(x, 'POSIXt'))])
  temp_timeCol = do.call("c",tempDf[,sapply(tempDf, function(x) inherits(x, 'POSIXt'))])
  
  hourDiff = sapply(o2_timeCol, function(x) temp_timeCol[which.min(abs(temp_timeCol-x))]-x)
  hourDiff_mod = sapply(hourDiff, function(x) ifelse(x > maxDiff,NA,x))
  timeDiff = sapply(o2_timeCol, function(x) which.min(abs(temp_timeCol-x)))
  tempFill = unlist(tempDf[timeDiff,tempCol])
  
  return(tempFill)
}


#'
#'
#'
estimateDuskDawn = function(df = NULL,...){
  require(plyr)
  require(dplyr)
  require(magrittr)
  date_timeCol = names(df[sapply(df, function(x) inherits(x, "POSIXt"))])
  # dayCol = names(df[sapply(df, function(x) inherits(x, "difftime"))])
  day = 'day'
  # set the variables for day of experiment, time of day and 
  df = df %>% ungroup #%>%
  #   dplyr::mutate(date = as.Date(date_time),
  #                 !!day := (julian(as.Date(!!as.symbol(date_timeCol), tz=attr(df[[date_timeCol]],"tzone"))) - min(julian(as.Date(!!as.symbol(date_timeCol), tz=attr(df[[date_timeCol]],"tzone"))))+1),
  #                 TOD = case_when(lubridate::hour(!!as.symbol(date_timeCol)) < 12 ~ "morning",
  #                                 TRUE ~ "evening")) %>%
  #   group_by(day, TOD) %>%
  #   dplyr::mutate(!!as.symbol(date_timeCol) := sort(!!as.symbol(date_timeCol)),
  #                 time_point = 1:n())
  
  # test that >1 day of data exist for mornings
  if(length(unique(df[which(grepl("morning",df[["TOD"]])),df[[day]]])) <= 1) stop("GPP calculations require >1 day of oxygen measurements")
  
  # Calculate NP
  dayVec = unique(df[[day]])
  morningDf = df[which(df[["time_point"]] == 1 & df[["TOD"]] == "morning"),]
  NP_mg_o2_l_hr = unlist(sapply(dayVec[-1], function(x) morningDf[which(morningDf[[day]] == x),"o2_do_mg_l"] - morningDf[which(morningDf[[day]]== (x-1)), "o2_do_mg_l"]))/unlist(sapply(dayVec[-1], function(x) morningDf[which(morningDf[[day]] == x), date_timeCol]-morningDf[which(morningDf[[day]] == (x-1)), date_timeCol]))
  # calculate R @ night
  nightDf = df[which(df[['TOD']] == 'evening'),]
  nightDf = nightDf[which(nightDf[['time_point']] == max(nightDf[['time_point']])),]
  Rnight_mg_o2_l_hr = unlist(sapply(dayVec[-length(dayVec)], function(x) nightDf[which(nightDf[[day]] == x), "o2_do_mg_l"] - morningDf[which(morningDf[[day]] == (x+1)),"o2_do_mg_l"]))/unlist(sapply(dayVec[-length(dayVec)], function(x) nightDf[which(nightDf[[day]] == x), date_timeCol] - morningDf[which(morningDf[[day]] == (x+1)), date_timeCol]))
  
  # estimate GP as the sum of NP & R
  
GP_mg_o2_l_hr = NP_mg_o2_l_hr + (abs(Rnight_mg_o2_l_hr))
  
  # return this list of GP, NP, and R estimates
  return(list(NP_mg_o2_l_hr = NP_mg_o2_l_hr,
              Rnight_mg_o2_l_hr = Rnight_mg_o2_l_hr,
              GP_mg_o2_l_hr = GP_mg_o2_l_hr))
}


#'
#'
# covariance matrix of outcomes
cor_matrix <- function(x, r, v = rep(1, length(x)), na.rm = FALSE) {
  mat <- diag(v)
  se <- sqrt(v)
  se[is.na(se)] <- 0
  if (length(x) > 1L) {
    for (i in 2:nrow(mat)) {
      for (j in 1:(i-1)) {
        if (x[i] == x[j]) {
          mat[i, j] <- mat[j, i] <- r * se[i] * se[j] 
        }
      }
    }
  }
  dimnames(mat) <- list(1:nrow(mat), 1:ncol(mat))
  if (na.rm) {
    keep <- !is.na(diag(mat))
    mat <- mat[keep, keep]
  }
  mat
}
