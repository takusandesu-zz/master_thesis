#マハラノビス距離
mahad <- function(x, plot = TRUE, flag = FALSE, confidence = 0.99, na.rm = TRUE) {
  if(na.rm == FALSE) {
    if(any(is.na(x)) == TRUE) {stop("Some values are NA. Mahalanobis distance was not computed.
                                      Use na.rm = TRUE to use available cases.", call. = FALSE)}
  }
  #remove rows with all NA and issue warning
  complete.na <- apply(x, 1, function(y) { all(is.na(y)) } )
  if(any(complete.na)) {
    warning("Some cases contain only NA values. The Mahalanobis distance will be calculated using available cases.",
            call. = FALSE) }
  #NAがある行が削除され、残ったデータフレーム
  x_filtered <- x[!complete.na,]
  
  maha_data <- as.numeric(psych::outlier(x_filtered, plot, bad = 0, na.rm = na.rm))
  d_sq <- rep_len(NA, nrow(x_filtered))#NAを人数分用意するベクトル
  d_sq[!complete.na] <- maha_data#TRUEであるベクトルだけにマハラビノス距離を入れる
  
  #外れ値にはフラグをつける
  if(flag == TRUE) {
    cut <- stats::qchisq(confidence, ncol(x))#分位点関数　　累積確率密度と自由度を入力
    flagged <- (d_sq > cut)
    return(data.frame(d_sq = d_sq, flagged = flagged))
  }
  else{ return(d_sq) }
}