#psychsynの関数
psychsyn <- function(x, critval=.60, anto=FALSE, diag=FALSE, resample_na=TRUE) {
  x <- as.matrix(x)
  item_pairs <- get_item_pairs(x, critval, anto)
  
  synonyms <- apply(x,1,syn_for_one, item_pairs, resample_na)
  synonyms_df <- as.data.frame(aperm(synonyms))
  colnames(synonyms_df) <- c("numPairs", "cor")
  
  if(diag==TRUE) { return(synonyms_df) }
  else { return(synonyms_df$cor) }
}

# Helper function that identifies psychometric synonyms in a given dataset
get_item_pairs <- function(x, critval=.60, anto=FALSE) {
  critval <- abs(critval) #Dummy Proofing
  
  correlations <- stats::cor(x, use = "pairwise.complete.obs")#共分散行列
  correlations[upper.tri(correlations, diag=TRUE)] <- NA #対角線を含めた上三角行列で上側がTRUEとなりそこをNAとする
  correlations <- as.data.frame(as.table(correlations)) #テーブル型にしてデータ型にする var1 va2 Freqという列ができる
  
  # Identifying item pairs differs depending on whether the user wants
  # Psychometric Synonyms or Psychometric Antonyms
  if(anto==FALSE) {
    item_pair_names <- correlations[which(correlations$Freq > critval, arr.ind=TRUE),c(1,2)]#var1とvar2の組み合わせを出力
    if(nrow(item_pair_names)==0) {
      stop("No Psychometric Synonyms found.")
    }
  }
  else if(anto==TRUE) {
    item_pair_names <- correlations[which(correlations$Freq < -critval, arr.ind=TRUE),c(1,2)]
    if(nrow(item_pair_names)==0) {
      stop("No Psychometric Antonyms found.")
    }
  }
  
  matches <- item_pair_names
  return(matches)#var1とvar2のデータフレームが返される 相関が0.6以上の列名のペア
}

# Helper function to calculate the within person correlation for a single individual
syn_for_one <- function(x, item_pairs, resample_na) {#一人に対する演算
  item_pairs_omit_na <- which(!(is.na(x[item_pairs[,1]]) | is.na(x[item_pairs[,2]])))#whichでペアの列にNAがないインデックス番号をベクトルで返す
  sum_item_pairs <- length(item_pairs_omit_na)
  #only execute if more than two item pairs
  if(sum_item_pairs > 2) {
    #ある人に対する問題ペアの列ができる
    itemvalues <- cbind(as.numeric(x[as.numeric(item_pairs[,1])]), as.numeric(x[as.numeric(item_pairs[,2])]))#列の名前を列の番号に変換
    
    # helper that calculates within-person correlation
    psychsyn_cor <- function(x) {
      suppressWarnings(stats::cor(x, use = "pairwise.complete.obs", method = "pearson")[1,2])
    }
    
    # if resample_na == TRUE, re-calculate psychsyn should a result return NA
    if(resample_na == TRUE) {
      counter <- 1
      synvalue <- psychsyn_cor(itemvalues)
      while(counter <= 10 & is.na(synvalue)) {
        itemvalues <- t(apply(itemvalues, 1, sample, 2, replace = F))
        synvalue <- psychsyn_cor(itemvalues)
        counter = counter+1
      }
    } else {
      synvalue <- psychsyn_cor(itemvalues) # executes if resample_na == FALSE
    }
    
  } else {synvalue <- NA} # executes if insufficient item pairs
  
  return(c(sum_item_pairs, synvalue))
}