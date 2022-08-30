#IRV分析の関数
irv <- function(x, na.rm = TRUE, split = FALSE,q.length) {
  num.split <- length(q.length)
  #必要な問題の長さを1から順番にベクトルとして返す関数
  rep.func <- function(q.length){
    i=1
    vec=c()
    while(i<num.split+1){
      vec <- append(vec,rep(i,q.length[i]))
      #print(i,q.length$count[i])
      i=i+1
    }
    return(vec)
  }
  out <- apply(x, 1, stats::sd, na.rm = na.rm)
  
  if(split == TRUE) {
    chunk <- function(x,n) split(x, rep.func(q.length))
    split_x <- apply(x, 1, chunk, num.split)
    out_split <- t(replicate(nrow(x), rep(NA, num.split)))
    colnames(out_split) <- paste0("irv",1:num.split)
    for(k in 1:nrow(out_split)) {
      split_x_single <- split_x[[k]]
      #リストに対してそれぞれのブロックでの標準偏差を出してリストを外している
      #行に対して追加していく
      out_split[k,] <- unlist(lapply(split_x_single, stats::sd, na.rm = na.rm), use.names = FALSE)
    }
    out_split <- data.frame(out, out_split)
    colnames(out_split)[1] <- "irvTotal"
    return(out_split)} else { #split subsection end
      return(out)
    }
}