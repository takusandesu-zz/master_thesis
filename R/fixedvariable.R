#固定された変数
#jacsis2020：jacsis2020の生データ
#jacsis_ipw：アンケート回答者に対する重み
#colname.start3：項目の頭3文字が入ったベクトル
#colname.data：どの問題が何項目を含んでいるかのデータ


jacsis2020 <- read_csv("../data/ローデータ_1of1.csv",locale=locale(encoding="CP932"),skip = 1)
jacsis_ipw <- read_csv("../data/JACSIS20IPW20201020.csv",locale=locale(encoding="CP932"))

#項目に対して何問の問題があるか
colname.start3 <- colnames(jacsis2020) %>% str_sub(start = 1,end=3) 

colname.data <- data.frame(ID=c(1:length(colname.start3)),colname=colname.start3) %>%
  group_by(colname) %>% 
  summarise(count=n() , .groups="drop") %>% 
  arrange(desc(count))