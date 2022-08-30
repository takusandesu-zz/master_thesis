#項目に対して何問の問題があるか
item.leng.count <- function(v){
colname.start3 <- colnames(v) %>% str_sub(start = 1,end=3) 
colname.data <- data.frame(ID=c(1:length(colname.start3)),colname=colname.start3) %>%
  group_by(colname) %>% 
  summarise(count=n() , .groups="drop") %>% 
  arrange(desc(count))
return(colname.data)
}