#必要なライブラリのインポート
library(tidyverse)

#データのインポート
teams=read_csv("data/teams.csv")
boxscore2019=read_csv("data/games_boxscore_201920.csv")
boxscore2020=read_csv("data/games_boxscore_202021.csv")

#teamsから2019-20シーズンのB1のデータだけ抽出
teams2019=teams %>% filter(Season=="2019-20") %>% filter(League=="B1")

#teamsから2020-21シーズンのB1のデータだけ抽出
teams2020=teams %>% filter(Season=="2020-21") %>% filter(League=="B1")

#boxscore2019からB1のデータだけ抽出
boxscore2019=teams2019 %>% left_join(boxscore2019,by="TeamId")

#boxscore2020からB1のデータだけ抽出
boxscore2020=teams2020 %>% left_join(boxscore2020,by="TeamId")

#選手ごとに平均スタッツを算出する
boxscore2019=boxscore2019 %>% select(Player,MIN,EFF) %>% 
  group_by(Player) %>% summarise(MIN_mean_2019=mean(MIN),EFF_mean_2019=mean(EFF))

boxscore2020=boxscore2020 %>% select(Player,MIN,EFF) %>% 
  group_by(Player) %>% summarise(MIN_mean_2020=mean(MIN),EFF_mean_2020=mean(EFF))

#boxscore2019と2020を選手名で結合する
data=boxscore2019 %>% left_join(boxscore2020,by="Player") %>% na.omit()

#EFFの2019と2020の差分をとる
data=data %>% mutate(EFF_diff=EFF_mean_2020-EFF_mean_2019)

#出場時間の2019と2020の差分をとる
data=data %>% mutate(MIN_diff=MIN_mean_2020-MIN_mean_2019)

#可視化する
data %>% ggplot(aes(x=MIN_diff,y=EFF_diff))+
  geom_point()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)+
  ggtitle("選手の成長")+
  ylab("EFFの差分")+
  xlab("MINの差分")

#EFF_diffとMIN_diffの積をとって、散布図上で右上にいる選手を探す
data=data %>% mutate(Dev=EFF_diff*MIN_diff)
top10=data %>% filter(EFF_diff>=0) %>% arrange(desc(Dev)) %>% head(10)
top10