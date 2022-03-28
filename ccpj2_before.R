# Memory release & Read corrective library
rm(list=ls()); gc();  gc();
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, pwr, hrbrthemes, rlang, gt, patchwork, tidymodels)


# Sample Size
## t-test
pwr.t.test(sig.level = 0.05, power = 0.8, type = "two.sample", n = NULL, d = 0.5)
## paired t-test
pwr.t.test(sig.level = 0.05, power = 0.8, type = "paired", n = NULL, d = 0.5, alternative = "two.sided")
## anova test
pwr.anova.test(k = 6, f = 0.25, power = 0.8, sig.level = 0.05)
## TurkeyHD


# Read Datatable
library(tidyverse)
dat_b <- read_csv("/Users/kawaiyuusei/Documents/03_2021_Minacare/rawdat/dat_b_ver2.0.csv") 

# Oversee data frame
dim(dat_b)
glimpse(dat_b)
str(dat_b)

# Arrange data frame
colnamelist <- c("h1_読めない漢字がある" = "読めない漢字がある",
                 "h1_字が細かくて読みにくい" = "字が細かくて読みにくい", 
                 "h1_内容が難しくて分かりにくい" = "内容が難しくて分かりにくい", 
                 "h1_読むのに時間が掛かる" = "読むのに時間が掛かる",
                 "h1_誰かに代わりに読んでもらうことがある" = "誰かに代わりに読んでもらうことがある", 
                 "h2_いろいろなところから情報を集める" = "いろいろなところから情報を集める", 
                 "h2_たくさんある情報から自分が求めるものを選び出す" = "たくさんある情報から自分が求めるものを選び出す", 
                 "h2_自分が見聞きした情報を理解できる" = "自分が見聞きした情報を理解できる", 
                 "h2_病気についての自分の意見や考えを医師や身近なひとに伝える" = "病気についての自分の意見や考えを医師や身近なひとに伝える", 
                 "h2_見聞きした情報をもとに実際に生活を変えてみる" = "見聞きした情報をもとに実際に生活を変えてみる", 
                 "h3_自分にもあてはまるかどうか考えた" = "自分にもあてはまるかどうか考えた", 
                 "h3_信頼性に疑問をもった" = "信頼性に疑問をもった", 
                 "h3_正しいかどうか聞いたり調べたりした" = "正しいかどうか聞いたり調べたりした", 
                 "h3_病院や治療法などを自分で決めるために調べた" = "病院や治療法などを自分で決めるために調べた"
                 )

colnamelist2 <-c(  "01_workStyle" = "１．あなたの現在の働き方について、最も当てはまるものを教えてください", 
                   "02_workTime" = "２．あなたが現在、主に働いている時間帯を教えてください（複数回答可）", 
                   "03_co-residence" = "３．あなたが現在、同居されている方を全てお答えください（複数回答可）", 
                   "04_consultWith" = "４．健康課題について、主に話し相手・相談相手になる方を全てお答えください（複数回答可）", 
                   "05_easyToSpeak" = "５．現在のあなたの職場環境は健康課題について話しやすいと感じますか？",
                   "06_healthState" = "６．あなたの現在の健康状態を教えてください", 
                   "07_women" = "７．女性のライフステージにおける女性特有の心や体の変化について聞いたことがありますか？", 
                   "08_women" = "８．女性の健康課題について、関心があることをお答えください（複数回答可）",  
                   "09_women" = "９．女性特有の健康課題を周りの人と話したことがありますか？", 
                   "10_women" = "１０．各ライフステージにおける女性の心や体の変化について、悩んでいる女性が身近にどの程度いると思いますか？", 
                   "11_h1" = "１１．病院や薬局からもらう説明書やパンフレットなどを読む際、下記の項目について、あなたはどのように考えていますか？",  
                   "12_h2" = "１２．ある病気と診断されてから、その病気やその治療・健康法について、下記の項目について、あなたはどのように考えていますか？", 
                   "13_h3" = "１３．ある病気と診断されてから、その病気やその治療・健康法に関することで、自分が見聞きした知識や情報について、下記の項目について、あなたはどのように考えていますか？", 
                   "14_healthCheck" = "１４．企業健診（扶養者宛てに会社から案内される健診）は受けていますか？" , 
                   "15_reasonHealthCheck" = "１５．（設問１４でほとんど受けていない、全く受けたことがないと回答した方）健診を受けたくない、受けられない理由について、あてはまるものを全てお答えください（複数回答可）", 
                   "16_medicalCheck" = "１６．通常、何か心や身体の不調を感じたとき、あなたは病院に行きますか？", 
                   "17_reasonMedicalCheck" = "１７．（設問ほとんど受診しない、全く受診しないと回答した方）病院に行きたくない、行けない理由について、あてはまるものを全てお答えください（複数回答可）", 
                   "18_information1" = "1番目をお答えください", 
                   "18_information2" = "2番目をお答えください", 
                   "19_women" = "１９．現在、女性特有の不調を感じていますか？", 
                   "20_women" = "２０．（設問１９ではいと回答した方）現在の気になる症状（女性特有の不調）を教えてください。",  
                   "21_women" = "２１．（設問１９ではいと回答した方）現在の気になる症状（女性特有の不調）について、医師には相談をしていますか？", 
                   "22_exercise" = "２２．運動に関して", 
                   "23_diet" = "２３．食事に関して", 
                   "24_prevention" = "２４．予防（うがい、手洗いなど自宅でできるもの）", 
                   "25_vitalSign" = "２５．血圧、体重などのチェック", 
                   "age" = "年齢", 
                   "sex" = "性別", 
                   "name" = "氏名")


dat_b <- dat_b %>% rename(!!! colnamelist)　# Literacyに関わる列の列名の変更
dat_b <- dat_b %>% rename(!!! colnamelist2) # Change colname for Question

## Rename worktime columns
workTime <- dat_b %>% select(13:23) %>% names() %>% dput()
workTime_list<- str_c("02_", workTime, sep = "") %>% dput()
dat_b <- dat_b %>% rename_at(c(13:23), ~workTime_list)

## Insertion number into head strings
changecol <- function(x,y,z){
  dat_b %>% 
    rename_at(c(x:y), ~str_c(z, ., sep = ""))
}
dat_b <- changecol(25,31,"03_") ## rename co-residence columns
dat_b <- changecol(33,41,"04_") ## rename consultWith columns
dat_b <- changecol(46,55,"08_") ## rename women's issue columns
dat_b <- changecol(77,88,"15_") ## rename reasonHealthCheck columns
dat_b <- changecol(91,102,"17_") ## rename reasonMedicalCheck columns
dat_b <- changecol(107,113,"20_") ## rename women's current issue columns

## remove long string of column name
dat_b %>% select(23,31,41,55,88,102,113) %>% names()
dat_b <- dat_b %>% rename_at(c(23,31,41,55,88,102,113), ~str_sub(., start = 1, end = 6))

## write csv
dat_b %>% write_csv("/Users/kawaiyuusei/Documents/03_2021_Minacare/rawdat/dat_b_ver3.csv")

## change data type of data frame
list_n = dat_b %>% select(59:63, 65:69, 71:74) %>% names() %>% dput()
dat_b <- dat_b %>% mutate(across(all_of(list_n), as.integer)) # healthliteracyをint typeへ

list_f = dat_b %>% select(
  c(8,11,42,43,44,56,57,75,89,103,104,105,114:118)
  ) %>% names() %>% dput()
dat_b <- dat_b %>% mutate(across(all_of(list_f), as.factor)) ## to factor type

## ボツコード
#survey_b <- survey_b %>% mutate_all( ~str_replace_all(., pattern = list))  # 数字への置換


## Add cols for total health literacy
sum_h <- function(n){
  rowSums(across(starts_with(n)), na.rm = TRUE) }
dat_b <- dat_b %>%
  mutate(total_h1 = sum_h("h1"), total_h2 = sum_h("h2"), total_h3 = sum_h("h3"), total_h4 = sum_h("total")) 

## output csv data
dat_b %>% 
  write_csv( "/Users/kawaiyuusei/Documents/03_2021_Minacare/rawdat/dat_b_ver4.csv")


# Oversee Data for detail
## ベースライン（年齢、性別）
# plot01 <- 
plot01 <- dat_b %>% group_nest(Collector) %>%
  mutate("平均年齢" = map_dbl(data, ~mean(.$age,na.rm = TRUE)),
         '男性'　= map_dbl(data, ~length(.$sex[.$sex == "男性"])),
         '女性'　= map_dbl(data, ~length(.$sex[.$sex == "女性"]))
         ) %>% select(1, 3:5) 
plot011 <- tibble(Collector = "保険者全体",
       "平均年齢" = mean(dat_b$age),
       "男性" = sum(plot01$男性),
       "女性" = sum(plot01$女性))
plot012 <- bind_rows(plot01,plot011) 
plot012 %>% gt() %>%
  fmt_number(
    columns = c("平均年齢"),
    decimals = 1,
    use_seps = FALSE
  )　%>%
    tab_spanner(
      label = md("**参加者数**"),
      columns = c("男性", "女性")) %>%
        tab_options(table.width = 400)  %>%
  cols_label(Collector = "保険者名" ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "top",
      color = "gray",
      weight = px(2)
    ),
    locations = 
      cells_body(
        columns = everything(),
        rows = 7
      )
    ) %>%
  tab_options(table_body.hlines.color = "white",
              table_body.border.bottom.color = "white",
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "white") 


## ベースライン（年齢分布）
plot02 <- dat_b %>% ggplot() +
  aes(x = age, fill = sex)　+
  geom_histogram(stat = "count", bins = 50) +
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  labs(fill = "性別", x = "年齢", y = "人数") +
  scale_fill_grey(start = 0.4, end = 0.7) 

plot01 / plot02

  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/05_plot/plot.png")

## workTime
dat_b %>% select(starts_with("02_")) %>% skimr::skim()
dat_c <- dat_b %>% select(starts_with("02_")) %>% select(where(is.numeric)) 
dat_c %>% map_int(~length(.[. == "1"])) %>% tibble() %>% 
  mutate("worktime" = names(dat_c),
         "worktime" = str_replace(worktime, pattern = "02_", replacement = "")
         )  %>% arrange_if(is.integer, desc) %>% #arrangeはなくてよい
  ggplot() + aes(x= reorder(worktime, .), y=., fill = worktime) +
  geom_bar(stat="identity", width = .8, alpha = .7, fill = "#FFB72B") +
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  coord_flip()+
  labs(title =""  , y="人")+
  theme(legend.position = "none",
        axis.title.y = element_blank()) +
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/05_plot/plot05.png",
         bg = "transparent")

## co-residents
dat_d <- dat_b %>% 
  select(starts_with("03_")) %>% 
  select(where(is.numeric))
dat_d %>% map_int(
  ~length(.[. == "1"])) %>% tibble() %>%
  mutate("co-resident" = names(dat_d),
         "co-resident" = str_replace(`co-resident`, 
                                     pattern = "03_", 
                                     replacement = "")) %>%
  ggplot() + aes(x=reorder(`co-resident`, .), y=., fill=`co-resident`) +
  geom_bar(stat = "identity", alpha = .7, width = .7)+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  scale_x_discrete(labels=c("配偶者・パートナー027" = "配偶者・パートナー",
                            "子供029" = "子供",
                            "同居はしていない（単身）" = "単身",
                            "親（義両親を含む）" = "両親",
                            "兄弟姉妹028" = "兄弟姉妹")) +
  # scale_fill_brewer(palette = "Spectral")+
  scale_fill_brewer(palette = "Oranges") +
  coord_flip()+
  labs(y="人")+
  theme(legend.position = "none",
        axis.title.y = element_blank())+
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/05_plot/plot04.png",
         bg = "transparent")

## consultWith
dat_e <- dat_b %>%
  select(starts_with("04_"))%>% 
  select(where(is.numeric))

plot03 <- 
  dat_e %>% map_int(
  ~length(.[. == "1"])) %>% tibble() %>%
  mutate("consultWith" = names(dat_e),
         "consultWith" = str_replace(`consultWith`, 
                                     pattern = "04_", 
                                     replacement = "")) %>%
  ggplot() + aes(x=reorder(`consultWith`, .), y=., fill=`consultWith`) +
  scale_x_discrete(labels=c("配偶者・パートナー034" = "配偶者・パートナー",
                            "子供037" = "子供",
                            "兄弟姉妹035" = "兄弟姉妹")) +
  geom_bar(stat = "identity", 
           fill = "#FFB72B", 
           alpha=.8,
           width = .8)+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  coord_flip()+
  labs(y="人")+
  theme(legend.position = "none",
        axis.title.y = element_blank()) 

## easyToSpeak distribution
plot04 <-
  dat_b %>% ggplot() +
  aes(x=`05_easyToSpeak`, fill = `05_easyToSpeak`) +
  geom_bar(stat = "count", width =0.5, fill = "#FFB72B", alpha = .8)+
  scale_x_discrete(labels = c("1" = "全く思わない", 
                            "2" = "あまり思わない", 
                            "3" = "やや思う", 
                            "4" = "強く思う"
    ))+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  labs(y = "人") +
  coord_flip() +
  theme(legend.position = "none",
        axis.title.y = element_blank())

plot03 + plot04  

## interests for womens health issue
dat_f <- dat_b %>% select(starts_with("08_")) %>%
  select(where(is.numeric))

plotwi<- 
dat_f %>% map_int(
  ~length(.[. == "1"])) %>% tibble() %>%
  mutate("womenIssue" = names(dat_f),
         "womenIssue" = str_replace(`womenIssue`, 
                                     pattern = "08_", 
                                     replacement = "")) %>%
  ggplot() + aes(x=reorder(`womenIssue`, .), y=., fill=`womenIssue`) +
  geom_bar(stat = "identity", width = .7, alpha=.8, fill ="#FFB72B")+
  scale_x_discrete(labels = 
                     c(
                       "子宮や卵巣などの病気（がんを除く）" = "子宮や卵巣などの病気",
                       "職場における女性の健康に配慮した環境の整備（相談窓口、研修会、特別休暇制度など）"="職場における女性の健康に配慮した環境の整備",
                       "健康診断・検診（がんなど特定の病気を見つける検査）の種類や実施機関" = "健康診断・検診の種類や実施機関"
  ))+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  coord_flip()+
  labs(y="人")+
  theme(legend.position = "none",
        axis.title.y = element_blank())
+
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/plot08.png",
         bg = "transparent")


## Do you talk about women's issue to the peaople around you?
plot091 <- dat_b %>% ggplot() +
  aes(x=`09_women`, fill = `09_women`) +
  scale_x_discrete(labels = c(
    "1" = "話したことはない",
    "2" = "話したことがある",
    "3" = "ときどき話す",
    "4" = "よく話す"
    ))+
  geom_bar(stat = "count", width =0.5, fill = "#FFB72B", alpha = .8)+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
#  scale_fill_ipsum(labels = c("1 = 話したことはない", "2 = 話したことがある", "3 = ときどき話す", "4 = よく話す"))+
  labs(y = "人", fill = "女性健康課題") +
  coord_flip()+
  theme(legend.position = "none",
        axis.title.y = element_blank())

plot091 + ggsave(filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/05_plot/plot09.png",
                 bg = "transparent")

plot092 <- dat_b %>% ggplot() +
  aes(x=`09_women`, fill = `sex`) +
  geom_bar(stat = "count", width =0.5)+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  scale_fill_ipsum()+
  labs(y = "人", fill = "女性健康課題")

plot091 / plot092

## Do you think how many women being worried about women's specific issue
plot101 <- dat_b %>% ggplot() +
  aes(x=`10_women`, fill = `10_women`) +
  geom_bar(stat = "count", width =0.5)+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  scale_fill_ipsum(labels = c("1 = 全くいないと思う", "2 = あまりいないと思う", "3 = 少しいると思う", "4 = たくさんいると思う"))+
  labs(y = "人", fill = "女性健康課題")

plot102 <- dat_b %>% ggplot() +
  aes(x=`10_women`, fill = `sex`) +
  geom_bar(stat = "count", width =0.5)+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  scale_fill_ipsum()+
  labs(y = "人", fill = "女性健康課題")

plot101 / plot102

## Are you wiling to go to hospital ?
table(dat_b$`16_medicalCheck`) %>% tibble() %>%
  mutate("willingness" = c(
    "全く受信しない",
    "ほとんど受信しない",
    "ときどき受診する",
    "すぐに受診する"
  )) %>% rename("number" = ".") %>% gt()

dat_g <- dat_b %>% select(starts_with("17_")) %>%
  select(where(is.numeric))
dat_g %>% map_int(
  ~length(.[. == "1"])) %>% tibble() %>%
  mutate("reason" = names(dat_g),
         "reason" = str_replace(`reason`, 
                                    pattern = "17_", 
                                    replacement = "")) %>%
  ggplot() + aes(x=reorder(`reason`, .), y=., fill=`reason`) +
  scale_x_discrete(labels = c(
    "仕事で忙しい097" = "仕事で忙しい",
    "費用が高い098" = "費用が高い",
    "育児・家事で忙しい095" = "育児・家事で忙しい",
    "介護で忙しい096" = "介護で忙しい"
  ))+
  geom_bar(stat = "identity", width = .7, alpha=.8, fill ="#FFB72B")+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  coord_flip()+
  labs(y="人")+
  theme(legend.position = "none",
        axis.title.y = element_blank())+
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/05_plot/plot17.png",
         bg = "transparent")

table(dat_b$`19_women`)

## 更年期でフィルタリングした後に
dat_b %>% filter(`19_women` == 1,
  `20_更年期に関連すると思われる不調` == 1,
                 `16_medicalCheck` == 1 | `16_medicalCheck` == 2) %>%
  select(starts_with("17_")) %>%
  select(where(is.numeric)) %>% map_int(
    ~length(.[. == "1"]))%>% tibble() %>%
  mutate("reason" = names(dat_g),
         "reason" = str_replace(reason,
                                pattern = "17_",
                                replacement = "")) %>%
  mutate(ratio = round(./26, digits = 3)) %>%
  ggplot() + aes(x=reorder(reason, ratio), y=ratio*100, fill=reason) +
  geom_bar(stat = "identity", 
           fill = "orange", 
           alpha = .8,
           width = .8) +
  geom_text(aes(label = paste(format(ratio*100), "%")), vjust=.5, hjust=.5, color="black")+
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  scale_x_discrete(label = c("仕事で忙しい097" = "仕事で忙しい",
                             "費用が高い098" = "費用が高い",
                             "育児・家事で忙しい095" = "育児・家事で忙しい",
                             "介護で忙しい096" = "介護で忙しい")) +
  coord_flip() +
  labs(y="%") +
  theme(legend.position = "none",
        axis.title.y = element_blank())


## Women's specific problem
dat_b %>% group_nest(`19_women`) %>% 
  mutate("回答" = c("はい","いいえ","NA"),
         n = map_dbl(data, nrow)) %>% select(-1,-2) %>%
  gt() %>%
  tab_options(table.width = 200, 
              column_labels.background.color = "#FFC286") 

dat_j <- dat_b %>% select(starts_with("20_")) %>% select(where(is.numeric))

plot132 <- func_bar(dat_j,"20_")
plot132
ggsave(plot132 ,filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/plot13-2.png",
       bg = "transparent")

func_bar <- function(X,Y){
  X %>% map_int(
  ~length(.[. == "1"])) %>% tibble() %>%
  mutate("reason" = names(X),
         "reason" = str_replace(`reason`, 
                                pattern = Y, 
                                replacement = "")) %>%
  ggplot() + aes(x=reorder(`reason`, .), y=., fill=`reason`) +
  geom_bar(stat = "identity", width = .7, alpha=.8, fill ="#FFB72B")+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  coord_flip()+
  labs(y="人")+
  theme(legend.position = "none",
        axis.title.y = element_blank())
}

## health maintenance activity
dat_b %>% select(115:118) %>%
  pivot_longer(1:4, names_to = "activity", values_to = "state") %>%
  group_by(activity, state) %>%
  summarise(count=n()) %>%
  mutate(perc = count/sum(count)) %>%
  ggplot() +
  aes(x=factor(activity), y=perc*100, fill = state) +
  geom_bar(stat = "identity", width = 0.5 ) +
  labs(y="percent")+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  scale_fill_ipsum(labels = c(
    "開始するつもりはない",
    "6ヶ月以内に開始予定",
    "1ヶ月以内に開始予定",
    "開始済み半年未満",
    "開始して半年以上"
  ))+
  theme(axis.title.x = element_blank()) +
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/plot12.png",
         bg = "transparent")
  

# HL-score
## Summarise HL-score by Collector
dat_b %>% group_by(Collector) %>% nest() %>%
  mutate("平均値" = map_dbl(data, ~mean(.$total_h4)),
         "中央値" = map_dbl(data, ~median(.$total_h4)),
         "標準偏差" = map_dbl(data, ~sd(.$total_h4))) %>%
  select(1, 3:5) %>% ungroup() %>% gt() %>%   fmt_number(
              columns = c("平均値", "標準偏差"),
              decimals = 1,
              use_seps = FALSE
            )　%>%
  tab_options(table.width = 450, 
              column_labels.background.color = "#FFC286") %>%
  cols_label(Collector = "保険者名" )　


# boxplot HL-score by Collector
dat_b %>% filter(total_h4 >=25) %>%
  ggplot() +
  aes(x = Collector, y = total_h4, fill = Collector) +
  geom_boxplot(alpha = 0.5) +
  labs(x="保健者", y="ヘルスリテラシースコア", fill = "保険者") +
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  scale_fill_ipsum() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/plot23.png",
         bg = "transparent")

# Health literacy histogram
dat_b %>% ggplot() +
  aes(x = total_h4, fill = sex) +
  geom_histogram(stat = "count", bins = 100) +
  labs(x = "ヘルスリテラシースコア") +
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  scale_fill_grey(start = .3, end = .8) +
  labs(fill="性別")+
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/05_plot/plot10.png",
         bg = "transparent")
  

dat_b %>% ggplot() +
  aes(x = total_h4) +
  geom_histogram(stat = "count", bins = 100, fill="#F0A500", alpha=.8) +
  labs(x = "ヘルスリテラシースコア") +
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  scale_fill_brewer(palette = "Oranges")+
  labs(fill=NULL) +
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/05_plot/plot60.png",
         bg = "transparent", width = 8, height =3.5)


## Age vs Health literacy
dat_b %>% filter(total_h4 >= 25) %>% ggplot() +
  aes(x = age, y = total_h4) +
  geom_point(color = "royalblue", alpha = 0.5, size = 3)+
  labs(y = "ヘルスリテラシースコア")+
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/plot24.png",
         bg = "transparent")


t1 <- dat_b %>% filter(total_h4 >= 25) %>% ggplot() +
  aes(x = total_h1, y = total_h4) +
  geom_point(color = "#21325E", alpha = 0.5, size = 2)+
  geom_smooth(method = "lm", color = "salmon", alpha = 0.5)+
  labs(x= "機能的ヘルスリテラシー", y = "合計ヘルスリテラシー")+
  theme_ipsum(base_family = "HiraKakuPro-W3")

t2 <- dat_b %>% filter(total_h4 >= 25) %>% ggplot() +
  aes(x = total_h2, y = total_h4) +
  geom_point(color = "#21325E", alpha = 0.5, size = 2)+
  geom_smooth(method = "lm", color = "salmon", alpha = 0.5)+
  labs(x= "伝達的ヘルスリテラシー", y = "合計ヘルスリテラシー")+
  theme_ipsum(base_family = "HiraKakuPro-W3") 

t3 <- dat_b %>% filter(total_h4 >= 25) %>% ggplot() +
  aes(x = total_h3, y = total_h4) +
  geom_point(color = "#21325E", alpha = 0.5, size = 2)+
  geom_smooth(method = "lm", color = "salmon", alpha = 0.5)+
  labs(x= "批判的ヘルスリテラシー", y = "合計ヘルスリテラシー")+
  theme_ipsum(base_family = "HiraKakuPro-W3") 

t1 + t2 + t3

# healthState
table(dat_b$`06_healthState`) %>% tibble() %>% mutate() %>%
  mutate("healthState" = c("よくない", "あまりよくない", "ふつう", "まあよい", "よい")) %>%
  rename("number" = ".") %>% gt()

plot_HS <- function(a,b){
dat_b %>% filter(total_h4 >= 25,
                 `06_healthState`== c(a,b) ) %>%
  ggplot() + 
  aes(x = `06_healthState`, y = total_h4, fill = `06_healthState`) +
  geom_boxplot(alpha = 0.5,
               outlier.shape = NA) +
  labs(x=NULL, y="ヘルスリテラシースコア") +
    theme_ipsum(base_family = "HiraKakuPro-W3") +
    scale_fill_brewer(palette = "Oranges")+
    theme(legend.position = "none")
}

+
#  scale_fill_ipsum(labels = c("よくない", "あまりよくない", "ふつう", "まあよい", "よい"))+
#  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
 # ggsave(filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/plot27.png",
  #       bg = "transparent")

ploths25 <-  
plot_HS(2,5)+
  scale_x_discrete(labels = c(
    "2" = "あまりよくない",
    "5" = "よい"  )) +
  geom_signif(comparisons = list(c("2","5")),
              na.rm = FALSE,
              map_signif_level = TRUE,
              annotations = sig(0.015),
              y_position = 70,
              col ="black")
  
ploths35 <-  
plot_HS(3,5)+
  scale_x_discrete(labels = c(
    "3" = "ふつう",
    "5" = "よい"  )) +
  geom_signif(comparisons = list(c("3","5")),
              na.rm = FALSE,
              map_signif_level = TRUE,
              annotations = sig(0.001),
              y_position = 70,
              col ="black")

ploths25 + ploths35

  ## MCT between HL-score and health state
mct01 <- dat_b %>% 
  select(c(`06_healthState`, "total_h1", "total_h2", "total_h3", "total_h4"))
mctplot01 <- TukeyHSD(
  x = aov(mct01$total_h4 ~ mct01$`06_healthState`),
  conf.level = 0.95) 

mctplot01 %>% 
  tidy() %>% select(c(2,4,5:7)) %>% gt() %>%
  fmt_number(
    columns = c(2:5),
    decimals = 3,
    use_seps = FALSE) 

plot(mctplot01)

## health check
table(dat_b$`14_healthCheck`)


## boxplot between medicalcheck and HL-score 
table(dat_b$`16_medicalCheck`) %>%
  tibble() %>% mutate("medicalCheck" = c("1 = 受信しない", 
                                         "2 = ほとんど受信しない",
                                         "3 = 時々受診する",
                                         "4 = すぐ受診する")) %>%
  rename("number" = ".") %>% gt()

### generate * function
sig <- function(a) {
  if (a > 0.1) {
    return("")
  } else {
    if ((a <= 0.1)&&(a > 0.05)) {
      return(".")
    } else {
      if ((a <= 0.05)&&(a > 0.01)) {
        return("*")
      } else {
        if ((a <= 0.01)&&(a > 0.001)) {
          return("**")
        } else return("***")
      }
    }
  }
}

plot_MC <- function(a,b){ 
  dat_b %>% filter(total_h4 >= 25, `16_medicalCheck`== c(a,b) ) %>%
  ggplot() +
  aes(x = `16_medicalCheck`, y = total_h4, fill=`16_medicalCheck`) +
  geom_boxplot(alpha = 0.5,
               outlier.shape = NA)+
  labs(x=NULL, y="ヘルスリテラシースコア") +
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  scale_fill_brewer(palette = "Oranges")+
  theme(legend.position = "none")
}

plot24<-
plot_MC(2,4) + 
  scale_x_discrete(labels = c(
    "2" = "ほとんど受診しない",
    "4" = "すぐ受診する"  )) +
  geom_signif(comparisons = list(c("2","4")),
              na.rm = FALSE,
              map_signif_level = TRUE,
              annotations = sig(0.001),
              y_position = 70,
              col ="black")
plot34<-
plot_MC(3,4) +
  scale_x_discrete(labels = c(
    "3" = "時々受診する",
    "4" = "すぐ受診する"  )) +
  geom_signif(comparisons = list(c("3","4")),
              na.rm = FALSE,
              map_signif_level = TRUE,
              annotations = sig(0.01),
              y_position = 70,
              col ="black")
  


plot24+plot34

scale_x_discrete(labels=c("配偶者・パートナー027" = "配偶者・パートナー",
                          "子供029" = "子供",
                          "同居はしていない（単身）" = "単身",
                          "親（義両親を含む）" = "両親",
                          "兄弟姉妹028" = "兄弟姉妹"))+
  



## MCT between medicalcheck and HL-score 
mct02 <- dat_b %>% 
  select(c(`16_medicalCheck`, "total_h1", "total_h2", "total_h3", "total_h4"))
mctplot02 <- TukeyHSD(
  x = aov(mct02$total_h4 ~ mct02$`16_medicalCheck`),
  conf.level = 0.95) 

mctplot02 %>% tidy() %>%
  select(c(2,4,5:7)) %>% gt() %>%
  fmt_number(
    columns = c(2:5),
    decimals = 4,
    use_seps = FALSE) 

plot(mctplot02)

## HL-score by health care activity
dat_h <- dat_b %>% select(115:125) 
plot_exe <- dat_h %>% group_nest(`22_exercise`) %>%
    mutate(
      "activity" = c(
        "開始するつもりはない",
        "6ヶ月以内に開始予定",
        "1ヶ月以内に開始予定",
        "開始済み半年未満",
        "開始して半年以上"
      ),
      n = map_dbl(data, nrow),
      "HL-mean" = map_dbl(data, ~mean(.$total_h4)),
      "HL-median" = map_dbl(data, ~median(.$total_h4))
  ) %>% select(-1,-2) %>% gt() %>%
  tab_options(table.width = 500, 
              column_labels.background.color = "#FFC286")%>%
  fmt_number(
    columns = c(3:4),
    decimals = 1,
    use_seps = FALSE) %>%
  tab_header(
    title = md("運動")
  )

plot_diet <- dat_h %>% group_nest(`23_diet`) %>%
  mutate(
    "activity" = c(
      "開始するつもりはない",
      "6ヶ月以内に開始予定",
      "1ヶ月以内に開始予定",
      "開始済み半年未満",
      "開始して半年以上"
    ),
    n = map_dbl(data, nrow),
    "HL-mean" = map_dbl(data, ~mean(.$total_h4)),
    "HL-median" = map_dbl(data, ~median(.$total_h4))
  ) %>% select(-1,-2) %>% gt() %>%
  tab_options(table.width = 500, 
              column_labels.background.color = "#FFC286")%>%
  fmt_number(
    columns = c(3:4),
    decimals = 1,
    use_seps = FALSE) %>%
  tab_header(
    title = md("食事")
  )

plot_prev <- dat_h %>% group_nest(`24_prevention`) %>%
  mutate(
    "activity" = c(
      "開始するつもりはない",
      "6ヶ月以内に開始予定",
      "1ヶ月以内に開始予定",
      "開始済み半年未満",
      "開始して半年以上"
    ),
    n = map_dbl(data, nrow),
    "HL-mean" = map_dbl(data, ~mean(.$total_h4)),
    "HL-median" = map_dbl(data, ~median(.$total_h4))
  ) %>% select(-1,-2) %>% gt() %>%
  tab_options(table.width = 500, 
              column_labels.background.color = "#FFC286")%>%
  fmt_number(
    columns = c(3:4),
    decimals = 1,
    use_seps = FALSE) %>%
  tab_header(
    title = md("予防")
  )

plot_vit <- dat_h %>% group_nest(`25_vitalSign`) %>%
  mutate(
    "activity" = c(
      "開始するつもりはない",
      "6ヶ月以内に開始予定",
      "1ヶ月以内に開始予定",
      "開始済み半年未満",
      "開始して半年以上"
    ),
    n = map_dbl(data, nrow),
    "HL-mean" = map_dbl(data, ~mean(.$total_h4)),
    "HL-median" = map_dbl(data, ~median(.$total_h4))
  ) %>% select(-1,-2) %>% gt() %>%
  tab_options(table.width = 500, 
              column_labels.background.color = "#FFC286")%>%
  fmt_number(
    columns = c(3:4),
    decimals = 1,
    use_seps = FALSE) %>%
  tab_header(
    title = md("血圧・体重など")
  )

plot_exe
plot_diet
plot_vit
plot_prev


# 情報収集の第一選択
dat_i1 <- dat_b %>% group_nest(`18_information1`) 
dat_i2 <- dat_b %>% group_nest(`18_information2`)

plot35a <- func_info(dat_i1, "第１選択")
plot35b <- func_info(dat_i2, "第２選択")

plot35a + plot35b

func_info <- function(X,Y){
X %>% 
  mutate(n = map_dbl(data, nrow),
         Method = c("新聞",
                    "テレビ",
                    "雑誌",
                    "書籍",
                    "インターネット検索",
                    "SNS",
                    "医師",
                    "医師以外の医療スタッフ",
                    "健保や会社",
                    "母親",
                    "母親以外の家族",
                    "友人・知人",
                    "その他")) %>%
  arrange(desc(n)) %>% slice(1:12) %>% select(-1,-2) %>%
  ggplot() +
  aes(x = reorder(Method, n), y = n, fill = Method) +
  geom_bar(stat = "identity",
           width = .8,
           fill = "#FFAB76", alpha=.8) +
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  labs(x="収集方法", y = "人") +
  theme(legend.position = "none") +
  ggtitle(Y)+
  coord_flip()
}


# Training contents allocation
## add col by age group
dat1 <- survey_b %>% select(1:10, 119,120, 124)
dat1 <- dat1 %>% mutate(age_group = case_when(
  .$年齢 <= 30 ~ "1",
  .$年齢 >30&.$年齢<=40 ~ "2",
  .$年齢 > 40 & .$年齢 <=50 ~"3",
  .$年齢 >50 ~"4",
))

group_postA <- dat1 %>% group_by(性別) %>% sample_frac(0.5) %>% ungroup()

set.seed = 19
dat1_split <- initial_split(dat1, prop = 0.5, strata = age_group)
dat1_A <- training(dat1_split)
dat1_B <- testing(dat1_split)

table(dat1_A$性別)
table(dat1_B$性別)
table(dat1_A$age_group)
table(dat1_B$age_group)

PA <- dat1_A %>% ggplot() +
  aes(x=total_h4) +
  geom_histogram(binwidth = 2)

PB <- dat1_B %>% ggplot() +
  aes(x=total_h4) +
  geom_histogram(binwidth = 2)

library(patchwork)
PA + PB

dat1_A %>% write_csv("/Users/kawaiyuusei/Documents/2021_Minacare/group_A.csv")
dat1_B %>% write_csv("/Users/kawaiyuusei/Documents/2021_Minacare/group_B.csv")


## women under 30 year old
plots1 <- dat_h %>% 
  filter(
    sex == "女性",
    age < 30
  ) %>% 
  group_nest(`23_diet`) %>%
  mutate(
    "activity" = c(
      "開始するつもりはない",
      "6ヶ月以内に開始予定",
      "1ヶ月以内に開始予定",
      "開始済み半年未満",
      "開始して半年以上"
    ),
    n = map_dbl(data, nrow),
    "HL-mean" = map_dbl(data, ~mean(.$total_h4)),
    "HL-median" = map_dbl(data, ~median(.$total_h4))
  ) %>% select(-1,-2) %>% 
  ggplot() +
  aes(x = activity, y=n ) +
  geom_bar(stat = "identity", width = .7, alpha=.8, fill ="#FFB72B") +
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  scale_x_discrete(limit=c(
    "開始するつもりはない",
    "6ヶ月以内に開始予定",
    "1ヶ月以内に開始予定",
    "開始済み半年未満",
    "開始して半年以上"
  ))+
  coord_flip() +
  labs(x=NULL, y="人数")




plots2 <- dat_b %>% 
  filter(
    sex == "女性",
    age < 30
  ) %>% 
  group_nest(`09_women`) %>%
  mutate(
    "activity" = c(
      "話したことはない",
      "話したことがある",
      "ときどき話す",
      "よく話す"
    ),
    n = map_dbl(data, nrow),
  ) %>% select(-1,-2) %>% 
  ggplot() +
  aes(x = activity, y=n ) +
  geom_bar(stat = "identity", width = .7, alpha=.8, fill ="#FFB72B") +
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  scale_x_discrete(limit=c(
    c(
      "話したことはない",
      "話したことがある",
      "ときどき話す",
      "よく話す"
    )
  ))+
  coord_flip() +
  labs(x=NULL, y="人数")

plots1
