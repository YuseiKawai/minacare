# import library
pacman::p_load(tidyverse, pwr, hrbrthemes, rlang, gt, patchwork, tidymodels)

# read data
dat_rezept <- 
  read_csv("/Users/kawaiyuusei/Documents/03_2021_Minacare/12_rawdat/20211203_経産省PJ_レセプトデータ_758名.csv")

glimpse(dat_rezept)

# join nefsted data with survey data & extract
dat_rez <- dat_rezept %>% 
  group_nest(personal_id) %>% 
  left_join(dat_b, ., by = "personal_id") %>% 
  select(-c(11:119)) %>%
  select(-c("Started_on", "Last updated on", "Status", 
            "Language", "IP address", 
            ))

## Scatter plot HL_score vs total-rezept-point
dat_rez %>%
  mutate("total_rez_point" = map_dbl(data, ~sum(.$`傷病紐付きレセプト合計点数`, na.rm = TRUE))) %>% 
  ggplot() +
  aes(x=log(total_rez_point), y = total_h4, col = sex)+
  geom_point(alpha = 0.5, size = 3) +
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/02_plot/plot52.png",
         bg = "transparent")

## 入院経験者と非経験者のHLスコア比較
dat_rezept %>% 
  filter(.$入院フラグ == "入院") %>% 
  group_nest(personal_id) %>% mutate( hospitalization = "yes") %>%
  select(-c("data")) %>%
  left_join(dat_b, ., by = "personal_id") %>% 
  select(-c(3:119)) %>%
  replace_na(list(hospitalization = "no")) %>%
  group_nest(hospitalization) %>%
  mutate(median_hl_score = map_dbl(data, ~median(.$total_h4)),
         mean_hl_score = map_dbl(data, ~mean(.$total_h4)),
         sd_hl_score = map_dbl(data, ~sd(.$total_h4))) %>% select(-data) %>%
  gt() %>%
  tab_options(table.width = 600, 
              column_labels.background.color = "#FFC286") %>%
  fmt_number(
    columns = c(3:4),
    decimals = 1,
    use_seps = FALSE)


## 参加者の主要疾患
table(dat_rezept$ＩＣＤ１０大分類名) %>% tibble() %>%
  mutate(大分類 = c("健康状態に影響を及ぼす要因及び保健サービスの利用", 
                   "先天奇形，変形及び染色体異常", "内分泌，栄養及び代謝疾患", 
                   "周産期に発生した病態", "呼吸器系の疾患", "妊娠，分娩及び産じょく＜褥＞", 
                   "循環器系の疾患", "感染症及び寄生虫症", "損傷，中毒及びその他の外因の影響", 
                   "新生物＜腫瘍＞", "消化器系の疾患", "特殊目的用コード", 
                   "症状，徴候及び異常臨床所見・異常検査所見で他に分類されないもの", 
                   "皮膚及び皮下組織の疾患", "眼及び付属器の疾患", 
                   "神経系の疾患", "筋骨格系及び結合組織の疾患", 
                   "精神及び行動の障害", "耳及び乳様突起の疾患", 
                   "腎尿路生殖器系の疾患", "血液及び造血器の疾患並びに免疫機構の障害"
  )) %>% rename("n" = ".") %>% arrange(desc(n)) %>% gt() %>%
  tab_options(table.width = 450, 
              column_labels.background.color = "#FFC286")


## 参加者が罹患した疾患名（多い順）
dname <- table(dat_rezept$傷病名) %>% names() %>% dput()
table_01 <- table(dat_rezept$傷病名) %>% tibble() %>%
  mutate("疾患名" = dname) %>% 
  rename("n" = ".") %>%
  arrange(desc(n)) %>% 
  slice(1:15) %>% gt()
table_02 <- table(dat_rezept$傷病名) %>% tibble() %>%
  mutate("疾患名" = dname) %>% 
  rename("n" = ".") %>%
  arrange(desc(n)) %>% 
  slice(16:30) %>% gt()

## HL-score w/ FamilyDoctor vs w/o FamilyDoctor
###登場する病院名が多い病院へ通っている人をかかりつけ医がいると判断する。
hosp_name <- table(dat_rezept$医療機関名称) %>% 
  data.frame() %>%
  arrange(desc(Freq)) %>% 
  filter(Freq >= 100) 
hosp_name$Var1 <- as.character(hosp_name$Var1)
hosp_list <- table(hosp_name$Var1) %>% names() %>% dput()

### 100回以上出現する施設に紐づく患者のHL-scoreを導く
dat_rezept %>% filter(.$医療機関名称 %in% hosp_list) %>%
  group_nest(personal_id)　%>% mutate(familyDoctor = "yes") %>%
  select(-c("data")) %>%
  left_join(dat_b, ., by = "personal_id") %>% 
  select(-c(3:119)) %>%
  replace_na(list(familyDoctor = "no")) %>%
  group_nest(familyDoctor) %>% 
  mutate(n = map_dbl(data, nrow),
         median_hl_score = map_dbl(data, ~median(.$total_h4)),
         mean_hl_score = map_dbl(data, ~mean(.$total_h4)),
         sd_hl_score = map_dbl(data, ~sd(.$total_h4))
         ) %>% select(-data) %>% gt() %>% fmt_number(
           columns = c(4:5),
           decimals = 1,
           use_seps = FALSE)

dat_rezept %>% filter(.$医療機関名称 %in% hosp_list) %>%
  group_nest(personal_id)　%>% mutate(familyDoctor = "yes") %>%
  select(-c("data")) %>%
  left_join(dat_b, ., by = "personal_id") %>%
  select(-c(3:118)) %>%
  replace_na(list(familyDoctor = "no")) %>% 
  filter(familyDoctor == 'yes') %>%
  ggplot(aes(x=age, y=total_h4)) +
  geom_point(size = 2, color = "royalblue") +
  theme_ipsum(base_family = "HiraKakuPro-W3")

## 生活習慣病関連疾患の抽出
table(dat_rezept$傷病名) %>% tibble() %>%
  mutate("疾患名" = dname) %>% 
  rename("n" = ".") %>%
  arrange(desc(n)) %>% filter(n>100) %>% names()

dat_rezept %>% group_nest(`傷病名`) %>%
  mutate(n = map_int(data, nrow)) %>%
  arrange(desc(n)) %>% filter(`傷病名` %in% lh_list) %>% 
  mutate(ms_group = "y") %>% select(-2,-3)
    
dat_n <- dat_rezept %>% 
  group_nest(`personal_id`, `傷病名`) %>%
  filter(`傷病名` %in% lh_list2) %>% select(1,2) %>%
  group_nest(`personal_id`) %>%
  mutate(ms_group = "y") %>% select(1,3) %>%
  left_join(dat_b, ., by = "personal_id") %>% select(-c(3:118)) %>%
  replace_na(list(ms_group = "n")) 

dat_n %>% group_nest(ms_group) %>%
  mutate(n = map_dbl(data, nrow),
         median_hl_score = map_dbl(data, ~median(.$total_h4)),
         mean_hl_score = map_dbl(data, ~mean(.$total_h4)),
         sd_hl_score = map_dbl(data, ~sd(.$total_h4))
  ) %>% select(-data) %>% gt() %>% fmt_number(
    columns = c(4:5),
    decimals = 1,
    use_seps = FALSE)

dat_n %>% 
  ggplot(aes(x=total_h4, fill = ms_group)) +
  geom_histogram(bin = 25) +
  theme_ipsum(base_family = "HiraKakuPro-W3")


dat_n %>% slice(1:60) %>% select(1) %>% table() %>% names() %>% dput()
lh_list <- c("２型糖尿病", 
             "う蝕", 
             "便秘症",  
             "慢性歯周炎", 
             "歯周炎", 
             "歯髄炎", 
             "糖尿病",  
             "肝機能障害",  
             "脂肪肝", 
             "脂質異常症", 
             "腰痛症", 
             "骨粗鬆症", 
             "高コレステロール血症", 
             "高尿酸血症", 
             "高脂血症", 
             "高血圧症")
  
lh_list2 <- c("脂肪肝",
             "高コレステロール血症", 
             "高尿酸血症", 
             "高脂血症", 
             "高血圧症")

## sinaplot ms_group y vs n
dat_n %>% 
  ggplot(aes(x=ms_group, y=total_h4, fill=ms_group))+
  geom_violin()+
  ggforce::geom_sina() +
  theme_ipsum()
  
  geom_jitter(shape=16, position = position_jitter(0.3))

  
## boxplot ms_group y vs n
dat_n %>% 
  ggplot(aes(x=ms_group, y=total_h4, fill=ms_group))+  
  geom_boxplot(alpha = 0.5,
               outlier.shape = NA) +
    labs(x=NULL, y="ヘルスリテラシースコア") +
    theme_ipsum(base_family = "HiraKakuPro-W3") +
    scale_fill_brewer(palette = "Oranges")+
    theme(legend.position = "none") +
  scale_x_discrete(labels = c(
    "n" = "非生活習慣病群",
    "y" = "生活習慣病群"  )) +
  geom_signif(comparisons = list(c("n","y")),
              na.rm = FALSE,
              map_signif_level = TRUE,
              annotations = sig(0.023),
              y_position = 70,
              col ="black")
  
  
## 2-sample t-test
t_test(x=dat_n,
       formula = total_h4 ~ ms_group,
       order = c("n", "y"),
       alternative = "two-sided") %>% gt()


dat_n %>%
  specify(total_h4 ~ ms_group) %>%
  calculate(stat = )
