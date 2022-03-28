# 保険者ごとの結果
## groupby collector
func1 <- function(X, Y){
  dat_ze %>% 
    drop_na(after_total_h4) %>%
    filter(Collector == X) %>%
    group_nest({{Y}}) %>%
    mutate(N = map_int(data, nrow)) %>%
    rename(., c("state" = {{Y}}))
}

func1("C&R", before_diet) 
func1("C&R", after_diet) 

func2 <- function(X, Y, y){
  table1 <- func1(X, {{Y}})
  table2 <- func1(X, {{y}})
  right_join(table1, table2, by = "state") %>%
    arrange(state) %>%
    mutate(`健康維持活動` = 
             c("開始するつもりはない",
               "6ヶ月以内に開始予定",
               "1ヶ月以内に開始予定",
               "開始済み半年未満",
               "開始して半年以上"
             )) %>%
    relocate(`健康維持活動`, .before = 1) %>%
    select(-c(2,3,5)) %>%
    rename(., c("介入前" = "N.x", "介入後" = "N.y"))
}

func3 <- function(X,Y,Z){
  X %>%
    gt() %>%
    tab_spanner(
      label = md(Y),
      columns = c(2,3)) %>%
    tab_options(table.width = 400)  %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(2)
      ),
      locations = 
        cells_body(
          columns = everything(),
          rows = 5
        )) %>%
    tab_header(title = Z)%>%
    tab_options(table_body.hlines.color = "white",
                table_body.border.bottom.color = "white",
                column_labels.border.bottom.color = "black",
                column_labels.border.top.color = "white",
                table.border.top.color = "white",
                heading.border.bottom.color = "white") 
}

func4 <- function(a,b,c,d,e,f,X,Y){
  tablea <- func2(a,{{X}},{{Y}}) 
  tableb <- func2(b,{{X}},{{Y}})
  tablec <- func2(c,{{X}},{{Y}})
  tabled <- func2(d,{{X}},{{Y}})
  tablee <- func2(e,{{X}},{{Y}})
  tablef <- func2(f,{{X}},{{Y}})
  
  list(tablea, tableb, tablec, tabled, tablee, tablef) %>%
    reduce(inner_join, by ="健康維持活動")
}

func4("C&R", 
      "九州電力健康保険組合", 
      "内田洋行健康保険組合", 
      "日本事務器健康保険組合", 
      "日本航空健康保険組合", 
      "第一生命健康保険組合", 
      before_diet, 
      after_diet) 
#%>% 
#  rename_with(~str_replace(.,pattern = ".x|.y",
replacement = "1"))


func5 <- function(X,Y1,Y2,Y3,Y4,Y5,Y6,Z){
  X %>%
    gt() %>%
    tab_spanner(
      label = md(Y1),
      columns = c(2,3)) %>%
    tab_spanner(
      label = md(Y2),
      columns = c(4,5)) %>%
    tab_spanner(
      label = md(Y3),
      columns = c(6,7)) %>%
    tab_spanner(
      label = md(Y4),
      columns = c(8,9)) %>%
    tab_spanner(
      label = md(Y5),
      columns = c(10,11)) %>%
    tab_spanner(
      label = md(Y6),
      columns = c(12,13)) %>%
    tab_options(table.width = 1000)  %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(2)
      ),
      locations = 
        cells_body(
          columns = everything(),
          rows = 5
        )) %>%
    tab_header(title = Z)%>%
    tab_options(table_body.hlines.color = "white",
                table_body.border.bottom.color = "white",
                column_labels.border.bottom.color = "black",
                column_labels.border.top.color = "white",
                table.border.top.color = "white",
                heading.border.bottom.color = "white") 
}

func5(table_diet, 
      "C&R", 
      "九州電力健康保険組合", 
      "内田洋行健康保険組合", 
      "日本事務器健康保険組合", 
      "日本航空健康保険組合", 
      "第一生命健康保険組合",
      "食事")


tablecollector <- function(X,Y,Z){
  func2(X, {{Y}}, {{Z}}) %>%
    mutate("介入前" = round(介入前/sum(介入前, na.rm = TRUE)*100, digits = 0),
           "介入後" = round(介入後/sum(介入後, na.rm = TRUE)*100, digits = 0)) %>%
    pivot_longer(cols = c(2:3), names_to = "kainyu", values_to = "ratio") %>%
    mutate(Collector = X)}

tablecollector("C&R", before_diet, after_diet)


bindcollector <- function(X,Y){
  table1 <- tablecollector("C&R", {{X}}, {{Y}})
  table2 <- tablecollector("九州電力健康保険組合",{{X}}, {{Y}})
  table3 <- tablecollector("内田洋行健康保険組合", {{X}}, {{Y}})
  table4 <- tablecollector("日本事務器健康保険組合", {{X}}, {{Y}})
  table5 <- tablecollector("日本航空健康保険組合", {{X}}, {{Y}})
  table6 <- tablecollector("第一生命健康保険組合", {{X}}, {{Y}})
  
  list(table1,table2,table3,table4,table5,table6) %>%
    reduce(bind_rows)
}

summarytable <- function(X,Y){
  bindcollector({{X}}, {{Y}}) %>%
    ggplot(aes(x=kainyu, y=ratio, 
               fill=factor(健康維持活動, levels = c("開始するつもりはない",
                                              "6ヶ月以内に開始予定",
                                              "1ヶ月以内に開始予定",
                                              "開始済み半年未満",
                                              "開始して半年以上" ))))　+
    geom_bar(stat = "identity", width = .5, alpha=.7) +
    #scale_x_discrete(limits = c("介入後", "介入前"))+
    labs(x=NULL, y="回答者（％）", fill = "健康維持活動:") +
    theme_ipsum(base_family = "HiraKakuPro-W3") +
    theme(legend.position = "bottom",
          legend.background = element_rect(fill="white", 
                                           size=.5, 
                                           linetype = "solid",
                                           color = "gray"),
          legend.text = element_text(size = 14),
          legend.title = element_text(size=14))+
    scale_fill_brewer(palette = "Oranges")+
    geom_text(aes(label = ratio), colour = "black", 
              position = "stack", vjust = 1.5)+
    facet_wrap(~Collector, ncol = 6, labeller = newlabel)}


newlabel <- as_labeller(c( 
  "C&R" = "C&Rグループ健康保険組合", 
  "九州電力健康保険組合" = "九州電力健康保険組合", 
  "内田洋行健康保険組合" = "内田洋行健康保険組合", 
  "日本事務器健康保険組合" = "日本事務器健康保険組合", 
  "日本航空健康保険組合" = "日本航空健康保険組合", 
  "第一生命健康保険組合" = "第一生命健康保険組合"))

names(newlabel)

summarytable(before_diet, after_diet)+
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/05_plot/plot40.png",
         bg = "transparent", width = 16, height = 5)
summarytable(before_exercise, after_exercise) +
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/05_plot/plot41.png",
         bg = "transparent", width = 16, height = 5)


# extract who change their willingness
dat_ze %>% glimpse()

# 上向きの変化があった人 = 意識調査の1以上の変化が認められた人
dat_zd %>% glimpse()


dat_zg <- dat_za %>% 
  mutate(A_total_agg = (sum_agg("Agg_AD_A") + Agg_ALL01),
         B_total_agg = (sum_agg("Agg_AD_B") + Agg_ALL01 + Agg_AD_AB8)) %>%
  rename(!!! list3) %>% 
  drop_na(after_total_h4) %>%
  select(-starts_with(c("AM_","Open", "Agg_")))
  
dat_zg <- dat_zg %>%
  mutate(exercise_p = after_exercise - before_exercise,
         diet_p = after_diet - before_diet,
         vital_p = after_vitalSign - before_vitalSign) %>%
  mutate(age_group = case_when(
    .$年齢 <= 30 ~ "20代",
    .$年齢 >30&.$年齢<=40 ~ "30代",
    .$年齢 > 40 & .$年齢 <=50 ~"40代",
    .$年齢 >50 ~"50代以上",
  ))　

ishikitable <- function(X,Y,Z){
table_1 <- dat_zg %>% group_nest(age_group) %>% 
  mutate(n = map_int(data,nrow)) %>% select(-2)
table_2 <- dat_zg %>% 
  filter({{X}} >=1, #exercisepoint
         {{Y}} >=4) %>% group_nest(age_group) %>% #afterexercise
  mutate(positive = map_int(data,nrow)) %>% select(-2)
table_3 <- dat_zg %>%
  filter({{X}} <=0,
    {{Z}} <=3) %>% #beforeexercise
  group_nest(age_group) %>%
  mutate(asis = map_int(data,nrow)) %>% select(-2) 
table_4 <- dat_zg %>%
  filter({{Z}} >=4,
         {{X}} >=0) %>%
  group_nest(age_group) %>%
  mutate(doing = map_int(data,nrow)) %>% select(-2) 
  
list(table_1, table_2, table_3, table_4) %>%
  reduce(inner_join, by="age_group")%>%
  mutate(positive = round(positive/n,digits = 2),
         asis = round(asis/n,digits = 2),
         doing = round(doing/n,digits = 2)) 
}

ishikitable(diet_p, after_diet, before_diet) %>% gt() %>%
  tab_options(table.width = 900)%>%
  fmt_percent(columns = c(3,4,5),decimals = 0) %>%
  cols_label(
    age_group = md("年代"),
    n = md("人数"),
    positive = md("上向きの意識変容が認められ<br>かつ食事改善（行動）に至った"),
    asis = md("上向きの意識変容が認められず<br>かつ食事改善（行動）していない"),
    doing = md("コンテンツ配信前から<br>継続して食事に気をつけている")
  ) %>%
  tab_header(title = md("年代別に見る介入後の意識変化_食事"))%>%
  tab_options(table_body.hlines.color = "white",
              table_body.border.bottom.color = "white",
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "white",
              table.border.top.color = "white",
              heading.border.bottom.color = "black")%>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(2)
    ),
    locations = 
      cells_body(
        columns = everything(),
        rows = 4
      ))

  
dat_zg %>% filter(before_exercise == 1|2|3,
                    exercise_p<=0) %>% glimpse()
  
  
fig1 <- dat_zg %>% 
  filter(exercise_p >=1) %>% 
  ggplot(aes(x=`年齢`)) +
  geom_histogram(stat = "count")+
  geom_density(color = "Orange", size = 1.5) +
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  labs(x="年齢", y="密度")

fig2 <- dat_zg %>%
  filter(before_exercise == 1|2|3,
         exercise_p<=0) %>%
  ggplot(aes(x=`年齢`))+
  geom_histogram(stat = "count")+
  geom_density(color = "royalblue", size = 1.5) +
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  labs(x="年齢", y="密度")

fig1 / fig2

fig3 <- dat_zg %>% 
  filter(diet_p >=1) %>% 
  ggplot(aes(x=`年齢`)) +
  geom_density(color = "Orange", size = 1.5) +
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  labs(x="年齢", y="密度")

fig4 <- dat_zg %>%
  filter(before_diet == 1|2|3,
         diet_p<=0) %>%
  ggplot(aes(x=`年齢`))+
  geom_density(color = "royalblue", size = 1.5) +
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  labs(x="年齢", y="密度")

fig3/fig4

sextable <- function(X,Y,Z){
  table_1 <- dat_zg %>% group_nest(性別) %>% 
    mutate(n = map_int(data,nrow)) %>% select(-2)
  table_2 <- dat_zg %>% 
    filter({{X}} >=1, #exercisepoint
           {{Y}} >=4) %>% group_nest(性別) %>% #afterexercise
    mutate(positive = map_int(data,nrow)) %>% select(-2)
  table_3 <- dat_zg %>%
    filter({{X}} <=0,
           {{Z}} <=3) %>% #beforeexercise
    group_nest(性別) %>%
    mutate(asis = map_int(data,nrow)) %>% select(-2) 
  table_4 <- dat_zg %>%
    filter({{Z}} >=4,
           {{X}} >=0) %>%
    group_nest(性別) %>%
    mutate(doing = map_int(data,nrow)) %>% select(-2) 
  
  list(table_1, table_2, table_3, table_4) %>%
    reduce(inner_join, by="性別")%>%
    mutate(positive = round(positive/n,digits = 2),
           asis = round(asis/n,digits = 2),
           doing = round(doing/n,digits = 2)) 
}

sextable(exercise_p, after_exercise, before_exercise) %>% gt() %>%
  tab_options(table.width = 900)%>%
  fmt_percent(columns = c(3,4,5),decimals = 0) %>%
  cols_label(
    n = md("人数"),
    positive = md("上向きの意識変容が認められ<br>かつ運動（行動）に至った"),
    asis = md("上向きの意識変容が認められず<br>かつ運動（行動）していない"),
    doing = md("コンテンツ配信前から<br>継続して運動している")
  ) %>%
  tab_header(title = md("性別で見る介入後の意識変化_運動"))%>%
  tab_options(table_body.hlines.color = "white",
              table_body.border.bottom.color = "white",
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "white",
              table.border.top.color = "white",
              heading.border.bottom.color = "black")%>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(2)
    ),
    locations = 
      cells_body(
        columns = everything(),
        rows = 2
      ))
