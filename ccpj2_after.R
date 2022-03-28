# import library
pacman::p_load(tidyverse, pwr, hrbrthemes, rlang, gt, patchwork, tidymodels)

# read data set
dat_za <- 
  read_csv("/Users/kawaiyuusei/Documents/03_2021_Minacare/13_rawdat_after/DataSet.csv")
dat_za %>% glimpse()

dat_za %>% 
  drop_na(after_total_h4) %>%
  group_nest(Collector) %>%
  mutate(n = map_int(data, nrow))

# total HL-scoreの比較
## select field and change data type
dat_zb <- dat_za %>% select(-(9:240)) 
list_f = dat_zb %>% select(9:29) %>% names() %>% dput()
dat_zb <- dat_zb %>% mutate(across(all_of(list_f), as.factor))

## t-test before vs after
dat_zc <- 
  dat_zb %>% filter(Agg_ALLOpen == 1) %>% 
  na.omit() %>%
  pivot_longer(cols = c(before_total_h4, after_total_h4),
               values_to = "hl_score", names_to = "ab") 

dat_zc %>% t_test(
  formula =  hl_score ~ ab,
  order = c("before_total_h4", "after_total_h4"),
  alternative = "two-sided",
) %>% gt() %>%
  fmt_number(
    columns = c(statistic, t_df, p_value, estimate, lower_ci, upper_ci),
    decimals = 3,
    use_seps = FALSE
  ) 

## histogram before vs after

fighisto <-function(X,Y){
dat_zf %>% ggplot() +
  aes(x = {{X}}) +
  geom_density(size=1.5, color= Y) +
  xlim(15,65)+
  labs(x = "ヘルスリテラシースコア") +
  theme_ipsum(base_family = "HiraKakuPro-W3") 
}

figA <- fighisto(before_total_h4, "orange")
figB <- fighisto(after_total_h4, "royalblue")


figA / figB


## boxplot before vs after
dat_zc %>% ggplot(aes(x=ab, y=hl_score, fill = ab)) +
  scale_x_discrete(limit = c("before_total_h4", "after_total_h4"),
                   labels = c("before_total_h4" = "コンテンツ配信前",
                              "after_total_h4" = "コンテンツ配信後"))+
  geom_boxplot(alpha = 0.5, 
               outlier.shape = NA) +
  labs(x=NULL, y="ヘルスリテラシースコア") +
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  scale_fill_brewer(palette = "Oranges")+
  theme(legend.position = "none") +
  ggsave(filename = "/Users/kawaiyuusei/Documents/03_2021_Minacare/05_plot/plot18.png",
         bg = "transparent")

## t-test by "training times"
### set function rowsum
sum_agg <- function(n){
  rowSums(across(starts_with(n)), na.rm = TRUE) 
  }
### add cols of total taken times
dat_zd <- dat_za %>% 
  select(-(10:240)) %>%
  mutate(A_total_agg = (sum_agg("Agg_AD_A") + Agg_ALL01),
         B_total_agg = (sum_agg("Agg_AD_B") + Agg_ALL01 + Agg_AD_AB8)) %>%
  select(1:9, "A_total_agg", "B_total_agg") %>% na.omit()

### barplot per training times
#### Contents-A
dat_zd %>% group_nest(Group, A_total_agg) %>%
  mutate(n = map_int(data, nrow)) %>%
  filter(Group == "A") %>%
  ggplot(aes(x=factor(A_total_agg), y=n))+
  geom_bar(stat = "identity", fill = "royalblue") +
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  labs(title = "Contents-A",
       x = "受講回数", 
       y = "人数")

#### Contents-B
dat_zd %>% group_nest(Group, B_total_agg) %>%
  mutate(n = map_int(data, nrow)) %>%
  filter(Group == "B") %>%
  ggplot(aes(x=factor(B_total_agg), y=n))+
  geom_bar(stat = "identity", fill = "royalblue") +
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  labs(title = "Contents-B",
       x = "受講回数", 
       y = "人数")

### group_A t-test function
ttfunc_A <- function(X){
dat_zd %>%
  filter(Group == "A",
         A_total_agg >= X) %>%
  pivot_longer(cols = c(before_total_h4, after_total_h4),
               values_to = "hl_score", names_to = "ba") %>%
  t_test(formula =  hl_score ~ ba,
         order = c("before_total_h4", "after_total_h4"),
         alternative = "two-sided")
}

resultdfA <- data.frame()
set.seed(100)
for(i in 1:8){
resultdfA = rbind(resultdfA, ttfunc_A(i))
}

resultdfA %>% slice(7,8) %>% mutate(times = c(7,8)) %>% 
  gt() %>%
  fmt_number(
    columns = c(statistic, t_df, p_value, estimate, lower_ci, upper_ci),
    decimals = 3,
    use_seps = FALSE
  ) %>%
  tab_header(
    title = md("t-test_A群のHL-score")
  )

### group_B t-test function
ttfunc_B <- function(X){
  dat_zd %>%
    filter(Group == "B",
           B_total_agg >= X) %>%
    pivot_longer(cols = c(before_total_h4, after_total_h4),
                 values_to = "hl_score", names_to = "ba") %>%
    t_test(formula =  hl_score ~ ba,
           order = c("before_total_h4", "after_total_h4"),
           alternative = "two-sided")
}

resultdfB <- data.frame()
set.seed(100)
for(i in 1:14){
  resultdfB = rbind(resultdfB, ttfunc_B(i))
}

resultdfB %>% slice(13,14) %>% mutate(times = c(13,14)) %>% 
  gt() %>%
  fmt_number(
    columns = c(statistic, t_df, p_value, estimate, lower_ci, upper_ci),
    decimals = 3,
    use_seps = FALSE
  ) %>%
  tab_header(
    title = md("t-test_B群のHL-score")
  )

# health maintenance action
### extract target, rename, NAremove
dat_ze <- 
  dat_za %>% 
  select(1:9, 115:118, 127:129, 185:188)

dat_ze %>% names() %>% dput()
list3 <- c(
  "before_exercise" = "before_question22", 
  "before_diet" = "before_question23", 
  "before_prevention" = "before_question24", 
  "before_vitalSign" = "before_question25", 
  "exerciseChange" = "after_question08", 
  "dietChange" = "after_question09", 
  "vitalChange" = "after_question10",
  "after_exercise" = "after_question31", 
  "after_diet" = "after_question32", 
  "after_prevention" = "after_question33", 
  "after_vitalSign" = "after_question34")

dat_ze <- dat_ze %>%
  rename(!!! list3)
dat_ze <- dat_ze %>%
  drop_na(after_total_h4) # remove NA rows from after_total_h4

list4 <- dat_ze %>% select(10:20) %>% names() %>% dput()
dat_ze <- dat_ze %>% 
  mutate(across(all_of(list4), as.factor))

### exercise (trial)
dat_ze %>% 
  group_nest(before_exercise, after_exercise) %>%
  drop_na(after_exercise) 

### before vs after table
tablefunc <- function(X, Y){
Xsym <- ensym(X)
Ysym <- ensym(Y)

table_before <-
  dat_ze %>% 
  select(starts_with("before_")) %>%
  group_nest({{Xsym}}) %>%
  mutate(before_n = map_int(data, nrow))

table_after <- dat_ze %>%
  select(starts_with("after_")) %>%
  group_nest({{Ysym}}) %>%
  mutate(after_n = map_int(data, nrow))

bind_cols(table_before, table_after) %>%
  select(3,6) %>%
  mutate(state = c(
    "開始するつもりはない",
    "6ヶ月以内に開始予定",
    "1ヶ月以内に開始予定",
    "開始済み半年未満",
    "開始して半年以上"
  )) %>%
  relocate(state, .before = before_n)
}

tablefunc(before_exercise, after_exercise) %>% gt() %>% 
  tab_header(title = md("健康ケア_運動"))
tablefunc(before_diet, after_diet)%>% gt() %>% 
  tab_header(title = md("健康ケア_食事"))
tablefunc(before_vitalSign, after_vitalSign)%>% gt() %>% 
  tab_header(title = md("健康ケア_血圧など"))
tablefunc(before_prevention, after_prevention)%>% gt() %>% 
  tab_header(title = md("健康ケア_予防"))

### count
dat_ze %>% 
  select(ends_with("_exercise")) %>%
  na.omit() %>%
  pivot_longer(1:2, names_to = "ba", values_to = "state") %>%
  group_nest(ba, state) %>%
  mutate(n = map_int(data, nrow)) %>%
  group_by(ba) %>%
  mutate(perc = n/sum(n)) %>%

#group_by(ba, state) %>%
#summarise(count=n()) %>%
#mutate(perc = count/sum(count)) 
  
  ggplot() +
  aes(x=factor(ba), y=n, fill=state) +
  geom_bar(stat = "identity" , width = 0.5) +
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  scale_fill_ipsum(labels = c(
    "開始するつもりはない",
    "6ヶ月以内に開始予定",
    "1ヶ月以内に開始予定",
    "開始済み半年未満",
    "開始して半年以上"
  )) + 
  theme(axis.title.x = element_blank()) +
  scale_x_discrete(limit = c("before_exercise", "after_exercise"))

### function for 
hm_func <- function(X, Y, Z) {
  dat_ze %>% 
    select(ends_with(X)) %>%
    na.omit() %>%
    pivot_longer(1:2, names_to = "ba", values_to = "state") %>%
    group_nest(ba, state) %>%
    mutate(n = map_int(data, nrow)) %>%
    group_by(ba) %>%
    mutate(perc = n/sum(n)) %>%
    ggplot() +
    aes(x=factor(ba), y=perc*100, fill = state) +
    geom_bar(stat = "identity", width = 0.5) +
    labs(y="percent")+
    theme_ipsum(base_family = "HiraKakuPro-W3") +
    scale_fill_ipsum(labels = c(
      "開始するつもりはない",
      "6ヶ月以内に開始予定",
      "1ヶ月以内に開始予定",
      "開始済み半年未満",
      "開始して半年以上"
    )) + 
    theme(axis.title.x = element_blank()) +
    scale_x_discrete(limit = c(Y, Z))
}

hm_func("_exercise", "before_exercise", "after_exercise") +
  labs(title = "健康ケア_運動")
hm_func("_diet", "before_diet", "after_diet")　+
  labs(title = "健康ケア_食事")
hm_func("_vitalSign", "before_vitalSign", "after_vitalSign")　+
  labs(title = "健康ケア_血圧など")
hm_func("_prevention", "before_prevention", "after_prevention")　+
  labs(title = "健康ケア_予防")

## Cross Analysis
c("before_exercise" = "before_question22", 
"before_diet" = "before_question23", 
"before_prevention" = "before_question24", 
"before_vitalSign" = "before_question25", 
"exerciseChange" = "after_question08", 
"dietChange" = "after_question09", 
"vitalChange" = "after_question10",
"after_exercise" = "after_question31", 
"after_diet" = "after_question32", 
"after_prevention" = "after_question33", 
"after_vitalSign" = "after_question34")

func_table_s <- function(X,Y){
table(X) %>% tibble() %>% rename("n" = ".") %>%
  mutate(state = c(
    "以前と変わらず",
    "以前よりしている",
    "以前より心がけている"
  )) %>%
  relocate(state, .before = n) %>% 
    gt() %>%
    tab_options(table.width = 400)  %>%
    cols_label(
      state = md("主観的意識"),
      n = md("人数")
    ) %>%
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
          rows = 3
        )) %>%
    tab_header(title = Y)%>%
    tab_options(table_body.hlines.color = "white",
                table_body.border.bottom.color = "white",
                column_labels.border.bottom.color = "black",
                column_labels.border.top.color = "white",
                table.border.top.color = "white",
                heading.border.bottom.color = "white")
  }


func_table_s(dat_ze$dietChange, "主観的意識変化_食事")
func_table_s(dat_ze$exerciseChange, "主観的意識変化_運動")
func_table_s(dat_ze$vitalChange)

dat_test <-  dat_za %>% drop_na(after_total_h4)
table(dat_test$after_question05)

table_e <-
  table(dat_ze$after_exercise, dat_ze$exerciseChange) %>% tibble()
table_d <-
  table(dat_ze$after_diet, dat_ze$dietChange) %>% print()
table_v <- 
  table(dat_ze$after_vitalSign, dat_ze$vitalChange) %>% print()


dat_zf <- dat_za %>% rename(!!! list3) %>%  
  mutate(A_total_agg = (sum_agg("Agg_AD_A") + Agg_ALL01),
         B_total_agg = (sum_agg("Agg_AD_B") + Agg_ALL01 + Agg_AD_AB8)) %>%
  drop_na(after_total_h4) 

timeschange <- function(X, Y, x, y, z){
dat_xx <- 
  dat_zf %>% filter(Group == X, {{z}} == Y ) %>%
  group_nest({{x}}) %>%
  mutate("after_N" = map_int(data, nrow)) %>%
  select(-2)

dat_yy <- 
  dat_zf %>% filter(Group == X, {{z}} == Y) %>%
  group_nest({{y}}) %>%
  mutate("before_N" = map_int(data, nrow)) %>% 
  select(-2)

bind_cols(dat_yy, dat_xx$after_N)  %>%
  mutate(`健康維持活動` = 
           c(
    "開始するつもりはない",
    "6ヶ月以内に開始予定",
    "1ヶ月以内に開始予定",
    "開始済み半年未満",
    "開始して半年以上"
  )) %>%
  relocate(`健康維持活動`, .before = 1) %>%
  select(-2) %>%
  rename(!!! c("配信前" = "before_N",
              "配信後" = "...3" ))
}
timeschange("A",8, after_exercise, before_exercise, A_total_agg)

A8ex <- timeschange("A",8, after_exercise, before_exercise, A_total_agg) 
B14ex <- timeschange("B", 14, after_exercise, before_exercise, B_total_agg)
A8di <- timeschange("A",8, after_diet, before_diet, A_total_agg)
B14di <- timeschange("B", 14, after_diet, before_diet, B_total_agg)

B14di %>%
gt() %>%
  tab_spanner(
    label = md("**コンテンツ**"),
    columns = c("配信前", "配信後")) %>%
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
      tab_header(title = "食事_B群")%>%
  tab_options(table_body.hlines.color = "white",
              table_body.border.bottom.color = "white",
              column_labels.border.bottom.color = "black",
              column_labels.border.top.color = "white",
              table.border.top.color = "white",
              heading.border.bottom.color = "white") 

dat_xx$after_exercise <- as.factor(dat_xx$after_exercise)

dat_xx   %>%
  ggplot() +
  aes(x=A_total_agg, y=N, fill=after_exercise) +
  geom_bar(stat = "identity")+
  theme_ipsum(base_family = "HiraKakuPro-W3") +
  labs(title = "A群_運動")

plot_test2<-
dat_zf %>% filter(Group == "B") %>%
  group_nest(after_exercise, B_total_agg) %>%
  mutate(N = map_int(data, nrow)) %>%
  select(-3) %>% 
  ggplot() +
  aes(x=B_total_agg, y=N, fill=after_exercise) +
  geom_bar(stat = "identity") +
  theme_ipsum(base_family = "HiraKakuPro-W3")+labs(title = "B群_運動")

