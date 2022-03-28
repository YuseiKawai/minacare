# ML Modeling
## select data
dat_b %>% names()
dat_k <- dat_b %>% 
  select(-(1:24)) %>%
  select(-7,-8, -17) %>%
  select(-starts_with(
    c("08_", "11_", "12_", "13_", "h1_", 
      "h2_", "h3_", "14_", "15_", "17_", "18_","20_")
  )) 

## correlation matrix
dat_k %>% select(where(is.numeric)) %>% visdat::vis_cor()

## split HL-score to 3 groups
dat_k$total_h4 %>% summary()
dat_k <- dat_k %>% 
  mutate("HL_group" = case_when(
    .$total_h4 < 47  ~ "1",
    .$total_h4 >=47 & .$total_h4<57 ~ "2",
    .$total_h4 >= 57 ~ "3",
  ))
dat_k$HL_group <- as.factor(dat_k$HL_group)
dat_k %>% 
  ggplot(aes(x=total_h4, fill=HL_group)) +
  geom_histogram(bins = 45)+
  theme_ipsum(base_family = "HiraKakuPro-W3")+
  scale_fill_ipsum()

## 19_women: 3 <- NA / 21_women : 6 <- NA / sex <- factor
sexlist <- c("男性" = "1",  "女性" = "2")
dat_l <- dat_k %>%
  mutate(`19_women` = str_replace_na(`19_women`),
         `21_women` = str_replace_na(`21_women`)) %>% 
  mutate(`19_women` = as.factor(str_replace(`19_women`, "NA", "3")),
         `21_women` = as.factor(str_replace(`21_women`, "NA", "6"))) %>% 
  mutate(sex = as.factor(str_replace_all(sex, pattern = sexlist))) 

## reduce explanatory variable
dat_m <- dat_l %>% select(-name, -starts_with("total_"))
dat_n <- dat_m %>% select(-starts_with(c("03_","04_","24_",)))

## split data to train & test
datm_split <- initial_split(dat_m,
                            prop = 0.8,
                            strata = "HL_group")
datm_train <- training(datm_split)
datm_test <- testing(datm_split)

## prepare recipi
datm_recipi <- datm_train %>%
  recipe(formula = HL_group ~ .) 
## preparation recipi
datm_prepped <- prep(datm_recipi)
## bake trainData & testData
datm_train_prepped <- 
  datm_prepped %>% bake(new_data =NULL)
datm_test_prepped <- 
  datm_prepped %>% bake(new_data = datm_test)

## rfmodel (w/o any hyper parameter search)
rf_model <- rand_forest(trees = 3000, mtry = 10) %>%
  set_engine("randomForest") %>%
  set_mode("classification")
## gbmodel (w/o any hyper parameter search)
gb_model <- boost_tree(trees = 3000,
                       mtry = 5,
                       tree_depth = 29) %>%
  set_mode("classification") %>%
  set_engine("xgboost")

## fittng to model
rf_fit <- rf_model %>%
  fit(HL_group ~., data = datm_train_prepped)

df_rf_model_predict <- datm_test_prepped %>%
  select(HL_group) %>%
  bind_cols(
    predict(rf_fit, new_data = datm_test_prepped))

## manual confusion matrix
table(df_rf_model_predict$HL_group, df_rf_model_predict$.pred_class) 

cc_workflow <- workflow() %>%
  add_recipe(datm_recipi) %>%
  add_model(rf_model)
fit(cc_workflow, data = datm_train)

## update recipi
datm_recipi2 <- datm_recipi %>% 
  step_select(-starts_with(c("10_", "19_", "20_", "24_")))

cc_workflow %>%
  update_recipe(datm_recipi2) %>%
  update_model(gb_model) %>%
  fit(data = datm_train)



# re-engineering
dat_m %>% filter(`04_かかりつけの医師` == "1") %>% 
  ggplot() +
  aes(x=HL_group) +
  geom_bar(stat = "count")

dat_m %>% filter(`03_親（義両親を含む）` == "1",
                 `03_配偶者・パートナー027` == "1")  %>% 
  ggplot() +
  aes(x=HL_group) +
  geom_bar(stat = "count")


## split data to train and test
dat_m %>% names() %>% dput()

dat_n <- dat_m %>% setNames(
  c("03_祖父母", "03_親（義両親を含む）", "03_配偶者・パートナー027", 
    "03_兄弟姉妹028", "03_子供029", "03_同居はしていない（単身）", 
    "04_友人", "04_配偶者・パートナー034", "04_兄弟姉妹035", 
    "04_両親", "04_子供037", "04_職場の同僚", "04_職場の上司", 
    "04_かかりつけの医師", 
    "easyToSpeak", "healthState", 
    "women1", "women2", "women3", "medicalCheck", "women4", 
    "women5", "exercise", "diet", "prevention", "vitalSign", 
    "age", "sex", "HL_group"))


## dat_n test
datn_split <- initial_split(dat_n,
                            prop = 0.8,
                            strata = "HL_group")

datn_train <- training(datn_split)
datn_test <- testing(datn_split)

datn_train %>% names() %>% dput()

datn_recipi <- datn_train %>%
  recipe(formula = HL_group ~ .) %>%
  step_dummy(c("easyToSpeak", "healthState", 
               "women1", "women2", "women3", "medicalCheck", "women4", "women5", 
               "exercise", "diet","vitalSign","prevention", "sex"))

datn_prepped <- prep(datn_recipi)
datn_train_prepped <- datn_prepped %>% bake(new_data = NULL)
datn_test_prepped <- datn_prepped %>% bake(new_data = datn_test)

rf_model %>% fit(HL_group ~., data = datn_train_prepped)
gb_model %>% fit(HL_group ~., data = datn_train_prepped)

datn_train_prepped %>% glimpse()


# 女だけでモデリング
dat_l %>% glimpse()
dat_nl <- dat_n %>% filter(sex == "2") 

dat_nl_split <- initial_split(data = dat_nl,
                              prop = 0.8,
                              strata = HL_group)

dat_nl_train <- training(dat_nl_split)
dat_nl_test <- testing(dat_nl_split)

dat_nl_prepped <- dat_nl_train %>%
  recipe(formula = HL_group ~.) %>%
  step_select(-c("sex", "age","prevention")) %>%
  step_select(-starts_with("03_")) %>%
  step_dummy(
    c("easyToSpeak", "healthState", 
      "women1", "women2", "women3", "medicalCheck", "women4", "women5", 
      "exercise", "diet","vitalSign")
  ) %>%
  prep()

dat_nl_train_prepped <- dat_nl_prepped %>% bake(NULL)
dat_nl_test_prepped <- dat_nl_prepped %>% bake(dat_nl_test)

rf_model <- rand_forest(trees = 3000, mtry = 5) %>%
  set_engine("randomForest") %>%
  set_mode("classification")

rf_model %>% 
  fit(HL_group~., data = dat_nl_train_prepped)
dat_nl_train_prepped %>% glimpse()
