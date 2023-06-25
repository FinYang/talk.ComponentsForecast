## ---- library ----
library(tidyverse)
devtools::load_all(".")

## ---- path ----
raw_path <- function(...)
  normalizePath(file.path("../../Fin_PhD_main/pieces/progress_review/data", ...))
if(file.exists(raw_path("visnights.qs"))){
  file.copy(raw_path("visnights.qs"), "data/visnights.qs", overwrite = TRUE)
}
if(file.exists(raw_path("clustering.qs"))){
  file.copy(raw_path("clustering.qs"), "data/clustering.qs", overwrite = TRUE)
}

if(file.exists(raw_path("tourism_mcb.qs"))){

  tab_acc_h_id <- qs::qread(raw_path("tourism_mcb.qs"))

  mcb_df <- tab_acc_h_id %>%
    mutate(.n_comp = as.numeric(substr(.bt_model, 3, 3))) %>%
    filter(.n_comp!=9 | is.na(.n_comp)) %>%
    filter(.lag %in% c("48", "Inf") | is.na(.lag)) %>%
    mutate(
      .pc_model = case_when(
        is.na(.pc_model) & .model == "DFM" ~ "DFM",
        is.na(.pc_model) ~ "ETS",
        TRUE ~ .pc_model
      ),
      Model = case_when(
        .pc_model == "DFM" ~ "Benchmark DFM",
        .pc_model == "ETS" ~ "Benchmark ETS",
        substr(.bt_model, 1, 2) == "rf" ~ "RF",
        is.na(.alpha) ~ "OLS",
        TRUE ~ "DLS"),
      name = case_when(
        grepl("Benchmark", Model) ~ Model,
        TRUE ~ paste(
          ifelse(is.na(.alpha), Model, paste0(Model, .alpha)),
          .n_comp, .lag, sep = "_"
        ))
    ) %>%
    select(-.bt_model, -Model)

  mcb_df_bench <- mcb_df %>%
    filter(.pc_model %in% c("ETS", "DFM"))
  mcb_df_comp <- mcb_df %>%
    anti_join(mcb_df_bench,
              by = join_by(.pc_model, .model, .h, .lag, .alpha, .sample_id, mRMSSE, .n_comp, name))


  mcb_nest_bench <- mcb_df_bench %>%
    select(.h, name, mRMSSE, .sample_id) %>%
    group_by(.h) %>%
    nest() %>%
    rename(data_ets = data)

  mcb_df_top3 <- mcb_df_comp %>%
    filter(.lag == "Inf") %>%
    filter(.h %in% c(1, 12)) %>%
    group_by(.pc_model, .h, name) %>%
    summarise(mRMSSE = mean(mRMSSE), .groups = "drop_last") %>%
    slice_min(mRMSSE, n=3) %>%
    ungroup() %>%
    select(-mRMSSE) %>%
    left_join(mcb_df_comp) %>%
    mutate(name = gsub("_Inf", "", paste(.pc_model, name)))
  mcb_nest_top3 <- mcb_df_top3 %>%
    select( .h, name, mRMSSE, .sample_id) %>%
    group_by(.h) %>%
    nest() %>%
    rename(data_comp = data)

  mcb_pdata_top3 <-
    left_join(
      mcb_nest_top3, filter(mcb_nest_bench, .h %in% c(1, 12))
    ) %>%
    transmute(data = map2(data_comp, data_ets, bind_rows)) %>%
    # mutate(data = map(data, \(x) browser())) %>%
    mutate(data = map(data, pivot_wider, values_from = "mRMSSE")) %>%
    mutate(data = map(data, select, -1)) %>%
    mutate(mcb = map(data, tsutils::nemenyi, plottype = "none")) %>%
    mutate(mcb_data = map(mcb, as.data.frame)) %>%
    select(.h, mcb_data) %>%
    unnest(mcb_data) %>%
    ungroup()

  qs::qsave(mcb_pdata_top3, "data/mcb_pdata_top3.qs")

}
