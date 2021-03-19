server <- function(input, output, session) {
  res_getStatsList <- eventReactive(input$searchStatsList, {
    statsCode <- 
      stat_list %>% 
      filter(
        stat_name == input$statName
      ) %>% 
      pull(stat_code)
      
    searchWord <- 
      input$searchWord %>% 
      stringi::stri_trans_nfkc() %>% 
      str_trim(side = "both") %>% 
      str_replace_all(pattern = " ", replacement = " AND ")
    
    param <- list(
      "statsCode" = statsCode,
      "searchWord" = searchWord,
      "explanationGetFlg" = "N"
    )
    
    res <- estat_api(api_method = "getStatsList", params = param)
    res
  })

  
  output$getStatListResStatus <- 
    renderPrint({
      res_getStatsList() %>% 
        pluck("GET_STATS_LIST", "RESULT")
    })

  
  output$getStatListResTable <- 
    DT::renderDT({
      res_getStatsList() %>% 
        pluck("GET_STATS_LIST", "DATALIST_INF", "TABLE_INF") %>% 
        tibble(data = .) %>% 
        hoist(
          data,
          stat_table_id = "@id",
          stat_name_data = "STATISTICS_NAME_SPEC",
          stat_table_title_data = "TITLE_SPEC"
        ) %>% 
        hoist(
          stat_name_data,
          stat_name = "TABULATION_CATEGORY"
        ) %>% 
        hoist(
          stat_table_title_data,
          stat_table_title = "TABLE_NAME"
        ) %>% 
        select(
          `統計表ID` = stat_table_id, 
          `統計表` = stat_table_title, 
          `統計調査` = stat_name
        ) %>% 
        DT::datatable(
          rownames = F
        )
    })
  
  
  
  res_getMetaInfo <- eventReactive(input$searchMetaInfo, {
    param <- list(
      "statsDataId" = as.character(input$statsDataId),
      "explanationGetFlg" = "N"
    )

    res <- estat_api(api_method = "getMetaInfo", params = param)
    res
  })
  
  
  output$getMetaInfoResStatus <- 
    renderPrint({
      res_getMetaInfo() %>% 
        pluck("GET_META_INFO", "RESULT")
    })
  
  output$getMetaInfoResTable <-
    DT::renderDT({
      df_meta <-
        res_getMetaInfo() %>%
        pluck("GET_META_INFO", "METADATA_INF", "CLASS_INF", "CLASS_OBJ") %>%
        tibble(data = .) %>%
        hoist(
          data,
          var_id = "@id", var_lab = "@name",
          class_data = "CLASS"
        ) %>%
        mutate(
          named_list = class_data %>%
            map_lgl(~ names(.) %>% is.null %>% isFALSE)
        ) %>%
        {
          dt <- .

          dt1 <-
            dt %>%
            filter(named_list) %>%
            hoist(
              class_data,
              val_id = "@code", val_lab = "@name",
              val_level = "@level", val_unit = "@unit"
            )

          dt2 <-
            dt %>%
            filter(!named_list) %>%
            unnest_auto(class_data) %>%
            hoist(
              class_data,
              val_id = "@code", val_lab = "@name",
              val_level = "@level", parent_code = "@parentCode"
            )

          bind_rows(dt1, dt2)
        } %>%
        select(-c(named_list))
      
      df_meta %>% 
        rename(
          `分類事項コード` = var_id, 
          `分類事項ラベル` = var_lab, 
          `値コード` = val_id, 
          `値ラベル` = val_lab,
          `値レベル` = val_level,
          `親コード` = parent_code,
          `単位` = val_unit
        ) %>%
        DT::datatable(rownames = F)
    })
  
}
