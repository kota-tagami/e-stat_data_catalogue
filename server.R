server <- function(input, output, session) {
  #-----------------------------#
  # >> getStatsList << #
  #-----------------------------#
  
  res_getStatsList <- eventReactive(input$searchStatsList, {
    statsCode <- 
      stat_list %>% 
      filter(
        stat_name == input$statName
      ) %>% 
      pull(stat_code)
      
    searchWord <- 
      input$searchWord %>% 
      stri_trans_nfkc() %>% 
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

  df_list <- reactive({
    df_list <- res_getStatsList() %>% 
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
      )
    df_list
  })
  
  output$getStatListResTable <- 
    DT::renderDT({
      df_list() %>% 
        DT::datatable(
          rownames = F
        )
    })
  
  output$getStatListResTable_csv <- downloadHandler(
    filename = function() {
      paste("statslsit", ".csv", sep = "")
    },
    content = function(file) {
      readr::write_excel_csv(
        df_list(), 
        file
      )
    }
  )

  output$getStatListResTable_excel_csv <- downloadHandler(
    filename = function() {
      paste("statslsit", ".csv", sep = "")
    },
    content = function(file) {
      readr::write_excel_csv(
        df_list() %>% 
          mutate(
            across(everything(), ~ str_c('="', ., '"'))
          ), 
        file
      )
    }
  )
  
  
  #-----------------------------#
  # >> getMetaInfo << #
  #-----------------------------#
  
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
  
  df_meta <- reactive({
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
      select(-c(named_list)) %>% 
      rename(
        `分類事項コード` = var_id, 
        `分類事項ラベル` = var_lab, 
        `値コード` = val_id, 
        `値ラベル` = val_lab,
        `値レベル` = val_level,
        `親コード` = parent_code,
        `単位` = val_unit
      ) %>% 
      select(
        `分類事項コード`, `分類事項ラベル`,
        `値コード`, `値ラベル`, `親コード`, `単位`
      ) %>% 
      mutate(across(everything(), as.character))
    
    df_meta
    
  })
  
  output$getMetaInfoResTable <-
    DT::renderDT({
      df_meta() %>% 
        DT::datatable(
          rownames = F
        )
    })
  
  output$getMetaInfoResTable_csv <- downloadHandler(
    filename = function() {
      paste(as.character(input$statsDataId), ".csv", sep = "")
    },
    content = function(file) {
      readr::write_excel_csv(
        df_meta(), 
        file
      )
    }
  )

  output$getMetaInfoResTable_excel_csv <- downloadHandler(
    filename = function() {
      paste(as.character(input$statsDataId), ".csv", sep = "")
    },
    content = function(file) {
      readr::write_excel_csv(
        df_meta() %>% 
          mutate(
            across(everything(), ~ str_c('="', ., '"'))
          ), 
        file
      )
    }
  )
  
}
