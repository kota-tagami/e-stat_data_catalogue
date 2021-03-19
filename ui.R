##=======================================================##
## >> Section No.1 <<
## >> Header << 
##=======================================================##
header <- dashboardHeader(
  title = "e-Stat Data Catalogue",
  dropdownMenu(
    type = "notifications",
    icon = icon("info"),
    headerText = paste0("ver ", version),
    badgeStatus = NULL,
    notificationItem(
      "GitHub", icon = icon("github"), status = "primary", href = github_url
    )
  )
)


##=======================================================##
## >> Section No.2 <<
## >> Sidebar << 
##=======================================================##
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "統計表を探す", 
      tabName = "getStatsList", 
      icon = icon("search")
    ),
    menuItem(
      "メタ情報を調べる",
      tabName = "getMetaInfo",
      icon = icon("list-alt")
    )
  )
)


##=======================================================##
## >> Section No.3 <<
## >> Body << 
##=======================================================##

#-----------------------------#
# >> GetStatsList << #
#-----------------------------#
body_getstatslist_input <- tagList(
  selectInput(
    "statName", "統計調査：",
    choices = stat_list$stat_name,
    selectize = T
  ),
  textInput(
    "searchWord", "キーワード検索：",
    placeholder = "性　年齢　学歴"
  ),
  actionButton(
    "searchStatsList", "検索", icon = icon("search"), width = "100%"
  )
)

body_getstatslist <- 
  tabItem(
    tabName = "getStatsList",
    h2("統計表情報"),
    fluidRow(
      box(
        body_getstatslist_input,
        status = "primary", 
        solidHeader = TRUE,
        collapsible = F,
        width = 6
      ),
      box(
        verbatimTextOutput(
          "getStatListResStatus",
          placeholder = F
        ),
        title = "Status",
        status = "primary", 
        solidHeader = TRUE,
        collapsible = F,
        width = 6
      )
    ),
    fluidRow(
      box(
        DT::DTOutput("getStatListResTable"),
        title = "Outputs",
        status = "primary", 
        solidHeader = TRUE,
        collapsible = F,
        width = 12
      )
    )
  )


#-----------------------------#
# >> GetMetaInfo << #
#-----------------------------#
body_getmetainfo_input <- tagList(
  textInput(
    "statsDataId", "統計表ID：",
    placeholder = "半角数字10桁"
  ),
  actionButton(
    "searchMetaInfo", "検索", icon = icon("search"), width = "100%"
  )
)

body_getmetainfo <- 
  tabItem(
    tabName = "getMetaInfo",
    h2("メタ情報"),
    fluidRow(
      box(
        body_getmetainfo_input,
        status = "primary", 
        solidHeader = TRUE,
        collapsible = F,
        width = 6
      ),
      box(
        verbatimTextOutput(
          "getMetaInfoResStatus",
          placeholder = F
        ),
        title = "Status",
        status = "primary", 
        solidHeader = TRUE,
        collapsible = F,
        width = 6
      )
    ),
    fluidRow(
      box(
        DT::DTOutput("getMetaInfoResTable"),
        title = "Outputs",
        status = "primary", 
        solidHeader = TRUE,
        collapsible = F,
        width = 12
      )
    )
  )


#-----------------------------#
# >> Combined << #
#-----------------------------#
body <- dashboardBody(
  add_busy_spinner(
    spin = "flower",
    color = "orange",
    margins = c(10, 20)
  ),
  tabItems(
    body_getstatslist,
    body_getmetainfo
  )
)


##=======================================================##
## >> Section No.4 <<
## >> UI << 
##=======================================================##
ui <- dashboardPage(
  title = "e-Stat Data Catalogue",
  header = header,
  sidebar = sidebar,
  body = body
)
