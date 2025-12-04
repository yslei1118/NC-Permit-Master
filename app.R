# app.R - Permit考试刷题App

library(shiny)
library(shinydashboard)
library(dplyr)

# 加载工具函数
source("R/utils.R")

# 加载题库数据
questions <- read.csv("data/questions.csv", stringsAsFactors = FALSE)

# UI部分
ui <- dashboardPage(
  skin = "blue",

  # 顶部标题栏
  dashboardHeader(title = "NC Premit Master"),

  # 左侧边栏
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("主页", tabName = "home", icon = icon("home")),
      menuItem("常规选择题", tabName = "general", icon = icon("book")),
      menuItem("管制标志", tabName = "regulatory", icon = icon("stop-circle")),
      menuItem("警告标志", tabName = "warning", icon = icon("exclamation-triangle")),
      menuItem("高速公路标志", tabName = "highway", icon = icon("road")),
      menuItem("我的收藏", tabName = "favorites", icon = icon("star")),
      menuItem("错题本", tabName = "wrong", icon = icon("times-circle")),
      menuItem("统计分析", tabName = "stats", icon = icon("chart-bar")),
      # 隐藏的练习页面菜单项（用于编程切换）
      menuItem("练习", tabName = "practice", icon = icon("edit")),
      hr(),
      actionButton("reset_progress", "重置进度", icon = icon("refresh"),
                   style = "margin: 10px;")
    )
  ),

  # 主内容区
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* 全局样式 */
        body {
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', 'Helvetica Neue', Arial, sans-serif;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        }

        .content-wrapper {
          background: #f5f7fa !important;
        }

        /* 顶部栏配色与题目条一致 */
        .skin-blue .main-header .navbar,
        .skin-blue .main-header .logo {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
          color: #fff !important;
        }
        /* 顶部标题加粗 */
        .skin-blue .main-header .logo,
        .skin-blue .main-header .logo .logo-lg,
        .skin-blue .main-header .logo .logo-mini {
          font-weight: 700 !important;
          letter-spacing: 0.5px;
        }
        .skin-blue .main-header .logo:hover {
          background: linear-gradient(135deg, #5b6fe0 0%, #6e43a0 100%) !important;
        }
        .skin-blue .main-header .navbar .sidebar-toggle,
        .skin-blue .main-header .navbar .nav>li>a {
          color: #fff !important;
        }
        .skin-blue .main-header .navbar { border-bottom: none !important; }

        /* 隐藏侧边栏中的“练习”菜单项（仅用于编程切换） */
        .sidebar-menu a[data-toggle='tab'][data-value='practice'] { display: none !important; }
        .sidebar-menu li a[data-value='practice'] { display: none !important; }

        /* 主页大按钮 */
        .big-button {
          height: 150px;
          font-size: 24px;
          font-weight: 600;
          margin: 15px 10px;
          border-radius: 16px !important;
          border: none !important;
          box-shadow: 0 8px 16px rgba(0,0,0,0.15);
          transition: all 0.3s ease;
          position: relative;
          overflow: hidden;
        }

        .big-button:hover {
          transform: translateY(-4px);
          box-shadow: 0 12px 24px rgba(0,0,0,0.2);
        }

        .big-button:active {
          transform: translateY(-2px);
        }

        .btn-success.big-button {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
        }

        .btn-warning.big-button {
          background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%) !important;
        }

        .btn-danger.big-button {
          background: linear-gradient(135deg, #fa709a 0%, #fee140 100%) !important;
        }

        .btn-info.big-button {
          background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%) !important;
        }

        .btn-primary.big-button {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
        }

        /* 练习按钮 */
        .btn-lg {
          border-radius: 12px !important;
          padding: 15px 20px;
          font-weight: 600;
          font-size: 18px;
          border: none !important;
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
          transition: all 0.3s ease;
          margin: 8px 0;
        }

        .btn-lg:hover {
          transform: translateY(-2px);
          box-shadow: 0 6px 16px rgba(0,0,0,0.2);
        }

        .btn-primary {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
        }

        .btn-info {
          background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%) !important;
        }

        .btn-danger {
          background: linear-gradient(135deg, #fa709a 0%, #fee140 100%) !important;
        }

        .btn-success {
          background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%) !important;
        }

        .btn-warning {
          background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%) !important;
        }

        /* 题目框 */
        .question-box {
          font-size: 20px;
          padding: 30px;
          margin: 20px 0;
          background: white;
          border-radius: 16px;
          box-shadow: 0 4px 16px rgba(0,0,0,0.08);
          border-left: 5px solid #667eea;
        }

        /* 选项按钮 */
        .option-btn {
          width: 100%;
          height: 70px;
          font-size: 18px;
          margin: 12px 0;
          text-align: left;
          padding: 15px 25px;
          border-radius: 12px !important;
          border: 2px solid #e0e6ed !important;
          background: white !important;
          color: #2d3748 !important;
          transition: all 0.2s ease;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        }

        .option-btn:hover {
          border-color: #667eea !important;
          background: #f7fafc !important;
          transform: translateX(4px);
          box-shadow: 0 4px 12px rgba(102, 126, 234, 0.15);
        }

        .correct-answer {
          background: linear-gradient(135deg, #d4f4dd 0%, #b8f2c4 100%) !important;
          border-color: #48bb78 !important;
          color: #22543d !important;
          font-weight: 600;
        }

        .wrong-answer {
          background: linear-gradient(135deg, #fed7d7 0%, #fbb6ce 100%) !important;
          border-color: #f56565 !important;
          color: #742a2a !important;
          font-weight: 600;
        }

        /* 解释框 */
        .explanation-box {
          padding: 20px 25px;
          margin: 20px 0;
          border-radius: 12px;
          font-size: 16px;
          line-height: 1.6;
          background: linear-gradient(135deg, #e6f7ff 0%, #f0f8ff 100%);
          border-left: 4px solid #4facfe;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        }

        /* 统计卡片 */
        .stat-box {
          font-size: 16px;
          padding: 20px;
          background: white;
          border-radius: 12px;
          box-shadow: 0 2px 12px rgba(0,0,0,0.08);
        }

        .stat-box p {
          margin: 10px 0;
          font-size: 17px;
        }

        /* Box样式 */
        .box {
          border-radius: 12px !important;
          box-shadow: 0 2px 12px rgba(0,0,0,0.08) !important;
          border-top: none !important;
        }

        .box.box-solid {
          border-radius: 12px !important;
        }

        .box-header {
          border-radius: 12px 12px 0 0 !important;
        }

        /* Value Box */
        .small-box {
          border-radius: 12px !important;
          box-shadow: 0 4px 16px rgba(0,0,0,0.1) !important;
        }

        /* 模态框 */
        .modal-content {
          border-radius: 16px !important;
          border: none !important;
          box-shadow: 0 20px 60px rgba(0,0,0,0.3) !important;
        }

        .modal-header {
          border-radius: 16px 16px 0 0 !important;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          border-bottom: none !important;
        }

        .modal-title {
          font-weight: 600;
          font-size: 22px;
        }

        .modal-body {
          padding: 30px;
        }

        /* 题目列表项 */
        .question-list-item {
          border: 2px solid #e0e6ed;
          padding: 15px 20px;
          margin: 10px 0;
          border-radius: 12px;
          display: flex;
          justify-content: space-between;
          align-items: center;
          background: white;
          transition: all 0.2s ease;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        }

        .question-list-item:hover {
          border-color: #667eea;
          transform: translateX(4px);
          box-shadow: 0 4px 12px rgba(102, 126, 234, 0.15);
        }

        /* 页面标题 */
        h2, h3, h4 {
          color: #2d3748;
          font-weight: 600;
        }

        h2 {
          font-size: 32px;
          margin-bottom: 20px;
        }

        /* 进度条 */
        .progress {
          height: 8px;
          border-radius: 10px;
          background-color: #e2e8f0;
        }

        .progress-bar {
          border-radius: 10px;
          background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
        }

        /* 收藏和错题卡片 */
        .favorite-item, .wrong-item {
          border: 2px solid #e0e6ed;
          padding: 15px 20px;
          margin: 10px 0;
          border-radius: 12px;
          background: white;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);
          transition: all 0.2s ease;
        }

        .favorite-item:hover, .wrong-item:hover {
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
          transform: translateY(-2px);
        }

        /* 小按钮 */
        .btn-sm {
          border-radius: 8px !important;
          font-weight: 600;
          border: none !important;
          padding: 8px 16px;
        }
      "))
    ),

    tabItems(
      # 主页
      tabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 12,
            h2("Welcome to NC Premit Master!", style = "text-align: center; margin: 30px;"),
            p("请选择题目类型开始练习：", style = "text-align: center; font-size: 18px;")
          )
        ),
        fluidRow(
          column(
            width = 3,
            actionButton("go_general", "常规选择题",
                         icon = icon("book"),
                         class = "btn-success big-button btn-block")
          ),
          column(
            width = 3,
            actionButton("go_regulatory", "管制标志",
                         icon = icon("stop-circle"),
                         class = "btn-danger big-button btn-block")
          ),
          column(
            width = 3,
            actionButton("go_warning", "警告标志",
                         icon = icon("exclamation-triangle"),
                         class = "btn-warning big-button btn-block")
          ),
          column(
            width = 3,
            actionButton("go_highway", "高速公路标志",
                         icon = icon("road"),
                         class = "btn-info big-button btn-block")
          )
        ),
        fluidRow(
          column(
            width = 6,
            actionButton("go_favorites", "我的收藏",
                         icon = icon("star"),
                         class = "btn-info big-button btn-block")
          ),
          column(
            width = 6,
            actionButton("go_wrong_home", "错题本",
                         icon = icon("times-circle"),
                         class = "btn-primary big-button btn-block")
          )
        ),
        fluidRow(
          column(
            width = 12,
            h3("学习进度概览", style = "margin-top: 40px;"),
            uiOutput("home_stats")
          )
        )
      ),

      # 管制标志
      tabItem(
        tabName = "regulatory",
        h2("管制标志 Regulatory Signs"),
        p("管制标志（Regulatory Signs）包括STOP、YIELD、限速、禁止标志等，通常为红色、白色或黑色。", style = "color: #666; margin-bottom: 20px;"),
        fluidRow(
          column(width = 6, actionButton("regulatory_order", "顺序练习", class = "btn-primary btn-lg btn-block")),
          column(width = 6, actionButton("regulatory_random", "随机练习", class = "btn-info btn-lg btn-block"))
        ),
        fluidRow(
          column(width = 6, actionButton("regulatory_wrong", "错题重刷", class = "btn-danger btn-lg btn-block", style = "margin-top: 10px;")),
          column(width = 6, actionButton("regulatory_list", "题目列表", class = "btn-success btn-lg btn-block", style = "margin-top: 10px;"))
        ),
        hr(),
        uiOutput("regulatory_stats"),
        hr(),
        uiOutput("regulatory_question_list")
      ),

      # 警告标志
      tabItem(
        tabName = "warning",
        h2("警告标志 Warning Signs"),
        p("警告标志（Warning Signs）通常为黄色菱形或其他形状，警告驾驶者前方道路状况。", style = "color: #666; margin-bottom: 20px;"),
        fluidRow(
          column(width = 6, actionButton("warning_order", "顺序练习", class = "btn-primary btn-lg btn-block")),
          column(width = 6, actionButton("warning_random", "随机练习", class = "btn-info btn-lg btn-block"))
        ),
        fluidRow(
          column(width = 6, actionButton("warning_wrong", "错题重刷", class = "btn-danger btn-lg btn-block", style = "margin-top: 10px;")),
          column(width = 6, actionButton("warning_list", "题目列表", class = "btn-success btn-lg btn-block", style = "margin-top: 10px;"))
        ),
        hr(),
        uiOutput("warning_stats"),
        hr(),
        uiOutput("warning_question_list")
      ),

      # 高速公路标志
      tabItem(
        tabName = "highway",
        h2("高速公路标志 Highway Signs"),
        p("高速公路标志（Highway Signs）包括州际公路、州级公路、路线标记、服务设施等指示标志。", style = "color: #666; margin-bottom: 20px;"),
        fluidRow(
          column(width = 6, actionButton("highway_order", "顺序练习", class = "btn-primary btn-lg btn-block")),
          column(width = 6, actionButton("highway_random", "随机练习", class = "btn-info btn-lg btn-block"))
        ),
        fluidRow(
          column(width = 6, actionButton("highway_wrong", "错题重刷", class = "btn-danger btn-lg btn-block", style = "margin-top: 10px;")),
          column(width = 6, actionButton("highway_list", "题目列表", class = "btn-success btn-lg btn-block", style = "margin-top: 10px;"))
        ),
        hr(),
        uiOutput("highway_stats"),
        hr(),
        uiOutput("highway_question_list")
      ),

      # 常规选择题
      tabItem(
        tabName = "general",
        h2("常规选择题"),
        fluidRow(
          column(width = 6, actionButton("general_order", "顺序练习", class = "btn-primary btn-lg btn-block")),
          column(width = 6, actionButton("general_random", "随机练习", class = "btn-info btn-lg btn-block"))
        ),
        fluidRow(
          column(width = 6, actionButton("general_wrong", "错题重刷", class = "btn-danger btn-lg btn-block", style = "margin-top: 10px;")),
          column(width = 6, actionButton("general_list", "题目列表", class = "btn-success btn-lg btn-block", style = "margin-top: 10px;"))
        ),
        hr(),
        uiOutput("general_stats"),
        hr(),
        uiOutput("general_question_list")
      ),

      # 收藏
      tabItem(
        tabName = "favorites",
        h2("我的收藏"),
        actionButton("practice_favorites", "开始练习收藏题", class = "btn-warning btn-lg btn-block"),
        hr(),
        uiOutput("favorites_list")
      ),

      # 错题本
      tabItem(
        tabName = "wrong",
        h2("错题本"),
        fluidRow(
          column(width = 4, actionButton("wrong_all", "全部错题", class = "btn-danger btn-lg btn-block")),
          column(width = 4, actionButton("wrong_signs", "交通信号错题", class = "btn-warning btn-lg btn-block")),
          column(width = 4, actionButton("wrong_general", "常规题错题", class = "btn-info btn-lg btn-block"))
        ),
        hr(),
        uiOutput("wrong_list")
      ),

      # 统计分析
      tabItem(
        tabName = "stats",
        h2("统计分析"),
        fluidRow(
          valueBoxOutput("total_questions_box", width = 4),
          valueBoxOutput("answered_box", width = 4),
          valueBoxOutput("accuracy_box", width = 4)
        ),
        fluidRow(
          box(
            title = "正确率统计",
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            uiOutput("type_stats")
          ),
          box(
            title = "错题统计",
            width = 6,
            solidHeader = TRUE,
            status = "danger",
            uiOutput("wrong_stats")
          )
        )
      ),

      # 答题界面（完整页面）
      tabItem(
        tabName = "practice",
        uiOutput("practice_page")
      )
    )
  )
)

# Server部分
server <- function(input, output, session) {

  # 添加images资源路径映射
  addResourcePath("images", "images")

  # 响应式数据
  user_data <- reactiveVal(load_user_progress())
  current_practice <- reactiveValues(
    questions = NULL,
    current_index = 1,
    mode = NULL,
    type = NULL,
    answered = FALSE,
    return_page = NULL  # 记录退出后应该返回的页面
  )

  show_question_list <- reactiveValues(
    general = FALSE,
    regulatory = FALSE,
    warning = FALSE,
    highway = FALSE
  )

  # 主页统计
  output$home_stats <- renderUI({
    user <- user_data()
    total <- nrow(questions)
    answered <- get_answered_count(user)
    accuracy <- calculate_accuracy(user)

    general_total <- sum(questions$type == "general")
    regulatory_total <- sum(questions$type == "regulatory")
    warning_total <- sum(questions$type == "warning")
    highway_total <- sum(questions$type == "highway")
    general_answered <- get_answered_count(user, "general")
    regulatory_answered <- get_answered_count(user, "regulatory")
    warning_answered <- get_answered_count(user, "warning")
    highway_answered <- get_answered_count(user, "highway")

    div(
      class = "stat-box",
      fluidRow(
        column(4, valueBox(total, "总题数", icon = icon("book"), color = "blue")),
        column(4, valueBox(answered, "已完成", icon = icon("check"), color = "green")),
        column(4, valueBox(paste0(round(accuracy, 1), "%"), "总正确率", icon = icon("chart-line"), color = "yellow"))
      ),
      fluidRow(
        column(3,
               box(
                 title = "常规选择题",
                 width = 12,
                 solidHeader = TRUE,
                 status = "info",
                 p(paste0("总数：", general_total, "题")),
                 p(paste0("进度：", general_answered, "/", general_total, " (", round(general_answered/general_total*100, 1), "%)")),
                 p(paste0("错题：", sum(user$wrong_questions %in% questions$id[questions$type == "general"]), "题"))
               )
        ),
        column(3,
               box(
                 title = "管制标志",
                 width = 12,
                 solidHeader = TRUE,
                 status = "danger",
                 p(paste0("总数：", regulatory_total, "题")),
                 p(paste0("进度：", regulatory_answered, "/", regulatory_total, " (", round(regulatory_answered/regulatory_total*100, 1), "%)")),
                 p(paste0("错题：", sum(user$wrong_questions %in% questions$id[questions$type == "regulatory"]), "题"))
               )
        ),
        column(3,
               box(
                 title = "警告标志",
                 width = 12,
                 solidHeader = TRUE,
                 status = "warning",
                 p(paste0("总数：", warning_total, "题")),
                 p(paste0("进度：", warning_answered, "/", warning_total, " (", round(warning_answered/warning_total*100, 1), "%)")),
                 p(paste0("错题：", sum(user$wrong_questions %in% questions$id[questions$type == "warning"]), "题"))
               )
        ),
        column(3,
               box(
                 title = "高速公路标志",
                 width = 12,
                 solidHeader = TRUE,
                 status = "primary",
                 p(paste0("总数：", highway_total, "题")),
                 p(paste0("进度：", highway_answered, "/", highway_total, " (", round(highway_answered/highway_total*100, 1), "%)")),
                 p(paste0("错题：", sum(user$wrong_questions %in% questions$id[questions$type == "highway"]), "题"))
               )
        )
      )
    )
  })

  # 常规题统计
  output$general_stats <- renderUI({
    user <- user_data()
    total <- sum(questions$type == "general")
    answered <- get_answered_count(user, "general")
    wrong <- sum(user$wrong_questions %in% questions$id[questions$type == "general"])
    accuracy <- calculate_accuracy(user, "general")

    div(
      class = "stat-box",
      p(paste0("总题数：", total)),
      p(paste0("进度：", answered, "/", total, " (", round(answered/total*100, 1), "%)")),
      p(paste0("错题数：", wrong)),
      p(paste0("正确率：", round(accuracy, 1), "%"))
    )
  })

  # 管制标志统计
  output$regulatory_stats <- renderUI({
    user <- user_data()
    total <- sum(questions$type == "regulatory")
    answered <- get_answered_count(user, "regulatory")
    wrong <- sum(user$wrong_questions %in% questions$id[questions$type == "regulatory"])
    accuracy <- calculate_accuracy(user, "regulatory")

    div(
      class = "stat-box",
      p(paste0("总题数：", total)),
      p(paste0("进度：", answered, "/", total, " (", round(answered/total*100, 1), "%)")),
      p(paste0("错题数：", wrong)),
      p(paste0("正确率：", round(accuracy, 1), "%"))
    )
  })

  # 警告标志统计
  output$warning_stats <- renderUI({
    user <- user_data()
    total <- sum(questions$type == "warning")
    answered <- get_answered_count(user, "warning")
    wrong <- sum(user$wrong_questions %in% questions$id[questions$type == "warning"])
    accuracy <- calculate_accuracy(user, "warning")

    div(
      class = "stat-box",
      p(paste0("总题数：", total)),
      p(paste0("进度：", answered, "/", total, " (", round(answered/total*100, 1), "%)")),
      p(paste0("错题数：", wrong)),
      p(paste0("正确率：", round(accuracy, 1), "%"))
    )
  })

  # 高速公路标志统计
  output$highway_stats <- renderUI({
    user <- user_data()
    total <- sum(questions$type == "highway")
    answered <- get_answered_count(user, "highway")
    wrong <- sum(user$wrong_questions %in% questions$id[questions$type == "highway"])
    accuracy <- calculate_accuracy(user, "highway")

    div(
      class = "stat-box",
      p(paste0("总题数：", total)),
      p(paste0("进度：", answered, "/", total, " (", round(answered/total*100, 1), "%)")),
      p(paste0("错题数：", wrong)),
      p(paste0("正确率：", round(accuracy, 1), "%"))
    )
  })

  # 题目列表输出函数
  render_question_list <- function(type) {
    if (!show_question_list[[type]]) {
      return(NULL)
    }

    user <- user_data()
    type_questions <- questions[questions$type == type, ]

    div(
      h4("题目列表"),
      lapply(1:nrow(type_questions), function(i) {
        q <- type_questions[i, ]
        is_answered <- q$id %in% user$answers$question_id
        is_correct <- FALSE
        if (is_answered) {
          last_answer <- user$answers[user$answers$question_id == q$id, ]
          last_answer <- last_answer[nrow(last_answer), ]
          is_correct <- last_answer$correct
        }
        is_wrong <- q$id %in% user$wrong_questions
        is_favorite <- q$id %in% user$favorites

        status_text <- if (!is_answered) {
          "未答"
        } else if (is_correct) {
          "✓ 正确"
        } else {
          "✗ 错误"
        }

        status_color <- if (!is_answered) {
          "#999"
        } else if (is_correct) {
          "#28a745"
        } else {
          "#dc3545"
        }

        div(
          class = "question-list-item",
          div(
            style = "flex: 1;",
            span(strong(paste0("题 ", q$id)), style = "margin-right: 10px; font-size: 16px;"),
            span(status_text, style = paste0("color: ", status_color, "; margin-right: 10px; font-weight: 600;")),
            if (is_favorite) span("★", style = "color: #ffc107; font-size: 18px;"),
            p(substr(q$question, 1, 60), "...", style = "margin: 5px 0 0 0; color: #718096;")
          ),
          actionButton(paste0("start_from_", q$id), "开始练习",
                      class = "btn-sm btn-primary",
                      onclick = paste0("Shiny.setInputValue('start_from_id', ", q$id, ", {priority: 'event'});"))
        )
      })
    )
  }

  output$general_question_list <- renderUI({ render_question_list("general") })
  output$regulatory_question_list <- renderUI({ render_question_list("regulatory") })
  output$warning_question_list <- renderUI({ render_question_list("warning") })
  output$highway_question_list <- renderUI({ render_question_list("highway") })

  # 收藏列表
  output$favorites_list <- renderUI({
    user <- user_data()
    if (length(user$favorites) == 0) {
      return(p("还没有收藏题目", style = "color: gray; font-size: 16px;"))
    }

    fav_questions <- questions[questions$id %in% user$favorites, ]
    div(
      h4(paste0("共收藏 ", nrow(fav_questions), " 道题"), style = "color: #2d3748; margin: 20px 0;"),
      lapply(1:nrow(fav_questions), function(i) {
        q <- fav_questions[i, ]
        div(
          class = "favorite-item",
          p(strong(paste0("题 ", q$id, ": ", substr(q$question, 1, 80), "...")), style = "font-size: 16px; margin-bottom: 8px;"),
          p(paste0("类型：", switch(q$type,
                                   "general" = "常规选择题",
                                   "regulatory" = "管制标志",
                                   "warning" = "警告标志",
                                   "highway" = "高速公路标志",
                                   "常规选择题")), style = "color: #718096; font-size: 14px; margin: 0;")
        )
      })
    )
  })

  # 错题列表
  output$wrong_list <- renderUI({
    user <- user_data()
    if (length(user$wrong_questions) == 0) {
      return(p("暂无错题，继续加油！", style = "color: green; font-size: 16px;"))
    }

    wrong_q <- questions[questions$id %in% user$wrong_questions, ]
    div(
      h4(paste0("共有 ", nrow(wrong_q), " 道错题"), style = "color: #2d3748; margin: 20px 0;"),
      lapply(1:nrow(wrong_q), function(i) {
        q <- wrong_q[i, ]
        wrong_count <- get_wrong_count(user, q$id)
        div(
          class = "wrong-item",
          style = "border-left: 4px solid #f56565;",
          p(strong(paste0("题 ", q$id, ": ", substr(q$question, 1, 80), "...")), style = "font-size: 16px; margin-bottom: 8px;"),
          p(paste0("类型：", switch(q$type,
                                   "general" = "常规选择题",
                                   "regulatory" = "管制标志",
                                   "warning" = "警告标志",
                                   "highway" = "高速公路标志",
                                   "常规选择题"), " | 错误次数：", wrong_count), style = "color: #c53030; font-size: 14px; font-weight: 600; margin: 0;")
        )
      })
    )
  })

  # 统计分析
  output$total_questions_box <- renderValueBox({
    valueBox(
      nrow(questions),
      "总题数",
      icon = icon("book"),
      color = "blue"
    )
  })

  output$answered_box <- renderValueBox({
    user <- user_data()
    answered <- get_answered_count(user)
    total <- nrow(questions)
    valueBox(
      paste0(answered, "/", total),
      paste0("已完成 (", round(answered/total*100, 1), "%)"),
      icon = icon("check"),
      color = "green"
    )
  })

  output$accuracy_box <- renderValueBox({
    user <- user_data()
    accuracy <- calculate_accuracy(user)
    valueBox(
      paste0(round(accuracy, 1), "%"),
      "总正确率",
      icon = icon("chart-line"),
      color = "yellow"
    )
  })

  output$type_stats <- renderUI({
    user <- user_data()
    general_acc <- calculate_accuracy(user, "general")
    regulatory_acc <- calculate_accuracy(user, "regulatory")
    warning_acc <- calculate_accuracy(user, "warning")
    highway_acc <- calculate_accuracy(user, "highway")
    overall_acc <- calculate_accuracy(user)

    div(
      p(strong("常规选择题："), paste0(round(general_acc, 1), "%")),
      p(strong("管制标志："), paste0(round(regulatory_acc, 1), "%")),
      p(strong("警告标志："), paste0(round(warning_acc, 1), "%")),
      p(strong("高速公路标志："), paste0(round(highway_acc, 1), "%")),
      p(strong("总正确率："), paste0(round(overall_acc, 1), "%"))
    )
  })

  output$wrong_stats <- renderUI({
    user <- user_data()
    general_wrong <- sum(user$wrong_questions %in% questions$id[questions$type == "general"])
    regulatory_wrong <- sum(user$wrong_questions %in% questions$id[questions$type == "regulatory"])
    warning_wrong <- sum(user$wrong_questions %in% questions$id[questions$type == "warning"])
    highway_wrong <- sum(user$wrong_questions %in% questions$id[questions$type == "highway"])

    div(
      p(strong("常规选择题错题："), general_wrong),
      p(strong("管制标志错题："), regulatory_wrong),
      p(strong("警告标志错题："), warning_wrong),
      p(strong("高速公路标志错题："), highway_wrong),
      p(strong("总错题："), length(user$wrong_questions))
    )
  })

  # 开始练习函数
  start_practice <- function(type, mode, return_page = NULL) {
    cat("start_practice called: type=", type, "mode=", mode, "\n")
    user <- user_data()
    practice_questions <- get_practice_questions(questions, mode, user, type)
    cat("practice_questions count:", nrow(practice_questions), "\n")

    if (nrow(practice_questions) == 0) {
      if (mode == "wrong") {
        showModal(modalDialog(
          title = "提示",
          "暂无错题，继续加油！",
          easyClose = TRUE,
          footer = modalButton("确定")
        ))
      } else if (mode == "favorite") {
        showModal(modalDialog(
          title = "提示",
          "还没有收藏题目",
          easyClose = TRUE,
          footer = modalButton("确定")
        ))
      }
      return()
    }

    current_practice$questions <- practice_questions
    current_practice$current_index <- 1
    current_practice$mode <- mode
    current_practice$type <- type
    current_practice$answered <- FALSE

    # 设置返回页面
    if (!is.null(return_page)) {
      current_practice$return_page <- return_page
    } else if (mode == "favorite") {
      current_practice$return_page <- "favorites"
    } else if (mode == "wrong") {
      current_practice$return_page <- "wrong"
    } else if (!is.null(type)) {
      current_practice$return_page <- type
    } else {
      current_practice$return_page <- "home"
    }

    # 导航到练习页面
    cat("Navigating to practice page\n")
    updateTabItems(session, "sidebar", "practice")
    cat("Navigation complete\n")
  }

  # 从特定题目开始练习
  start_from_question <- function(question_id) {
    q <- questions[questions$id == question_id, ]
    if (nrow(q) == 0) return()

    type <- q$type[1]
    type_questions <- questions[questions$type == type, ]

    # 找到该题目在类型中的索引
    start_index <- which(type_questions$id == question_id)
    if (length(start_index) == 0) return()

    # 重新排序：从选中的题目开始
    if (start_index > 1) {
      type_questions <- type_questions[c(start_index:nrow(type_questions), 1:(start_index-1)), ]
    }

    current_practice$questions <- type_questions
    current_practice$current_index <- 1
    current_practice$mode <- "order"
    current_practice$type <- type
    current_practice$answered <- FALSE
    current_practice$return_page <- type  # 返回到题目所属的类型页面

    # 导航到练习页面
    updateTabItems(session, "sidebar", "practice")
  }

  # 监听从题目列表开始练习
  observeEvent(input$start_from_id, {
    start_from_question(input$start_from_id)
  })

  # 导航到不同页面
  observeEvent(input$go_general, { updateTabItems(session, "sidebar", "general") })
  observeEvent(input$go_regulatory, { updateTabItems(session, "sidebar", "regulatory") })
  observeEvent(input$go_warning, { updateTabItems(session, "sidebar", "warning") })
  observeEvent(input$go_highway, { updateTabItems(session, "sidebar", "highway") })
  observeEvent(input$go_favorites, { updateTabItems(session, "sidebar", "favorites") })
  observeEvent(input$go_wrong_home, { updateTabItems(session, "sidebar", "wrong") })
  observeEvent(input$go_wrong, { updateTabItems(session, "sidebar", "wrong") })

  # 开始各种练习模式
  # 常规选择题
  observeEvent(input$general_order, { start_practice("general", "order") })
  observeEvent(input$general_random, { start_practice("general", "random") })
  observeEvent(input$general_wrong, { start_practice("general", "wrong") })

  # 管制标志
  observeEvent(input$regulatory_order, { start_practice("regulatory", "order") })
  observeEvent(input$regulatory_random, { start_practice("regulatory", "random") })
  observeEvent(input$regulatory_wrong, { start_practice("regulatory", "wrong") })

  # 警告标志
  observeEvent(input$warning_order, { start_practice("warning", "order") })
  observeEvent(input$warning_random, { start_practice("warning", "random") })
  observeEvent(input$warning_wrong, { start_practice("warning", "wrong") })

  # 高速公路标志
  observeEvent(input$highway_order, { start_practice("highway", "order") })
  observeEvent(input$highway_random, { start_practice("highway", "random") })
  observeEvent(input$highway_wrong, { start_practice("highway", "wrong") })

  # 题目列表按钮
  observeEvent(input$general_list, {
    show_question_list$general <- !show_question_list$general
  })
  observeEvent(input$regulatory_list, {
    show_question_list$regulatory <- !show_question_list$regulatory
  })
  observeEvent(input$warning_list, {
    show_question_list$warning <- !show_question_list$warning
  })
  observeEvent(input$highway_list, {
    show_question_list$highway <- !show_question_list$highway
  })

  # 收藏和错题本
  observeEvent(input$practice_favorites, { start_practice(NULL, "favorite") })
  observeEvent(input$wrong_all, { start_practice(NULL, "wrong") })
  observeEvent(input$wrong_signs, { start_practice(NULL, "wrong") })
  observeEvent(input$wrong_general, { start_practice("general", "wrong") })
  observeEvent(input$wrong_regulatory, { start_practice("regulatory", "wrong") })
  observeEvent(input$wrong_warning, { start_practice("warning", "wrong") })
  observeEvent(input$wrong_highway, { start_practice("highway", "wrong") })

  # 答题界面（完整页面）
  output$practice_page <- renderUI({
    cat("practice_page rendering...\n")
    practice_q <- current_practice$questions
    idx <- current_practice$current_index
    cat("practice_q is null:", is.null(practice_q), "\n")
    if (!is.null(practice_q)) {
      cat("practice_q rows:", nrow(practice_q), "idx:", idx, "\n")
    }

    # 如果没有练习题目，返回空页面或提示
    if (is.null(practice_q) || idx > nrow(practice_q)) {
      cat("Returning NULL - no practice questions\n")
      return(NULL)
    }

    current_q <- practice_q[idx, ]
    user <- user_data()
    is_favorite <- current_q$id %in% user$favorites

    # 创建答题界面（完整页面）
    div(
      # 顶部进度条
      div(
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 20px; color: white; border-radius: 8px; margin-bottom: 20px;",
        fluidRow({
          user_progress <- user_data()
          answered_in_set <- length(unique(user_progress$answers$question_id[user_progress$answers$question_id %in% practice_q$id]))
          total_in_set <- nrow(practice_q)
          pct <- ifelse(total_in_set > 0, round(answered_in_set/total_in_set*100, 1), 0)
          tagList(
            column(4, h3(paste0("题号: ", current_q$id), style = "margin: 0;")),
            column(4, h3(paste0("进度: ", answered_in_set, "/", total_in_set), style = "margin: 0; text-align: center;")),
            column(4, h3(paste0(pct, "%"), style = "margin: 0; text-align: right;"))
          )
        })
      ),

      # 题目内容
      div(
        class = "question-box",
        h4(current_q$question),
        # 显示图片（如果存在）
        if (!is.na(current_q$image_ref) && current_q$image_ref != "") {
          image_path <- paste0("images/", current_q$image_ref)
          image_file <- file.path("images", current_q$image_ref)
          if (file.exists(image_file)) {
            # 图片文件存在，显示图片
            div(
              style = "text-align: center; margin: 20px 0;",
              img(src = image_path, style = "max-width: 100%; max-height: 400px; border: 2px solid #ddd; border-radius: 5px;")
            )
          } else {
            # 图片文件不存在，显示提示
            div(
              style = "text-align: center; margin: 20px 0; padding: 30px; background-color: #fff3cd; border: 2px dashed #856404; border-radius: 5px;",
              p(strong("[需要图片]"), style = "color: #856404; font-size: 18px; margin-bottom: 10px;"),
              p(paste0("此题包含图示，图片文件: ", current_q$image_ref), style = "color: #856404; margin: 0;"),
              p("请参考PDF原文件或添加图片到 images/ 目录", style = "color: #856404; font-size: 14px; margin-top: 5px;")
            )
          }
        }
      ),

      # 选项
      fluidRow(
        column(12,
               actionButton("answer_a", paste0("A. ", current_q$option_a), class = "option-btn btn-default")
        )
      ),
      fluidRow(
        column(12,
               actionButton("answer_b", paste0("B. ", current_q$option_b), class = "option-btn btn-default")
        )
      ),
      fluidRow(
        column(12,
               actionButton("answer_c", paste0("C. ", current_q$option_c), class = "option-btn btn-default")
        )
      ),
      fluidRow(
        column(12,
               actionButton("answer_d", paste0("D. ", current_q$option_d), class = "option-btn btn-default")
        )
      ),

      # 答案反馈
      uiOutput("answer_feedback"),

      # 底部按钮
      div(
        style = "margin-top: 30px; padding: 20px; border-top: 2px solid #e0e6ed;",
        fluidRow({
          at_last <- idx >= nrow(practice_q)

          tagList(
            column(3,
                   actionButton("exit_practice", "退出", icon = icon("times"), class = "btn-lg btn-block", style = "background: #6c757d !important;")
            ),
            column(3,
                   actionButton("toggle_favorite_btn",
                                ifelse(is_favorite, "取消收藏", "收藏"),
                                icon = icon("star"),
                                class = paste("btn-lg btn-block", ifelse(is_favorite, "btn-warning", "btn-default")))
            ),
            column(3, actionButton("prev_question", "上一题", icon = icon("arrow-left"), class = "btn-lg btn-block")),
            column(3,
                   tagList(
                     actionButton("next_question", "下一题", icon = icon("arrow-right"), class = "btn-primary btn-lg btn-block"),
                     if (at_last) actionButton("finish_practice", "完成练习", icon = icon("check"), class = "btn-success btn-lg btn-block")
                   )
            )
          )
        })
      )
    )
  })

  # 答题反馈
  output$answer_feedback <- renderUI({
    if (!current_practice$answered) return(NULL)

    practice_q <- current_practice$questions
    idx <- current_practice$current_index
    current_q <- practice_q[idx, ]

    user <- user_data()
    last_answer <- tail(user$answers[user$answers$question_id == current_q$id, ], 1)

    if (nrow(last_answer) == 0) return(NULL)

    if (last_answer$correct) {
      div(
        class = "explanation-box correct-answer",
        h4(icon("check"), " 回答正确！"),
        p(strong("解析："), current_q$explanation)
      )
    } else {
      div(
        class = "explanation-box wrong-answer",
        h4(icon("times"), " 回答错误！"),
        p(strong("正确答案："), current_q$answer),
        p(strong("解析："), current_q$explanation)
      )
    }
  })

  # 处理答题
  answer_question <- function(user_answer) {
    practice_q <- current_practice$questions
    idx <- current_practice$current_index
    current_q <- practice_q[idx, ]

    # 记录答案
    user <- user_data()
    user <- record_answer(user, current_q$id, user_answer, current_q$answer)
    # 更新练习进度
    user <- update_practice_progress(user, current_q$id, current_practice$type)
    user_data(user)
    save_user_progress(user)

    current_practice$answered <- TRUE
  }

  observeEvent(input$answer_a, { if (!current_practice$answered) answer_question("A") })
  observeEvent(input$answer_b, { if (!current_practice$answered) answer_question("B") })
  observeEvent(input$answer_c, { if (!current_practice$answered) answer_question("C") })
  observeEvent(input$answer_d, { if (!current_practice$answered) answer_question("D") })

  # 上一题
  observeEvent(input$prev_question, {
    practice_q <- current_practice$questions
    if (is.null(practice_q)) return(NULL)
    if (current_practice$current_index > 1) {
      current_practice$current_index <- current_practice$current_index - 1
    } else {
      current_practice$current_index <- nrow(practice_q)
    }
    current_practice$answered <- FALSE
  })

  # 下一题
  observeEvent(input$next_question, {
    practice_q <- current_practice$questions
    if (is.null(practice_q)) return(NULL)
    if (current_practice$current_index < nrow(practice_q)) {
      current_practice$current_index <- current_practice$current_index + 1
    } else {
      current_practice$current_index <- 1
    }
    current_practice$answered <- FALSE
  })

  # 收藏切换
  observeEvent(input$toggle_favorite_btn, {
    practice_q <- current_practice$questions
    idx <- current_practice$current_index
    current_q <- practice_q[idx, ]

    user <- user_data()
    user <- toggle_favorite(user, current_q$id)
    user_data(user)
    save_user_progress(user)
  })

  # 退出练习
  observeEvent(input$exit_practice, {
    # 保存返回页面
    return_to <- current_practice$return_page

    # 重置练习状态
    current_practice$questions <- NULL
    current_practice$current_index <- 1
    current_practice$mode <- NULL
    current_practice$type <- NULL
    current_practice$answered <- FALSE
    current_practice$return_page <- NULL

    # 返回到对应的页面
    if (!is.null(return_to)) {
      updateTabItems(session, "sidebar", return_to)
    } else {
      updateTabItems(session, "sidebar", "home")
    }
  })

  # 完成练习
  observeEvent(input$finish_practice, {
    # 保存返回页面
    return_to <- current_practice$return_page

    # 重置练习状态
    current_practice$questions <- NULL
    current_practice$current_index <- 1
    current_practice$mode <- NULL
    current_practice$type <- NULL
    current_practice$answered <- FALSE
    current_practice$return_page <- NULL

    # 返回到对应的页面
    if (!is.null(return_to)) {
      updateTabItems(session, "sidebar", return_to)
    } else {
      updateTabItems(session, "sidebar", "home")
    }

    # 显示完成提示
    showModal(modalDialog(
      title = "练习完成",
      h4("恭喜完成本次练习！"),
      easyClose = TRUE,
      footer = modalButton("确定")
    ))
  })

  # 重置进度
  observeEvent(input$reset_progress, {
    showModal(modalDialog(
      title = "确认重置",
      "确定要重置所有进度吗？此操作不可恢复。",
      footer = tagList(
        modalButton("取消"),
        actionButton("confirm_reset", "确认重置", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_reset, {
    user_data(reset_progress())
    removeModal()
    showNotification("进度已重置", type = "warning")
  })
}

# 运行应用
shinyApp(ui = ui, server = server)
