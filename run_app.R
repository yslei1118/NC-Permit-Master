# 启动Permit考试刷题App
# 运行命令: Rscript run_app.R
# 或在R控制台中: source("run_app.R")

# 设置工作目录
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 加载shiny
library(shiny)

# 运行app
runApp(".", launch.browser = TRUE, port = 8888)
