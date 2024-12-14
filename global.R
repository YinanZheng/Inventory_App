# global.R

# Load required libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(dplyr)
library(DT)
library(digest) # For has value
library(magick) # For compression
library(stringi)
library(baRcodeR)
library(DBI)
library(RMariaDB)
library(uuid)
library(plotly)
library(rlang)

# Source all modular functions
source("./modules/supplierModuleServer.R")
source("./modules/typeModuleServer.R")
source("./modules/typeModuleUI.R")

source("utils.R")

# 定义轮询间隔（以毫秒为单位）
poll_interval <<- 10000  # 每 10 秒检查一次

host_url <<- "http://54.254.120.88/"

# Size of barcode paper (in cm)
page_width <<- 4
page_height <<- 2
size_unit <<- "cm"

# 定义需要记录时间的状态
status_columns <<- list(
  "采购" = "PurchaseTime",
  "国内入库" = "DomesticEntryTime",
  "国内出库" = "DomesticExitTime",
  "国内售出" = "DomesticSoldTime",
  "美国入库" = "UsEntryTime",
  "美国售出" = "UsSoldTime",
  "退货" = "ReturnTime"
)

# 定义瑕疵和修复的状态
defect_statuses <<- c("瑕疵", "修复", "无瑕")
