# notifications.R

# Unified function for showing notifications
show_notification <- function(message, type = "message") {
  showNotification(
    paste0(if (type == "error") "错误: " else "提示: ", message),
    type = type,
    duration = 10
  )
}