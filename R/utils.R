# utils.R - 工具函数

# 初始化用户数据
init_user_data <- function() {
  list(
    answers = data.frame(
      question_id = integer(),
      user_answer = character(),
      correct = logical(),
      timestamp = character(),
      stringsAsFactors = FALSE
    ),
    favorites = integer(),
    wrong_questions = integer(),
    # 记录每个类型最后练习的题目ID
    last_practice = list(
      general = 0,
      regulatory = 0,
      warning = 0
    )
  )
}

# 加载或创建用户进度数据
load_user_progress <- function(file_path = "data/user_progress.rds") {
  if (file.exists(file_path)) {
    readRDS(file_path)
  } else {
    init_user_data()
  }
}

# 保存用户进度
save_user_progress <- function(user_data, file_path = "data/user_progress.rds") {
  saveRDS(user_data, file_path)
}

# 记录答题
record_answer <- function(user_data, question_id, user_answer, correct_answer) {
  is_correct <- (user_answer == correct_answer)

  # 添加答题记录
  new_record <- data.frame(
    question_id = question_id,
    user_answer = user_answer,
    correct = is_correct,
    timestamp = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )

  user_data$answers <- rbind(user_data$answers, new_record)

  # 更新错题本
  if (!is_correct) {
    if (!(question_id %in% user_data$wrong_questions)) {
      user_data$wrong_questions <- c(user_data$wrong_questions, question_id)
    }
  } else {
    # 如果答对了，从错题本中移除（可选：答对2次才移除）
    # 这里采用答对1次就移除的策略
    user_data$wrong_questions <- setdiff(user_data$wrong_questions, question_id)
  }

  return(user_data)
}

# 切换收藏状态
toggle_favorite <- function(user_data, question_id) {
  if (question_id %in% user_data$favorites) {
    user_data$favorites <- setdiff(user_data$favorites, question_id)
  } else {
    user_data$favorites <- c(user_data$favorites, question_id)
  }
  return(user_data)
}

# 获取错题次数
get_wrong_count <- function(user_data, question_id) {
  if (nrow(user_data$answers) == 0) return(0)

  answers <- user_data$answers[user_data$answers$question_id == question_id, ]
  sum(!answers$correct)
}

# 计算正确率
calculate_accuracy <- function(user_data, type = NULL) {
  if (nrow(user_data$answers) == 0) return(0)

  if (!is.null(type)) {
    # 需要加载题库来筛选题型
    questions <- read.csv("data/questions.csv", stringsAsFactors = FALSE)
    type_questions <- questions$id[questions$type == type]
    answers <- user_data$answers[user_data$answers$question_id %in% type_questions, ]
  } else {
    answers <- user_data$answers
  }

  if (nrow(answers) == 0) return(0)

  # 只计算每道题最后一次的答题结果
  latest_answers <- answers %>%
    group_by(question_id) %>%
    slice_tail(n = 1) %>%
    ungroup()

  mean(latest_answers$correct) * 100
}

# 获取已答题数
get_answered_count <- function(user_data, type = NULL) {
  if (nrow(user_data$answers) == 0) return(0)

  if (!is.null(type)) {
    questions <- read.csv("data/questions.csv", stringsAsFactors = FALSE)
    type_questions <- questions$id[questions$type == type]
    answers <- user_data$answers[user_data$answers$question_id %in% type_questions, ]
  } else {
    answers <- user_data$answers
  }

  length(unique(answers$question_id))
}

# 重置进度
reset_progress <- function() {
  user_data <- init_user_data()
  save_user_progress(user_data)
  return(user_data)
}

# 更新练习进度
update_practice_progress <- function(user_data, question_id, type) {
  if (is.null(user_data$last_practice)) {
    user_data$last_practice <- list(general = 0, regulatory = 0, warning = 0)
  }

  if (!is.null(type) && type %in% c("general", "regulatory", "warning")) {
    user_data$last_practice[[type]] <- question_id
  }

  return(user_data)
}

# 获取练习题目列表
get_practice_questions <- function(questions, mode, user_data, type = NULL) {
  # 根据题型筛选
  if (!is.null(type)) {
    questions <- questions[questions$type == type, ]
  }

  # 根据模式筛选
  if (mode == "wrong") {
    questions <- questions[questions$id %in% user_data$wrong_questions, ]
  } else if (mode == "favorite") {
    questions <- questions[questions$id %in% user_data$favorites, ]
  }

  # 根据模式排序
  if (mode == "random") {
    questions <- questions[sample(nrow(questions)), ]
  } else if (mode == "order" && !is.null(type)) {
    # 顺序模式：从上次位置开始
    if (!is.null(user_data$last_practice) && !is.null(user_data$last_practice[[type]])) {
      last_id <- user_data$last_practice[[type]]
      if (last_id > 0 && last_id %in% questions$id) {
        # 找到上次位置的索引
        last_index <- which(questions$id == last_id)
        # 重新排序：从上次位置之后开始，然后接上前面的
        if (last_index < nrow(questions)) {
          questions <- questions[c((last_index + 1):nrow(questions), 1:last_index), ]
        }
      }
    }
  }

  return(questions)
}
