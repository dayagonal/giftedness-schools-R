# =========================================================
# Шаг 1: ЗАГРУЗКА ИНСТРУМЕНТОВ И ДАННЫХ
# =========================================================

# Подключаем библиотеки
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(stringr)
library(patchwork)
library(hms) # Нужен для работы со временем (duration_min)

# Загружаем файлы
df <- read_excel("talent_database.xlsx")
nobd <- readRDS("nobd.RDS")
teachers <- read_excel("педагоги_по категориям.xlsx")

# =========================================================
# Шаг 2: ЧИСТКА И ОБЪЕДИНЕНИЕ
# =========================================================

# 
df <- df %>% 
  select(-c(ФИО, `E-mail`, Телефон)) %>%
  rename(Названия_школ = `Организация образования`)

nobd <- nobd %>% 
  rename(Названия_школ = `Наименование организации`)

common_schools <- intersect(df$Названия_школ, nobd$Названия_школ)

df1 <- df %>% filter(Названия_школ %in% common_schools)
nobd1 <- nobd %>% filter(Названия_школ %in% common_schools)

teachers <- teachers %>%
  rename(Названия_школ = `Наименование организации`)

teachers <- teachers %>% filter(Названия_школ %in% common_schools)
dat_t <- df1
dat_teach <- left_join(dat_t, teachers %>% distinct(Названия_школ, .keep_all = TRUE), by="Названия_школ")

# =========================================================
# Шаг 3: СОЗДАНИЕ ПЕРЕМЕННЫХ (И 'ОДАРЕННОСТЬ' v5 - ФИНАЛ)
# =========================================================

# --- Сначала считаем время в минутах ---
dat_teach <- dat_teach %>%
  mutate(
    duration_min = case_when(
      is_hms(duration) ~ as.numeric(hms(duration))/60,
      is_ms(duration) ~ as.numeric(ms(duration))/60,
      TRUE ~ NA_real_
    )
  )

# --- НОВЫЙ БЛОК: ОПРЕДЕЛЯЕМ 'ОДАРЕННОСТЬ' ПО ВСЕМ ПРАВИЛАМ ---

# Правило 3: Создаем таблицу "чистых" попыток (>= 30 минут)
dat_valid_attempts <- dat_teach %>%
  filter(duration_min >= 30 | is.na(duration_min))

# Правило 1 (СУММИРОВАНИЕ): Считаем СУММУ баллов для каждого ID
student_total_scores <- dat_valid_attempts %>%
  group_by(ID) %>%
  summarise(Total_Score = sum(`Итоговый бал`, na.rm = TRUE)) %>%
  ungroup()

# Правило 2.1 (Тай-брейк МАТЕМАТИКА): Находим балл по Математике
math_scores <- dat_valid_attempts %>%
  filter(`Олимпиада (предмет)` == "Математика") %>%
  group_by(ID) %>% 
  summarise(Балл_Математика = max(`Итоговый бал`, na.rm = TRUE)) %>%
  ungroup()

# Правило 2.2 (Тай-брейк АНГЛИЙСКИЙ): Находим балл по Английскому
english_scores <- dat_valid_attempts %>%
  filter(`Олимпиада (предмет)` == "Английский язык") %>%
  group_by(ID) %>% 
  summarise(Балл_Английский = max(`Итоговый бал`, na.rm = TRUE)) %>%
  ungroup()

# Собираем все данные для рейтинга в одну таблицу
ranking_data <- student_total_scores %>%
  left_join(math_scores, by = "ID") %>%
  left_join(english_scores, by = "ID") %>%
  # Заменяем NA (если не участвовал) на 0 для честного рейтинга
  mutate(
    Балл_Математика = ifelse(is.na(Балл_Математика), 0, Балл_Математика),
    Балл_Английский = ifelse(is.na(Балл_Английский), 0, Балл_Английский)
  )

# СОРТИРУЕМ по 3-м правилам и отбираем ТОП-1000 УЧЕНИКОВ
top_1000_students <- ranking_data %>%
  arrange(
    desc(Total_Score),       # 1. Сначала по Сумме Баллов
    desc(Балл_Математика), # 2. Потом по Математике
    desc(Балл_Английский)  # 3. Потом по Английскому
  ) %>%
  slice_head(n = 1000)

# Получаем УНИКАЛЬНЫЙ список ID 1000 одаренных учеников
gifted_student_IDs <- top_1000_students$ID

# Создаем финальную колонку `Одаренность` (1/0)
dat_teach <- dat_teach %>%
  mutate(
    Одаренность = ifelse(ID %in% gifted_student_IDs, 1, 0)
  )

# --- КОНЕЦ НОВОГО БЛОКА ---


# --- Создаем переменные по школе ---
dat_teach <- dat_teach %>% 
  mutate(across(c(equipment_books_total, equipment_pc_total, staff_teachers, staff_educators, students), 
                ~suppressWarnings(as.numeric(.))))

dat_teach <- dat_teach %>%
  mutate(
    comp_per_100 = ifelse(!is.na(students) & students > 0, 100 * equipment_pc_total / students, NA_real_),
    books_per_stu = ifelse(!is.na(students) & students > 0 & !is.na(equipment_books_total), equipment_books_total / students, NA_real_),
    high_qual_teach = (`высшая категория` + `первая категория`) / total_teachers,
    expert_share_teach = (`педагог-эксперт` + `педагог-исследователь` + `педагог-мастер`) / total_teachers
  ) %>%
  mutate(
    urban = if_else(distDc <= 3, "город", "село"), 
    urban = factor(urban)
  ) %>%
  mutate(
    shifts = factor(shifts),
    one_shift = if_else(is.na(shifts) & shifts == 1, "1 смена", "2-3 смены"),
    one_shift = factor(one_shift, levels = c("2-3 смены", "1 смена"))
  )


# =========================================================
# Шаг 4: ПОВОРОТ В ШИРОКИЙ ФОРМАТ
# =========================================================

# 
last_attempt <- dat_teach %>%
  mutate(`Дата завершения` = str_replace_all(`Дата завершения`, "`", "")) %>%
  mutate(`Дата завершения` = dmy_hm(`Дата завершения`)) %>%
  arrange(ID, `Олимпиада (предмет)`, `Дата завершения`) %>%
  group_by(ID, `Олимпиада (предмет)`) %>%
  slice_max(order_by = `Дата завершения`, n = 1, with_ties = FALSE) %>%
  ungroup()

dat_wide <- last_attempt %>%
  pivot_wider(
    id_cols = c(ID, Названия_школ, Одаренность, comp_per_100, books_per_stu, 
                total_teachers, high_qual_teach, expert_share_teach, 
                urban, one_shift, students, distDc, `Территориальная принадлежность`), 
    names_from = `Олимпиада (предмет)`,
    values_from = `Итоговый бал`
  )


dat_wide <- dat_wide %>%
  mutate(
    Математика = as.numeric(unlist(Математика)),
    Химия = as.numeric(unlist(Химия)),
    География = as.numeric(unlist(География)),
    Физика = as.numeric(unlist(Физика)),
    Биология = as.numeric(unlist(Биология)),
    `Английский язык` = as.numeric(unlist(`Английский язык`)),
    `История Казахстана` = as.numeric(unlist(`История Казахстана`)),
    Информатика = as.numeric(unlist(Информатика))
  )

# =========================================================
# БЛОК 1: СЧИТАЕМ ПЕРСЕНТИЛИ
# =========================================================


subjects <- c("Математика", "Физика", "География", "Химия", "Английский язык", 
              "История Казахстана", "Информатика", "Биология")

dat_wide <- dat_wide %>%
  mutate(
    across(  
      all_of(subjects),  
      list(p = ~percent_rank(.) * 100),
      .names = "{.col}_p"
    )
  )

# =========================================================
# БЛОК 2: ЛОГАРИФМИРОВАНИЕ
# =========================================================


dat_wide <- dat_wide %>%
  mutate(
    log_comp_per_100 = log(comp_per_100 + 1),
    log_books_per_stu = log(books_per_stu + 1),
    log_students = log(students + 1)
  )

# =========================================================
# БЛОК 3: ПЕРЕДЕЛАННЫЕ МОДЕЛИ (ГИПОТЕЗЫ)
# =========================================================



# --- Модель для "Одаренности" (бинарная, GLM) ---
glm_h2 <- glm(
  Одаренность ~ log_comp_per_100 + total_teachers + high_qual_teach + 
    urban + one_shift, 
  data = dat_wide, 
  family = "binomial" 
)
summary(glm_h2)


# --- Модели по каждому предмету (цикл) ---
subjects_p <- paste0(subjects, "_p")

for (subj_p in subjects_p) {
  
  formula_h2 <- as.formula(
    paste(
      subj_p, 
      "~ log_comp_per_100 + total_teachers + high_qual_teach + urban + one_shift"
    )
  )
  
  lm_h2 <- lm(formula_h2, data = dat_wide)
  
  print(paste("===== РЕЗУЛЬТАТЫ ГИПОТЕЗЫ 2 ДЛЯ:", subj_p, "====="))
  print(summary(lm_h2))
}


# --- Другие модели (Гипотеза 3, 4, 6) ---
glm_h3 <- glm(
  Одаренность ~ log_comp_per_100 + I(log_comp_per_100^2) + log_books_per_stu +
    urban + one_shift,
  data = dat_wide,
  family = "binomial"
)
summary(glm_h3)

t.test(Математика_p ~ urban, data = dat_wide)

lm_h6 <- lm(
  Математика_p ~ `Территориальная принадлежность` + urban + one_shift,
  data = dat_wide
)
summary(lm_h6)