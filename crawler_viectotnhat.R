

library(tidyverse)
library(magrittr)
library(rvest)
library(stringr)
library(httr)
library(httr)
rm(list = ls())

#----------------------------------------------------------------
#  Hàm lấy link của một nhóm CV chọn trước được đăng trên 1 page
#----------------------------------------------------------------


get_link_for_job_group <- function(x) {
  Sys.sleep(5)
  m <- read_lines(x)
  m <- m[str_detect(m, "https://")]
  m <- m[str_detect(m, "title=")]
  m <- m[!str_detect(m, "com-name text-gray fontSize14")]
  m <- m[str_detect(m, ".html")]
  m <- m[str_detect(m, "[0-9]")]
  u <- str_locate_all(m, "https")
  u <- do.call("rbind", u) %>% as.vector()
  link <- str_sub(m, u[1:(length(u) / 2)], str_count(m) - 2)
  return(link)
  
}

#---------------------------------------------
#  Hàm lấy tất cả link của một nhóm công việc
#---------------------------------------------

get_all_link_for_job_group <- function(base_url, n_pages) {
  all_link <- lapply(paste0(base_url, 1:n_pages), get_link_for_job_group)
  all_link <- unlist(all_link)
  all_link <- all_link[!duplicated(all_link)] %>% as.character()
  all_link <- data.frame(job_link = all_link, 
                         job_type = c(rep(str_sub(base_url, 34, str_count(base_url) - 15))))
  return(all_link %>% mutate_if(is.factor, as.character))
}

job_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-kinh-doanh-f32.html?page=", 1)


#-----------------------------------------------------
#  Hàm lấy các thông tin về công việc được đăng tuyển
#-----------------------------------------------------

x <- "https://viectotnhat.com/tuyen-nhan-vien-kinh-doanh-ha-noi-1112855.html"


get_information_for_job <- function(x) {
  
  Sys.sleep(5)
  k <- http_status(GET(x))
  
  if (k$category == "Success") {
    
    m <- read_html(x)
    
    wage <- m %>%
      html_nodes(xpath = '//*[@id="main-content"]/div/div/div[1]/div[4]/div/div[1]/div[1]/ul/li[1]') %>% 
      html_text() %>% 
      str_trim()
    
    if (str_count(wage) == 0) {wage <- NA}
    
    kinh_nghiem <- m %>%
      html_nodes(xpath = '//*[@id="main-content"]/div/div/div[1]/div[4]/div/div[1]/div[1]/ul/li[3]') %>% 
      html_text() %>% 
      str_trim()
    
    if (str_count(kinh_nghiem) == 0) {kinh_nghiem <- NA}
    
    bang_cap <- m %>%
      html_nodes(xpath = '//*[@id="main-content"]/div/div/div[1]/div[4]/div/div[1]/div[1]/ul/li[5]') %>% 
      html_text() %>% 
      str_trim()
    
    if (str_count(bang_cap) == 0) {bang_cap <- NA}
    
    so_luong <- m %>%
      html_nodes(xpath = '//*[@id="main-content"]/div/div/div[1]/div[4]/div/div[1]/div[1]/ul/li[7]') %>% 
      html_text() %>% 
      str_trim()
    
    if (str_count(so_luong) == 0) {so_luong <- NA}
    
    sub_group <- m %>%
      html_nodes(xpath = '//*[@id="main-content"]/div/div/div[1]/div[4]/div/div[1]/div[1]/ul/li[9]') %>% 
      html_text() %>% 
      str_trim()
    
    if (str_count(sub_group) == 0) {sub_group <- NA}
    
    location <- m %>%
      html_nodes(xpath = '//*[@id="main-content"]/div/div/div[1]/div[4]/div/div[1]/div[1]/ul/li[2]') %>% 
      html_text() %>% 
      str_trim()
    
    if (str_count(location) == 0) {location <- NA}
    
    position <- m %>%
      html_nodes(xpath = '//*[@id="main-content"]/div/div/div[1]/div[4]/div/div[1]/div[1]/ul/li[4]') %>% 
      html_text() %>% 
      str_trim()
    
    if (str_count(position) == 0) {position <- NA}
    
    hinh_thuc <- m %>%
      html_nodes(xpath = '//*[@id="main-content"]/div/div/div[1]/div[4]/div/div[1]/div[1]/ul/li[6]') %>% 
      html_text() %>% 
      str_trim()
    
    if (str_count(hinh_thuc) == 0) {hinh_thuc <- NA}
    
    gioi_tinh <- m %>%
      html_nodes(xpath = '//*[@id="main-content"]/div/div/div[1]/div[4]/div/div[1]/div[1]/ul/li[8]') %>% 
      html_text() %>% 
      str_trim()
    
    if (str_count(gioi_tinh) == 0) {gioi_tinh <- NA}
    
    job_name <- m %>%
      html_nodes(xpath = '//*[@id="main-content"]/div/div/div[1]/div[2]/div[1]/h1') %>% 
      html_text() %>% 
      str_trim()
    
    if (str_count(job_name) == 0) {job_name <- NA}
    
    com_name <- m %>%
      html_nodes(xpath = '//*[@id="main-content"]/div/div/div[1]/div[2]/div[1]/h2') %>% 
      html_text() %>% 
      str_trim()
    
    if (str_count(com_name) == 0) {com_name <- NA}
    
    com_add <- m %>%
      html_nodes(xpath = '//*[@id="main-content"]/div/div/div[1]/div[4]/div/div[1]/div[1]/div[9]/ul/li[2]') %>% 
      html_text() %>% 
      str_trim()
    
    if (str_count(com_add) == 0) {com_add <- NA}
    
    deadline <- m %>%
      html_nodes(xpath = '//*[@id="main-content"]/div/div/div[1]/div[2]/div[1]/div[2]') %>% 
      html_text() %>% 
      str_trim()
    
    if (str_count(deadline) == 0) {deadline <- NA}
    
    all_info <- data.frame(luong = wage, 
                           kinh_nghiem = kinh_nghiem, 
                           bang_cap = bang_cap, 
                           so_luong = so_luong, 
                           sub_group = sub_group, 
                           location = location, 
                           position = position, 
                           hinh_thuc = hinh_thuc, 
                           gioi_tinh = gioi_tinh, 
                           job_name = job_name, 
                           com_name = com_name, 
                           com_add = com_add, 
                           deadline = deadline, 
                           link_source = x)
    
  }
  
  return(all_info)
  
}

# Test hàm: 

job_link$job_link[2] %>% get_information_for_job()

# Hàm lấy các thông tin cho một list các link về công việc phương án 1: 

get_information_for_group_job1 <- function(x) {
  a <- lapply(x, get_information_for_job)
  return(do.call("rbind", a))
}

get_information_for_group_job1(job_link$job_link[1:2]) # Kiểu này có thể lỗi connection. 

# Phương án 2: 

get_information_for_group_job2 <- function(list_job_link) {
  n <- length(list_job_link)
  job_inf <- vector("list", length = n)
  for (i in seq_along(list_job_link)) {
    Sys.sleep(3)
    job_inf[[i]] <- get_information_for_job(list_job_link[i])
  }
  job_inf <- do.call("bind_rows", job_inf)
  return(job_inf)
}

get_information_for_group_job2(job_link$job_link[1:2])


# Phương án 3:  

get_information_for_group_job3 <- function(list_job_link) {
  job_inf <- tryCatch(lapply(list_job_link, get_information_for_job))
  job_inf <- do.call("bind_rows", job_inf)
  return(job_inf)
}

get_information_for_group_job3(job_link$job_link[1:2])


# Phương án 4 (tham khảo: https://somerealnumbers.wordpress.com/2015/05/17/11/): 


get_information_for_group_job4 <- function(list_job_link) {
  n <- length(list_job_link)
  all_df <- data.frame()
  for (i in 1:n) {
    job_inf <- tryCatch({get_information_for_job(list_job_link[i])}, 
                        error = function(i){return(data.frame())})
    all_df <- bind_rows(all_df, job_inf)
    
  }
  return(all_df)
}

get_information_for_group_job4(job_link$job_link[1:2])



# Test tốc độ: 
library(microbenchmark)

microbenchmark(
  u1 <- get_information_for_group_job1(job_link$job_link[1:2]), 
  u2 <- get_information_for_group_job2(job_link$job_link[1:2]), 
  u3 <- get_information_for_group_job3(job_link$job_link[1:2]), 
  u4 <- get_information_for_group_job4(job_link$job_link[1:2]),
  times = 3
  
)

# Phương án 2 lâu nhất với thời gian trung bình là 17 giây, Phương án 1, 3
# và 4 có thời gian trung bình thấp hơn cả. 


#-----------------------------------------------
#  Tất cả việc thuộc nhóm kinh doanh  (1100)
#-----------------------------------------------

kinh_doanh_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-kinh-doanh-f32.html?page=", 60)
kinh_doanh_job <- get_information_for_group_job4(kinh_doanh_link$job_link)


# n <- length(kinh_doanh_link$job_link)
# 
# all_df_kinh_doanh <- data.frame()
# for (i in 1:n) {
#   job_inf <- tryCatch({get_information_for_job(kinh_doanh_link$job_link[i])}, 
#                       error = function(i) {return(data.frame())})
#   all_df_kinh_doanh <- bind_rows(all_df_kinh_doanh, job_inf)
#   }




