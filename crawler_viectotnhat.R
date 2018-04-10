

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


get_information_for_job <- function(x) {
  
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
    
    
    all_info <- data.frame(luong = wage, 
                           kinh_nghiem = kinh_nghiem, 
                           bang_cap = bang_cap, 
                           so_luong = so_luong, 
                           sub_group = sub_group, 
                           location = location, 
                           position = position, 
                           hinh_thuc = hinh_thuc, 
                           gioi_tinh = gioi_tinh, 
                           link_source = x)
    
  }
  
  return(all_info)
  
}

job_link$job_link[2] %>% get_information_for_job()

# Hàm lấy các thông tin cho một list các link về công việc: 

get_information_for_group_job <- function(x) {
  a <- lapply(x, get_information_for_job)
  return(do.call("rbind", a))
}


get_information_for_group_job(job_link$job_link)



