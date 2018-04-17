
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#     Phần 1: Thu thập dữ liệu từ web và xử lí sơ bộ
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


library(tidyverse)
library(magrittr)
library(rvest)
library(stringr)
library(httr)



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
  all_link <- all_link %>% mutate(job_group = base_url)
  return(all_link %>% mutate_if(is.factor, as.character))
}

# job_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-kinh-doanh-f32.html?page=", 1)


#-----------------------------------------------------
#  Hàm lấy các thông tin về công việc được đăng tuyển
#-----------------------------------------------------

# x <- "https://viectotnhat.com/tuyen-nhan-vien-kinh-doanh-ha-noi-1112855.html"


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

# job_link$job_link[2] %>% get_information_for_job()

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

# get_information_for_group_job2(job_link$job_link[1:2])


# Phương án 3:  

get_information_for_group_job3 <- function(list_job_link) {
  job_inf <- tryCatch(lapply(list_job_link, get_information_for_job))
  job_inf <- do.call("bind_rows", job_inf)
  return(job_inf)
}

# get_information_for_group_job3(job_link$job_link[1:2])


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

# get_information_for_group_job4(job_link$job_link[1:2])



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



# Nhân sự: 

nhan_su_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-nhan-su-f40.html?page=", 13)
nhan_su_job <- get_information_for_group_job4(nhan_su_link$job_link[!duplicated(nhan_su_link$job_link)])

# Thiết kế - mĩ thuật: 

thiet_ke_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-thiet-ke-my-thuat-f49.html?page=", 12)
thiet_ke_job <- get_information_for_group_job4(thiet_ke_link$job_link[!duplicated(thiet_ke_link$job_link)])

# Xây dựng: 
xay_dung_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-xay-dung-f52.html?page=", 15)
xay_dung_job <- get_information_for_group_job4(xay_dung_link$job_link[!duplicated(xay_dung_link$job_link)])

# Giáo dục - đào tạo: 

giao_duc_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-giao-duc-dao-tao-thu-vien-f26.html?page=", 11)
giao_duc_job <- get_information_for_group_job4(giao_duc_link$job_link[!duplicated(giao_duc_link$job_link)])

# Kiến trúc - nội thất: 

kien_truc_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-kien-truc-noi-that-f31.html?page=", 11)
kien_truc_job <- get_information_for_group_job4(kien_truc_link$job_link[!duplicated(kien_truc_link$job_link)])


# Nhóm công việc không tên:
khac_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-khac-f55.html?page=", 9)
khac_job <- get_information_for_group_job4(khac_link$job_link[!duplicated(khac_link$job_link)])


# Bảo Hiểm: 

bao_hiem_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-tu-van-bao-hiem-f11.html?page=", 8)
bao_hiem_job <- get_information_for_group_job4(bao_hiem_link$job_link[!duplicated(bao_hiem_link$job_link)])


# Hành chính - thư kí - trợ lí (500): 

hanh_chinh_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-hanh-chinh-thu-ky-tro-ly-f29.html?page=", 33)
hanh_chinh_job <- get_information_for_group_job4(hanh_chinh_link$job_link[!duplicated(hanh_chinh_link$job_link)])


# Sinh viên ra trường - thực tập học việc: 

sinh_vien_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-sinh-vien-moi-tot-nghiep-thuc-tap-f35.html?page=", 24)
sinh_vien_job <- get_information_for_group_job4(sinh_vien_link$job_link[!duplicated(sinh_vien_link$job_link)])


# Marketing (500): 

marketing_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-quang-cao-marketing-pr-f45.html?page=", 34)
marketing_job <- get_information_for_group_job4(marketing_link$job_link[!duplicated(marketing_link$job_link)])

# Bất động sản: 
bat_dong_san_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-bat-dong-san-f13.html?page=", 30)
bat_dong_san_job <- get_information_for_group_job4(bat_dong_san_link$job_link[!duplicated(bat_dong_san_link$job_link)])

# Công Nghệ Thông Tin: 
cong_nghe_tt_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-cong-nghe-thong-tin-f17.html?page=", 23)
cong_nghe_tt_job <- get_information_for_group_job4(cong_nghe_tt_link$job_link[!duplicated(cong_nghe_tt_link$job_link)])

# Cơ khí: 

co_khi_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-co-khi-ki-thuat-ung-dung-f16.html?page=", 21)
co_khi_job <- get_information_for_group_job4(co_khi_link$job_link[!duplicated(co_khi_link$job_link)])

# Du lịch - nhà hàng - khách sạn: 
nha_hang_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-du-lich-nha-hang-khach-san-f23.html?page=", 30)
nha_hang_job <- get_information_for_group_job4(nha_hang_link$job_link[!duplicated(nha_hang_link$job_link)])

# Điện tử - điện lạnh: 
dien_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-dien-dien-tu-dien-lanh-f22.html?page=", 30)
dien_job <- get_information_for_group_job4(dien_link$job_link[!duplicated(dien_link$job_link)])

# Chăm sóc khách hàng (650) - làm ngày 16 - 04: 

cham_soc_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-cham-soc-khach-hang-f21.html?page=", 43)
cham_soc_job_16_04 <- get_information_for_group_job4(cham_soc_link$job_link[!duplicated(cham_soc_link$job_link)])


# Việc kinh doanh: 


kinh_doanh_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-kinh-doanh-f32.html?page=", 75)
kinh_doanh_job <- get_information_for_group_job4(kinh_doanh_link$job_link[!duplicated(kinh_doanh_link$job_link)])


# Việc bán hàng (1000): 
ban_hang_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-ban-hang-f10.html?page=", 69)
ban_hang_job <- get_information_for_group_job4(ban_hang_link$job_link[!duplicated(ban_hang_link$job_link)])


# Lao động phổ thông (450): 

lao_dong_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-lao-dong-pho-thong-f33.html?page=", 300)  
lao_dong_job <- get_information_for_group_job4(lao_dong_link$job_link[!duplicated(lao_dong_link$job_link)])


# Tài Chính - Kế Toán: 

tai_chinh_link <- get_all_link_for_job_group("https://viectotnhat.com/viec-lam-tai-chinh-ke-toan-kiem-toan-f47.html?page=", 100)
tai_chinh_job <- get_information_for_group_job4(tai_chinh_link$job_link[!duplicated(tai_chinh_link$job_link)])


# load("D:/job_anh_cuong/viec_lam.RData")

library(tidyverse)
library(magrittr)

all_job_data <- bind_rows(ban_hang_job %>% mutate(job_group = "viec-lam-ban-hang"), 
                          bao_hiem_job %>% mutate(job_group = "viec-lam-tu-van-bao-hiem"), 
                          bat_dong_san_job %>% mutate(job_group = "viec-lam-bat-dong-san"), 
                          cham_soc_job_16_04 %>% mutate(job_group = "viec-lam-cham-soc-khach-hang"), 
                          co_khi_job %>% mutate(job_group = "viec-lam-co-khi-ki-thuat-ung-dung"), 
                          cong_nghe_tt_job %>% mutate(job_group = "viec-lam-cong-nghe-thong-tin"), 
                          dien_job %>% mutate(job_group = "viec-lam-dien-dien-tu-dien-lanh"), 
                          giao_duc_job %>% mutate(job_group = "viec-lam-giao-duc-dao-tao-thu-vien"), 
                          hanh_chinh_job %>% mutate(job_group = "viec-lam-hanh-chinh-thu-ky-tro-ly"), 
                          khac_job %>% mutate(job_group = "viec-lam-khac"), 
                          kien_truc_job %>% mutate(job_group = "viec-lam-kien-truc-noi-that"), 
                          kinh_doanh_job2 %>% mutate(job_group = "viec-lam-kinh-doanh"), 
                          lao_dong_job %>% mutate(job_group = "viec-lam-lao-dong-pho-thong"), 
                          marketing_job %>% mutate(job_group = "viec-lam-quang-cao-marketing-pr"), 
                          nha_hang_job %>% mutate(job_group = "viec-lam-du-lich-nha-hang-khach-san"), 
                          nhan_su_job %>% mutate(job_group = "viec-lam-nhan-su"), 
                          sinh_vien_job %>% mutate(job_group = "viec-lam-sinh-vien-moi-tot-nghiep-thuc-tap"), 
                          tai_chinh_job %>% mutate(job_group = "viec-lam-tai-chinh-ke-toan-kiem-toan"), 
                          thiet_ke_job %>% mutate(job_group = "viec-lam-thiet-ke-my-thuat"), 
                          xay_dung_job %>% mutate(job_group = "viec-lam-xay-dung"))


# Lưu lại dữ liệu: 
write.csv(all_job_data, "D:/all_job_data.csv", row.names = TRUE)