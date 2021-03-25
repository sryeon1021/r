colnames(customer_r) <- tolower(colnames(customer_r))
colnames(reservation_r) <- tolower(colnames(reservation_r))
colnames(order_info_r) <- tolower(colnames(order_info_r))
colnames(item_r) <- tolower(colnames(item_r))
install.packages('gglpot')
install.pac?ages('ggplot')
install.packages('ggplot2')
library('ggplot2')
install.packages('dplyr')
library(dplyr)
table(reservation_r$branch)
no_cancel_data <- reservation_r %>% filter(cancel == "N")
table(no_cancel_data$branch)
df_f_join_1 <- inner_join(reservation_?, order_info_r, by = "reserv_no")
df_f_join_2 <- inner_join(df_f_join_1, item_r, by = "item_id")
head(df_f_join_2)
df_branch_sales <- df_f_join_2 %>%
filter(branch == "강남" | branch == "마포" | branch == "서초") %>%
group_by(branch, product_name) %>%     ?     # 부서 이름과 메뉴 이름으로 그룹화
summarise(sales_amt = sum(sales) / 1000)     # 매출 합산
df_rfm_join_1 <- inner_join(reservation_r, order_info_r, by = "reserv_no")
head(df_rfm_join_1)
quantile(df_rfm_data$visit_sum, probs = c(0.6, 0.9))
quantile(df?rfm_data$sales_sum, probs = c(0.6, 0.9))
total_sum_data <- df_rfm_data %>%
  summarise(t_visit_sum = sum(visit_sum), t_sales_sum = sum(sales_sum))
loyalty_sum_data <- df_rfm_data %>%
  summarise(l_visit_sum = sum(ifelse(visit_sum > 2, visit_sum, 0)), l_sal?s_sum = sum(ifelse(sales_sum > 135, sales_sum, 0)))
df_rfm_data <- df_rfm_join_1 %>%
  group_by(customer_id) %>%
  summarise(visit_sum = n_distinct(reserv_no), sales_sum = sum(sales) / 1000) %>%
  arrange(customer_id)
df_rfm_data
summary(df_rfm_data)
ggplo?(df_rfm_data, aes(x = "", y = visit_sum)) +
  geom_boxplot(width = 0.8, outlier.size = 2, outlier.colour = "red") +
  labs(title = "방문 횟수 상자그림", x = "빈도", y = "방문횟수")
ggplot(df_rfm_data, aes(x = "", y = sales_sum)) +
  geom_boxplot(width = 0.?, outlier.size = 2, outlier.colour = "red") +
  labs(title = "매출 상자그림", x = "매출", y = "금액")
quantile(df_rfm_data$visit_sum, probs = c(0.6, 0.9))
quantile(df_rfm_data$sales_sum, probs = c(0.6, 0.9))
total_sum_data <- df_rfm_data %>%
  summarise(t_?isit_sum = sum(visit_sum), t_sales_sum = sum(sales_sum))
loyalty_sum_data <- df_rfm_data %>%
  summarise(l_visit_sum = sum(ifelse(visit_sum > 2, visit_sum, 0)), l_sales_sum = sum(ifelse(sales_sum > 135, sales_sum, 0)))
loyalty_sum_data / total_sum_data
l_v?sit_sum l_sales_sum
df_f_join_1 <- inner_join(reservation_r, order_info_r, by = "reserv_no")
df_f_join_2 <- inner_join(df_f_join_1, item_r, by = "item_id")
target_item <- c("M0005", "M0009")    
df_stime_order <- df_f_join_2 %>%
  filter((item_id %in% targ?t_item)) %>%       # 스테이크나 와인을 주문한 경우 선택
  group_by(reserv_no) %>%                      # 예약 번호로 그룹화
  mutate(order_cnt = n()) %>%                  # 그룹화된 행 세기
  distinct(branch, reserv_no, order_cnt) %>%   # 중복 예약 번호는 ???나만 출력
  filter(order_cnt == 2) %>%                   # 2인 경우 선택(스테이크와 와인을 동시 주문한 경우)
  arrange(branch)
df_stime_order
stime_order_rsv_no <- df_stime_order$reserv_no
df_stime_sales <- df_f_join_2 %>%
  filter((reserv_no %in% stime_or?er_rsv_no) & (item_id %in% target_item)) %>%
  group_by(reserv_no, product_name) %>%          # 예약 번호와 메뉴 아이템으로 그룹화
  summarise(sales_amt = sum(sales) / 1000) %>%   # 매출 합계 요약 계산
  arrange(product_name, reserv_no)               # 메??? 아이템, 예약 번호 기준으로 정렬
df_stime_sales
steak <- df_stime_sales %>% filter(product_name == "STEAK")   # 스테이크 정보만 담음
wine <- df_stime_sales %>% filter(product_name == "WINE")     # 와인 정보만 담음

df_rsv_customer <- reservation_r %>%
  se?ect(customer_id, reserv_no) %>%    # 고객별 모든 예약 번호 선택
  arrange(customer_id, reserv_no)

head(df_rsv_customer)    
df_steak_order_rsv_no <- order_info_r %>%
filter(item_id == "M0005") %>%   # 스테이크 주문이면
mutate(steak_order = "Y") %>%    # s?eak_order 열 데이터를 ‘Y’로 만듦
arrange(reserv_no)
head(df_steak_order_rsv_no)  
df_table_join_3 <- df_table_join_2 %>%
  group_by(customer_id, sex_code, reserv_no, visitor_cnt) %>% # ⓐ
  summarise(sales_sum = sum(sales)) %>%
  group_by(customer_id, se?_code) %>%                         # ⓑ
  summarise(visit_sum = n_distinct(reserv_no), visitor_sum = sum(visitor_cnt), sales_sum = sum(sales_sum) / 1000) %>%     # ⓒ
  arrange(customer_id)
df_customer <- customer_r %>% filter(!is.na(sex_code))
df_table_jo?n_1 <- inner_join(df_customer, reservation_r, by = "customer_id")
df_table_join_2 <- inner_join(df_table_join_1, order_info_r, by = "reserv_no")
str(df_table_join_2) 
df_rsv_customer <- reservation_r %>%
  select(customer_id, reserv_no) %>%    # 고객별 모??? 예약 번호 선택
  arrange(customer_id, reserv_no)
df_steak_order_rsv_no <- order_info_r %>%
filter(item_id == "M0005") %>%   # 스테이크 주문이면
mutate(steak_order = "Y") %>%    # steak_order 열 데이터를 ‘Y’로 만듦
arrange(reserv_no)
df_steak_order_1 <- ?eft_join(df_rsv_customer, df_steak_order_rsv_no, by = "reserv_no") %>%
  group_by(customer_id) %>%                                       # 고객 번호로 그룹화하여(182명)
  mutate(steak_order = ifelse(is.na(steak_order), "N", "Y")) %>%  # 주문 여부가 NA이면 ?, Y이면 Y로 
df_steak_order_1 <- left_join(df_rsv_customer, df_steak_order_rsv_no, by = "reserv_no") %>%
  group_by(customer_id) %>%                                       # 고객 번호로 그룹화하여(182명)
  mutate(steak_order = ifelse(is.na(steak_order), "N"? "Y")) %>%  # 주문 여부가 NA이면 N, Y이면 Y로 바꿈
  summarise(steak_order = max (steak_order)) %>%                  # 최댓값만 취함
  arrange(customer_id)
df_dpd_var <- df_steak_order_1
df_dpd_var
df_customer <- customer_r %>% filter(!is.na(sex_code))
df_?able_join_1 <- inner_join(df_customer, reservation_r, by = "customer_id")
df_table_join_2 <- inner_join(df_table_join_1, order_info_r, by = "reserv_no")
str(df_table_join_2)     # df_table_join_2 테이블 구조 확인
df_table_join_3 <- df_table_join_2 %>%
  gr?up_by(customer_id, sex_code, reserv_no, visitor_cnt) %>% # ⓐ
  summarise(sales_sum = sum(sales)) %>%
  group_by(customer_id, sex_code) %>%                         # ⓑ
  summarise(visit_sum = n_distinct(reserv_no), visitor_sum = sum(visitor_cnt), sales_su? = sum(sales_sum) / 1000) %>%     # ⓒ
  arrange(customer_id)
df_idp_var <- df_table_join_3   # 독립 변수
df_idp_var                      # 독립 변수 확인(142행)
