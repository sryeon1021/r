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
filter(branch == "����" | branch == "����" | branch == "����") %>%
group_by(branch, product_name) %>%     ?     # �μ� �̸��� �޴� �̸����� �׷�ȭ
summarise(sales_amt = sum(sales) / 1000)     # ���� �ջ�
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
  labs(title = "�湮 Ƚ�� ���ڱ׸�", x = "��", y = "�湮Ƚ��")
ggplot(df_rfm_data, aes(x = "", y = sales_sum)) +
  geom_boxplot(width = 0.?, outlier.size = 2, outlier.colour = "red") +
  labs(title = "���� ���ڱ׸�", x = "����", y = "�ݾ�")
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
  filter((item_id %in% targ?t_item)) %>%       # ������ũ�� ������ �ֹ��� ��� ����
  group_by(reserv_no) %>%                      # ���� ��ȣ�� �׷�ȭ
  mutate(order_cnt = n()) %>%                  # �׷�ȭ�� �� ����
  distinct(branch, reserv_no, order_cnt) %>%   # �ߺ� ���� ��ȣ�� ???���� ���
  filter(order_cnt == 2) %>%                   # 2�� ��� ����(������ũ�� ������ ���� �ֹ��� ���)
  arrange(branch)
df_stime_order
stime_order_rsv_no <- df_stime_order$reserv_no
df_stime_sales <- df_f_join_2 %>%
  filter((reserv_no %in% stime_or?er_rsv_no) & (item_id %in% target_item)) %>%
  group_by(reserv_no, product_name) %>%          # ���� ��ȣ�� �޴� ���������� �׷�ȭ
  summarise(sales_amt = sum(sales) / 1000) %>%   # ���� �հ� ��� ���
  arrange(product_name, reserv_no)               # ��??? ������, ���� ��ȣ �������� ����
df_stime_sales
steak <- df_stime_sales %>% filter(product_name == "STEAK")   # ������ũ ������ ����
wine <- df_stime_sales %>% filter(product_name == "WINE")     # ���� ������ ����

df_rsv_customer <- reservation_r %>%
  se?ect(customer_id, reserv_no) %>%    # ������ ��� ���� ��ȣ ����
  arrange(customer_id, reserv_no)

head(df_rsv_customer)    
df_steak_order_rsv_no <- order_info_r %>%
filter(item_id == "M0005") %>%   # ������ũ �ֹ��̸�
mutate(steak_order = "Y") %>%    # s?eak_order �� �����͸� ��Y���� ����
arrange(reserv_no)
head(df_steak_order_rsv_no)  
df_table_join_3 <- df_table_join_2 %>%
  group_by(customer_id, sex_code, reserv_no, visitor_cnt) %>% # ��
  summarise(sales_sum = sum(sales)) %>%
  group_by(customer_id, se?_code) %>%                         # ��
  summarise(visit_sum = n_distinct(reserv_no), visitor_sum = sum(visitor_cnt), sales_sum = sum(sales_sum) / 1000) %>%     # ��
  arrange(customer_id)
df_customer <- customer_r %>% filter(!is.na(sex_code))
df_table_jo?n_1 <- inner_join(df_customer, reservation_r, by = "customer_id")
df_table_join_2 <- inner_join(df_table_join_1, order_info_r, by = "reserv_no")
str(df_table_join_2) 
df_rsv_customer <- reservation_r %>%
  select(customer_id, reserv_no) %>%    # ������ ��??? ���� ��ȣ ����
  arrange(customer_id, reserv_no)
df_steak_order_rsv_no <- order_info_r %>%
filter(item_id == "M0005") %>%   # ������ũ �ֹ��̸�
mutate(steak_order = "Y") %>%    # steak_order �� �����͸� ��Y���� ����
arrange(reserv_no)
df_steak_order_1 <- ?eft_join(df_rsv_customer, df_steak_order_rsv_no, by = "reserv_no") %>%
  group_by(customer_id) %>%                                       # ���� ��ȣ�� �׷�ȭ�Ͽ�(182��)
  mutate(steak_order = ifelse(is.na(steak_order), "N", "Y")) %>%  # �ֹ� ���ΰ� NA�̸� ?, Y�̸� Y�� 
df_steak_order_1 <- left_join(df_rsv_customer, df_steak_order_rsv_no, by = "reserv_no") %>%
  group_by(customer_id) %>%                                       # ���� ��ȣ�� �׷�ȭ�Ͽ�(182��)
  mutate(steak_order = ifelse(is.na(steak_order), "N"? "Y")) %>%  # �ֹ� ���ΰ� NA�̸� N, Y�̸� Y�� �ٲ�
  summarise(steak_order = max (steak_order)) %>%                  # �ִ񰪸� ����
  arrange(customer_id)
df_dpd_var <- df_steak_order_1
df_dpd_var
df_customer <- customer_r %>% filter(!is.na(sex_code))
df_?able_join_1 <- inner_join(df_customer, reservation_r, by = "customer_id")
df_table_join_2 <- inner_join(df_table_join_1, order_info_r, by = "reserv_no")
str(df_table_join_2)     # df_table_join_2 ���̺� ���� Ȯ��
df_table_join_3 <- df_table_join_2 %>%
  gr?up_by(customer_id, sex_code, reserv_no, visitor_cnt) %>% # ��
  summarise(sales_sum = sum(sales)) %>%
  group_by(customer_id, sex_code) %>%                         # ��
  summarise(visit_sum = n_distinct(reserv_no), visitor_sum = sum(visitor_cnt), sales_su? = sum(sales_sum) / 1000) %>%     # ��
  arrange(customer_id)
df_idp_var <- df_table_join_3   # ���� ����
df_idp_var                      # ���� ���� Ȯ��(142��)