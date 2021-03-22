install.packages('ggplot2')
library(ggplot2)
install.packages('dplyr')
library(dplyr)

my_first_cook <- order_info_r %>%
  mutate(reserv_month = substr(reserv_no, 1, 6)) %>%
  group_by(item_id, reserv_month) %>%
  summarise(avg_sales = mean(sales)) %>%
  a?range(item_id, reserv_month)

my_first_cook

ggplot(my_first_cook, aes(x=reserv_month, y=avg_sales,group=item_id,color=item_id))+
  geom_line(size=1)+
  geom_point(color='darkorange', size=1.5)+
  scale_color_brewer(palette= 'Paired')+
  labs(title='�޴� ???���ۺ��� ��� ���� ����', x='month',y='income')
weight <- c(74, 66, 61, 59, 70) #�л� A������ ������ 
mean(weight)#���
median(weight) #�߾Ӱ�
var(weight) #�л�
sd(weight) #ǥ������
colnames(customer_r)<- tolower(colnames(customer_r))
colnames(reservation_?)<-tolower(colnames(reservation_r))
colnames(order_info_r)<-tolower(colnames(order_info_r))
colnames(item_r)<- tolower(colnames(item_r))
table(reservation_r$branch)
no_cancel_data<- reservation_r%>%filter(cancel=='N')
table(no_cancel_data$branch)
df_f_join?1 <- inner_join(reservation_r, order_info_r, by = "reserv_no")
df_f_join_2 <- inner_join(df_f_join_1, item_r, by = "item_id")
head(df_f_join_2)
df_branch_sales <- df_f_join_2 %>%
filter(branch == "����" | branch == "����" | branch == "����") %>%
group_by(b?anch, product_name) %>%           # �μ� �̸��� �޴� �̸����� �׷�ȭ
summarise(sales_amt = sum(sales) / 1000)

ggplot(df_branch_sales, aes(x = "", y = sales_amt, fill = product_name)) +
  facet_grid(facets = . ~ branch) + # �� ���� �Լ� branch �������� ��???
  geom_bar(stat = "identity")