# data input 
library(readr)
library(readxl)

# data mgmt
library(dplyr)
library(stringr)
library(tidyr)
library(zipcodeR)
library(reshape2)
library(data.table)
library(dtplyr)

# data visuals
library(lfe)
library(ggplot2)
library(scales)
library(stargazer)

setwd("raw_data")

# get the name of all files in the folder, and then the ones matching my raw data filenames
all_files=list.files()
monthly_cs_files=grep("^\\w+_q[1,2]_cs.xlsx",all_files,value=T)
monthly_basic_files=grep("^\\w+_q[1,2]_basic.xlsx",all_files,value=T)
monthly_agg_files=grep("^\\w+_q[1,2]_agg.xlsx",all_files,value=T)

# get basic rates from excel
basic_rates=read_excel("Basic_Rates_TH.xlsx")

# read each monthly CS excel into one big file
monthly_cs=lapply(monthly_cs_files,function(x){
  return(read_excel(x,col_types = c("date","text","numeric","numeric","numeric","numeric","numeric","text","text")))
})


## exclude datapoint from June 2019, which is outside of data request
monthly_cs=bind_rows(monthly_cs) %>% 
  filter(date > '2019-06-01') %>%
  group_by(date,supplier,rate,region,income) %>%
  summarize(kwh=sum(kwh),
            amt_billed=sum(amt_billed),
            no_accts=sum(no_accts),
            new_accts=sum(new_accts)) 

FIT_low_avg_kwh_Sept19<-monthly_cs %>% filter(region == "FIT") %>%
  filter(as.Date(date) == "2019-09-01") %>%
  filter(income == "low") %>%
  group_by() %>%
  summarize(avg_kwh_per_acct = sum(kwh)/sum(no_accts))
FIT_low_avg_kwh_Sept19

FIT_all_avg_kwh_Sept19<-monthly_cs %>% filter(region == "FIT") %>%
  filter(as.Date(date) == "2019-09-01") %>%
  filter(income == "all") %>%
  group_by() %>%
  summarize(avg_kwh_per_acct = sum(kwh)/sum(no_accts))
FIT_all_avg_kwh_Sept19

# change no_accts from 0 to 1 where it is currently 0 but kwh>0
monthly_cs[which(monthly_cs$no_accts ==0 & monthly_cs$kwh >0),]$no_accts <- 1

# remove records where kwh = 0 and number of accts = 0
monthly_cs <- monthly_cs %>% filter(kwh>0 & no_accts > 0)

monthly_cs_welfare=left_join(monthly_cs,basic_rates, by = c("date", "region")) %>%
  mutate(basic_bill=kwh*basic_rate,  
         bill_difference=amt_billed-basic_bill,  
         bill_difference_pp=bill_difference/no_accts,  
         rate_difference=rate-basic_rate,
         kwh_per_acct=ifelse(kwh==0|no_accts==0,0,kwh/no_accts))

# check for NAs
summary(monthly_cs_welfare)


# read each basic customer excel into one big file
monthly_basic=lapply(monthly_basic_files,function(x){
  return(read_excel(x,col_types = c("date","numeric","numeric","numeric","text","text")))
})
monthly_basic=bind_rows(monthly_basic)

colnames(monthly_basic)=c("date","basic_kwh","basic_billed","basic_accts","region","income")

# check for NAs
summary(monthly_basic)


# read each agg excel into one big file
# requires date, region, total_billed, kwh, number_accts, region, income
monthly_agg=lapply(monthly_agg_files,function(x){
  return(read_excel(x,col_types = c("date","text","numeric","numeric","numeric","text","text")))
})
monthly_agg=bind_rows(monthly_agg) %>% 
  group_by(date,region,income) %>% 
  summarize(kwh=sum(kwh),
            amt_billed=sum(amt_billed),
            no_accts=sum(no_accts)) 

colnames(monthly_agg)=c("date","region","income","agg_kwh","agg_billed","agg_accts")

# check for NAs
summary(monthly_agg)



## lineplot of CS rates over time, and basic tiers  ----
yearly_summary=monthly_cs_welfare %>% 
  group_by(date,income) %>%
  summarize(avg_rate=sum(kwh*rate)/sum(kwh),
            avg_basic_rate=sum(kwh*basic_rate)/sum(kwh),
            kwh=sum(kwh),
            amt_billed=sum(amt_billed)) 

yearly_summary_o=yearly_summary %>% 
  group_by(date) %>% 
  summarize(kwh=max(kwh)-min(kwh),
            amt_billed=max(amt_billed)-min(amt_billed),
            income="other") %>% 
  mutate(avg_rate=amt_billed/kwh)

yearly_summary=bind_rows(yearly_summary,yearly_summary_o) %>% 
  ungroup() %>% 
  mutate(rate=ifelse(income=="all",avg_basic_rate,avg_rate),
         income=factor(income,levels=c("low","other","all"))) %>%
  ungroup()

#############################

plot_dir = "../plots/"

p1<-ggplot(yearly_summary,aes(x=date,y=rate,color=factor(income))) +
  geom_step(size=1) +
  scale_y_continuous(labels=dollar,limits=c(0,.16)) +
  scale_fill_discrete(NULL) +
  scale_color_discrete(name=NULL,
                       labels=c("\nCompetitive: \nLow income \n",
                                "\nCompetitive: \nNon-low \nincome \n",
                                "\nBasic \nservice \n")) +
  labs(x=NULL,y="State-wide average rate (dollars per kWh)")
file_name = paste(plot_dir,"plot1_Statewide_average_rate.png", sep = "")
png(file_name, width = 500, height = 500)
print(p1)
dev.off()
p1


p2<- ggplot(yearly_summary,aes(x=date,y=rate,color=factor(income))) +
  geom_step(size=1) +
  scale_y_continuous(labels=dollar,limits=c(0.075,.16)) +
  scale_fill_discrete(NULL) +
  scale_color_discrete(name=NULL,
                       labels=c("\nCompetitive: \nLow income \n",
                                "\nCompetitive: \nNon-low \nincome \n",
                                "\nBasic \nservice \n")) +
  labs(x=NULL,y="State-wide average rate (dollars per kWh)")
file_name = paste(plot_dir,"plot2_Statewide_average_rate.png", sep = "")
png(file_name, width = 500, height = 500)
print(p2)
dev.off()
p2


# Exclude Nantucket from rate plot
rate_plot <- basic_rates %>% filter(region!="NAN")

p3 <- ggplot(rate_plot,aes(x=date,y=basic_rate,color=factor(region))) +
  geom_step(size=1) +
  scale_y_continuous(labels=dollar,breaks=c(.08,.09,.1)) +
  scale_fill_discrete(NULL) +
  scale_color_discrete(name=NULL) +
  labs(x=NULL,y="Basic rate") 
file_name = paste(plot_dir,"plot3_Statewide_average_rate.png", sep = "")
png(file_name, width = 500, height = 500)
print(p3)
dev.off()
p3

# overpaid and underpaid on CS ----
monthly_cs_overpaid=filter(monthly_cs_welfare,bill_difference>0) %>%  
  filter(rate<1) %>%
  group_by_(.dots=c("region","income")) %>%
  summarize(bill_difference=sum(bill_difference),
            no_accts=sum(no_accts),
            kwh=sum(kwh)) %>%
  mutate(avg_overpaid_per_month=bill_difference/no_accts,
         avg_overpaid_per_kwh=bill_difference/kwh,
         avg_overpaid_per_year=avg_overpaid_per_month*12)
write_csv(monthly_cs_overpaid,"../output/0111_monthly_cs_overpaid.csv")

monthly_cs_underpaid=filter(monthly_cs_welfare,bill_difference<=0) %>%  
  group_by_(.dots=c("region","income")) %>%
  summarize(bill_difference=sum(bill_difference),
            no_accts=sum(no_accts),
            kwh=sum(kwh)) %>%
  mutate(avg_saved_per_month=bill_difference/no_accts,
         avg_saved_per_kwh=bill_difference/kwh,
         avg_saved_per_year=avg_saved_per_month*12)
write_csv(monthly_cs_underpaid,"../output/0111_monthly_cs_underpaid.csv")

# overpaid and underpaid on CS ANNUAL ---- not separated by region
monthly_cs_overpaid=filter(monthly_cs_welfare,bill_difference>0) %>%  
  filter(rate<1) %>%
  group_by(income) %>%
  summarize(tot_bill_difference_over=sum(bill_difference),
            no_accts_over=sum(no_accts),
            kwh_over=sum(kwh)) %>%
  mutate(avg_monthly_kwh_per_hh_over=kwh_over/no_accts_over,
         avg_overpaid_per_month=tot_bill_difference_over/no_accts_over,
         avg_overpaid_per_kwh=tot_bill_difference_over/kwh_over,
         avg_overpaid_per_year=avg_overpaid_per_month*12)

monthly_cs_underpaid=filter(monthly_cs_welfare,bill_difference<=0) %>%  
  group_by(income) %>%
  summarize(tot_bill_difference_under=sum(bill_difference),
            no_accts_under=sum(no_accts),
            kwh_under=sum(kwh)) %>%
  mutate(avg_monthly_kwh_per_hh_under=kwh_under/no_accts_under,
         avg_saved_per_month=tot_bill_difference_under/no_accts_under,
         avg_saved_per_kwh=tot_bill_difference_under/kwh_under,
         avg_saved_per_year=avg_saved_per_month*12)

annual_over_under=left_join(monthly_cs_overpaid,monthly_cs_underpaid) %>% 
  mutate(pct_accounts_saving=no_accts_under/(no_accts_under+no_accts_over))
write_csv(annual_over_under,"../output/0111_annual_over_under.csv")

nrow(filter(monthly_cs_welfare,bill_difference==0))



# conservative estimation of OTHER

net=monthly_cs_welfare %>% 
  group_by(income) %>%
  summarize(amt_billed=sum(amt_billed),
            tot_bill_difference=sum(bill_difference),
            no_accts=sum(no_accts),
            kwh=sum(kwh)) %>%
  mutate(avg_monthly_kwh_per_hh=kwh/no_accts,
         avg_loss_per_month=tot_bill_difference/no_accts,
         avg_loss_per_kwh=tot_bill_difference/kwh,
         avg_loss_per_year=avg_loss_per_month*12,
         tag="net")

over=filter(monthly_cs_welfare,bill_difference>0) %>%  
  group_by(income) %>%
  summarize(amt_billed=sum(amt_billed),
            tot_bill_difference=sum(bill_difference),
            no_accts=sum(no_accts),
            kwh=sum(kwh)) %>%
  mutate(avg_monthly_kwh_per_hh=kwh/no_accts,
         avg_loss_per_month=tot_bill_difference/no_accts,
         avg_loss_per_kwh=tot_bill_difference/kwh,
         avg_loss_per_year=avg_loss_per_month*12,
         tag="over")

under=filter(monthly_cs_welfare,bill_difference<0) %>%  
  group_by(income) %>%
  summarize(amt_billed=sum(amt_billed),
            tot_bill_difference=sum(bill_difference),
            no_accts=sum(no_accts),
            kwh=sum(kwh)) %>%
  mutate(avg_monthly_kwh_per_hh=kwh/no_accts,
         avg_loss_per_month=tot_bill_difference/no_accts,
         avg_loss_per_kwh=tot_bill_difference/kwh,
         avg_loss_per_year=avg_loss_per_month*12,
         tag="under")

write_csv(bind_rows(net,under,over),"../output/0111_annual_summary.csv")

# estimate participation relative to basic, agg
monthly_basic <- monthly_basic[,c(1,2,3,4,5,6)]
cs_vs_basic_agg=monthly_cs_welfare %>% 
  group_by(region,income,date) %>%
  summarize(amt_billed=sum(amt_billed),
            tot_bill_difference=sum(bill_difference),
            no_accts=sum(no_accts),
            kwh=sum(kwh)) %>%
  left_join(monthly_basic) %>% 
  left_join(monthly_agg) %>% 
  group_by(income) %>% 
  summarize(amt_billed=sum(amt_billed),
            no_accts=sum(no_accts),
            kwh=sum(kwh),
            tot_bill_difference=sum(tot_bill_difference),
            basic_billed=sum(basic_billed),
            basic_accts=sum(basic_accts),
            basic_kwh=sum(basic_kwh),
            agg_billed=sum(agg_billed),
            agg_accts=sum(agg_accts),
            agg_kwh=sum(agg_kwh)) %>% 
  mutate(pct_cs_accts=no_accts/(no_accts+basic_accts+agg_accts),
         tot_accts=no_accts+basic_accts+agg_accts,
         pct_cs_billed=amt_billed/(amt_billed+basic_billed+agg_billed),
         pct_cs_kwh=kwh/sum(kwh+basic_kwh+agg_kwh),
         tot_kwh=kwh+basic_kwh+agg_kwh)

write_csv(cs_vs_basic_agg,"../output/0111_cs_vs_basic_agg.csv")



# summarize the supplier-level CS data by months, and years (compared to basic customers) ----
monthly_cs_vs_basic=monthly_cs_welfare %>%
  group_by_(.dots=c("region","income","date")) %>%
  summarize(cs_accts=sum(no_accts),
            cs_kwh=sum(kwh),
            cs_billed=sum(amt_billed)) %>%
  left_join(monthly_basic, by = c("region", "income", "date")) %>%
  mutate(pct_cs_accts=cs_accts/(cs_accts+basic_accts))
write_csv(monthly_cs_vs_basic,"../output/0111_monthly_cs_vs_basic.csv")

annual_cs_vs_basic=monthly_cs_vs_basic %>%
  group_by_(.dots=c("region","income")) %>%
  summarize(cs_accts=sum(cs_accts),
            cs_kwh=sum(cs_kwh),
            cs_billed=sum(cs_billed),
            basic_accts=sum(basic_accts),
            basic_kwh=sum(basic_kwh),
            basic_billed=sum(basic_billed),
            kwh_dif=(cs_kwh/cs_accts)-(basic_kwh/basic_accts)) %>%
  mutate(pct_cs_accts=cs_accts/(cs_accts+basic_accts))
write_csv(annual_cs_vs_basic,"../output/0111_annual_cs_vs_basic.csv")

summary(annual_cs_vs_basic)

# supplier summary ----
supplier_key=monthly_cs_welfare %>%
  ungroup() %>% 
  mutate(supplier_id=substr(supplier,1,5)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  select(supplier,supplier_id) %>%
  distinct()
write_csv(supplier_key,"../output/0111_supplier_key.csv")



suppliers=monthly_cs_welfare %>%
  mutate(supplier_id=substr(supplier,1,5)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  group_by(supplier_id,income) %>%
  summarize(no_months=length(unique(date)),
            avg_rate_by_accts=sum(rate*no_accts)/sum(no_accts),
            avg_basic_by_accts=sum(basic_rate*no_accts)/sum(no_accts),
            avg_rate_by_kwh=sum(rate*kwh)/sum(kwh),
            avg_basic_by_kwh=sum(basic_rate*kwh)/sum(kwh),
            no_accts=sum(no_accts),
            kwh=sum(kwh)) %>%
  ungroup() %>%
  filter(no_months==12) %>%
  group_by(income) %>%
  mutate(avg_premium_by_accts=avg_rate_by_accts-avg_basic_by_accts,
         avg_premium_by_kwh=avg_rate_by_kwh-avg_basic_by_kwh,
         share_of_accts=no_accts/sum(no_accts),
         share_of_kwh=kwh/sum(kwh),
         dollar_loss=avg_premium_by_kwh*kwh,
         share_of_loss=dollar_loss/sum(dollar_loss),
         loss_to_kwh=share_of_loss/share_of_kwh)
write_csv(suppliers,"../output/0111_supplier_summary.csv")


#############################
# rates by top three suppliers
# Previously the top three supplier were hard-coded as VIRID, CON E, and CONSO.
# This was based on total number of accounts by supplier.
# Now the top three are different.

supplier_totals <- suppliers %>% group_by(supplier_id) %>% summarise(total_accts = sum(no_accts))
top_suppliers_ordered <- supplier_totals %>% arrange(desc(total_accts))
top_three_suppliers <- top_suppliers_ordered[1:3,]
top_three_suppliers
                                     
# Now they are NRG R, DIRECT, & CONST.
# VIRID dropped well down, with only 242907 accounts.

top_suppliers=monthly_cs_welfare %>%
  mutate(supplier_id=substr(supplier,1,5)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  filter(supplier_id %in% c("NRG R","DIREC","CONST"),
         kwh>0,
         rate>0,
         income=="all") %>% 
  mutate(markup_bin=cut(rate_difference,c(-Inf,seq(-.02,.09,.01),Inf))) %>% 
  group_by(markup_bin,supplier_id) %>%
  summarize(no_accts=sum(no_accts)) 

all_levels=expand.grid(supplier_id=unique(top_suppliers$supplier_id),
                       markup_bin=unique(top_suppliers$markup_bin))
top_suppliers=left_join(all_levels,top_suppliers)

p4<-ggplot(filter(top_suppliers),
           aes(x=markup_bin,y=no_accts,fill=supplier_id))+
  geom_bar(stat="identity",position="dodge")+
  scale_x_discrete(labels=c("< -2","(-2 - 1]","(-1 - 0]","(0 - 1]","(1 - 2]","(2 - 3]",
                            "(3 - 4]","(4 - 5]","(5 - 6]","(6 - 7]","(7 - 8]","(8 - 9]","> 9"))+
  scale_fill_discrete(name=NULL,
                       labels=c("Supplier 1","Supplier 2","Supplier 3"))+
  labs(x="Competitive markup in cents ($0.01)",
       y="Number of accounts")+
  theme(legend.position="bottom")
file_name = paste(plot_dir,"plot4_competitive_markup.png", sep = "")
png(file_name, width = 500, height = 500)
print(p4)
dev.off()
p4


# visuals: summary bar charts ----
# TH removed na.rms 1/11

yearly_summary=monthly_cs_welfare %>% 
  group_by(income,date,region) %>% 
  summarize(rate=sum(rate*kwh)/sum(kwh),
            kwh=sum(kwh),
            no_accts=sum(no_accts),
            basic_rate=mean(basic_rate)) %>% 
  left_join(monthly_basic) %>% 
  left_join(monthly_agg) %>% 
  group_by(income) %>% 
  summarize(cs_kwh=sum(kwh),
            cs_rate=sum(rate*kwh)/sum(kwh),
            cs_accts=sum(no_accts),
            cs_basic_rate=sum(basic_rate*kwh)/sum(kwh),
            basic_accts=sum(basic_accts),
            basic_kwh=sum(basic_kwh),
            agg_accts=sum(agg_accts),
            agg_kwh=sum(agg_kwh)) %>% 
  mutate(cs_participation=cs_accts/(cs_accts+basic_accts+agg_accts),
         cs_premium=cs_rate-cs_basic_rate,
         cs_loss=cs_premium*cs_kwh/cs_accts) %>% 
  select(income,cs_participation,cs_premium,cs_loss) %>% 
  gather(variable,value,2:4)

summary(yearly_summary)


# This is a summary of statistics for competitive supplier market
p5<- ggplot(filter(yearly_summary,income!="all"),
            aes(x=variable,y=value,fill=income))+
  geom_bar(stat="identity",position="dodge")
file_name = paste(plot_dir,"plot5_yearly_summary.png", sep = "")
png(file_name, width = 500, height = 500)
print(p5)
dev.off()
p5

# a table may be more appropriate...
write.table(yearly_summary, "../output/0111_cs_yearly_summary.csv", row.names = F, sep = "," )

##################
# summary of number of accts and bill differences
yearly_summary=monthly_cs_welfare %>% 
  group_by(income,date,region) %>% 
  summarize(no_accts=sum(no_accts,na.rm=T),
            bill_difference=sum(bill_difference,na.rm=T)) %>% 
  left_join(monthly_basic) %>% 
  left_join(monthly_agg) %>% 
  group_by(income) %>% 
  summarize(total_accts=sum(basic_accts,na.rm=T)+sum(agg_accts,na.rm=T)+sum(no_accts,na.rm=T),
            basic_accts=sum(basic_accts,na.rm=T),
            cs_accts=sum(no_accts,na.rm=T),
            bill_difference=sum(bill_difference,na.rm=T)) %>% 
  mutate(income=factor(income,levels=c("other","low","all"))) %>% 
  gather(variable,value,2:5)

summary(yearly_summary)

# share of accts by income type and total consumer loss
p6<- ggplot(filter(yearly_summary),
            aes(x=variable,y=value,fill=income))+
  geom_col()+
  scale_x_discrete(limits=c("total_accts","basic_accts","cs_accts","bill_difference"),
                   labels=c("All accounts","Basic accounts","Competitive supply accounts","Counsumer loss"),
                   name=NULL)+
  #scale_y_continuous(name="Share by income group", labels=percent)+
  scale_fill_discrete(name=NULL, labels=c("Non-low-income accounts","Low-income accounts"))+
  theme(legend.position = "bottom")
file_name = paste(plot_dir,"plot6_share_by_income_group.png", sep = "")
png(file_name, width = 500, height = 500)
print(p6)
dev.off()
p6
# above as a table
write.table(yearly_summary, "../output/0111_accts_by_income_type.csv", row.names = F, sep = "," )


# CS premia ----
rate_differences=monthly_cs_welfare %>%
  mutate(rounded_dif=round(rate_difference/.01)*.01) %>%
  group_by(rounded_dif,income) %>%
  summarize(kwh=sum(kwh)) %>%   
  mutate(income=factor(income,levels=c("other","low","all")))

summary(rate_differences)

p7<- ggplot(filter(rate_differences,income %in% c("low")),
            aes(x=rounded_dif,y=kwh/1e8,fill=income))+
  geom_bar(stat="identity",position="stack")+
  geom_vline(xintercept=0,color="grey")+
  scale_x_continuous(labels=dollar,
                     limits=c(-.15,.15),
                     breaks=c(-.15,-.10,-.05,0,.05,.1,.15))+
  scale_fill_discrete(name=NULL,
                      labels=c("Low-income accounts"))+
  labs(x="Rate difference (competitive rate minus corresponding basic rate)",
       y="kWh Purchased (100,000,000s)")+
  theme(legend.position=c(.8,.8))
file_name = paste(plot_dir,"plot7_rate_difference.png", sep = "")
png(file_name, width = 500, height = 500)
print(p7)
dev.off()
p7


# CS premia 2 ----
rate_differences=monthly_cs_welfare %>%
  mutate(rate_dif=cut(rate_difference,c(-Inf,seq(-.04,.15,.01),Inf),right=F)) %>%
  group_by(rate_dif,income) %>%
  summarize(kwh=sum(kwh)) %>%   
  mutate(income=factor(income,levels=c("other","low","all")),
         welfare=ifelse(substr(rate_dif,2,2)=="-","gain","loss"))

p8 <- ggplot(filter(rate_differences,income=="all"),
             aes(x=rate_dif,y=kwh/1e8,fill=welfare))+
  geom_bar(stat="identity",position="stack")+
  scale_x_discrete(labels=c("< $0.04",
                            "$0 - $0.01",
                            "$0.05 - $0.06",
                            "$0.10 - $0.11",
                            "> $0.15"),
                   breaks=levels(rate_differences$rate_dif)[c(T,F,F,F,F)])+
  scale_fill_manual(name="Consumer welfare",
                    labels=c("Gain (underpaid)","Loss (overpaid)"),
                    values=c("springgreen3","tomato"))+
  labs(x="Rate difference (competitive rate minus corresponding basic rate)",
       y="kWh purchased (100,000,000s)")+
  theme(legend.position=c(.85,.8))
file_name = paste(plot_dir,"plot8_rate_difference.png", sep = "")
png(file_name, width = 500, height = 500)
print(p8)
dev.off()
p8


# bill differences 2 ----
bill_differences=monthly_cs_welfare %>% 
  mutate(bill_dif=cut(bill_difference_pp,c(-Inf,seq(-40,100,10),Inf),right=F)) %>%
  group_by(bill_dif,income) %>%
  summarize(no_accts=sum(no_accts)) %>% 
  filter(!is.na(bill_dif)) %>% 
  mutate(income=factor(income,levels=c("other","low","all")),
         welfare=ifelse(substr(bill_dif,2,2)=="-","gain","loss"))


p9 <- ggplot(filter(bill_differences,income=="all"),
             aes(x=bill_dif,y=no_accts/1e5,fill=welfare))+
  geom_bar(stat="identity",position="stack")+
  scale_x_discrete(labels=c("< $40",
                            "$0 - $10",
                            "$50 - $60",
                            "> $100"),
                   #"$100 - $110",
                   #"> $150"),
                   breaks=levels(bill_differences$bill_dif)[c(T,F,F,F,F)])+
  scale_fill_manual(name="Consumer welfare",
                    labels=c("Gain (underpaid)","Loss (overpaid)"),
                    values=c("springgreen3","tomato"))+
  labs(x="Monthly household loss (competitive bill minus corresponding basic bill)",
       y="Number of accounts (100,000s)")+
  theme(legend.position=c(.85,.8))
file_name = paste(plot_dir,"plot9_monthly_consumer_welfare.png", sep = "")
png(file_name, width = 500, height = 500)
print(p9)
dev.off()
p9


#for table of premia
rate_differences_table=monthly_cs_welfare %>%
  filter(kwh!=0,
         income!="other") %>% 
  mutate(premium_bin=cut(rate_difference,c(-Inf,seq(-.1,.25,.025),.5,.75,Inf))) %>%
  group_by(premium_bin,income) %>%
  summarize(no_accts=sum(no_accts,na.rm=T),
            kwh=sum(kwh,na.rm=T)) %>%   
  recast(premium_bin~income+variable) %>% 
  mutate(pct_low_accts=low_no_accts/all_no_accts,
         pct_low_kwh=low_kwh/all_kwh)

write_csv(rate_differences_table,"../output/0111_cs_markup_bins.csv")


#####
# for fill plot of premia
rate_differences_plot=monthly_cs_welfare %>%
  filter(kwh!=0) %>% 
  mutate(premium_bin=cut(rate_difference,c(seq(-.02,.08,.01)))) %>%
  filter(!is.na(premium_bin)) %>% 
  group_by(premium_bin,income) %>%
  summarize(no_accts=sum(no_accts,na.rm=T),
            kwh=sum(kwh,na.rm=T)) %>%   
  mutate(income=factor(income,levels=c("other","low","all")))

# frequency chart of degree of markup for low-income group
p10 <- ggplot(filter(rate_differences_plot,income %in% c("low")),
              aes(x=premium_bin,y=no_accts,fill=income))+
  geom_bar(stat="identity",position="dodge")+
  geom_vline(xintercept=0,color="grey")+
  scale_x_discrete(labels=c("(-2 - 1]","(-1 - 0]","(0 - 1]","(1 - 2]","(2 - 3]",
                            "(3 - 4]","(4 - 5]","(5 - 6]","(6 - 7]","(7 - 8]"))+
  scale_fill_discrete(name=NULL,
                      labels=c("Low-income accounts"))+
  labs(x="Competitive markup in cents ($0.01)",
       y="Share of accounts by income group")+
  theme(legend.position=c(.8,.8))
file_name = paste(plot_dir,"plot10_competitive_markup.png", sep = "")
png(file_name, width = 500, height = 500)
print(p10)
dev.off()
p10

# summarize(filter(monthly_cs_welfare,income=="all"),sum(bill_difference_pp*kwh,na.rm=T)/sum(kwh,na.rm=T))
# summarize(filter(monthly_cs_welfare,income=="all"),sum(rate_difference*kwh,na.rm=T)/sum(kwh,na.rm=T))

bill_differences=monthly_cs_welfare %>% 
  mutate(rounded_dif=round(bill_difference_pp/10)*10) %>%
  group_by(rounded_dif,income) %>%
  summarize(no_accts=sum(no_accts)) %>% 
  filter(!is.na(rounded_dif)&!is.infinite(rounded_dif)) %>% 
  mutate(income=factor(income,levels=c("other","low","all")))

p11 <- ggplot(filter(bill_differences),
              aes(x=rounded_dif,y=no_accts/1e5,fill=income))+
  geom_bar(stat="identity",position="dodge")+
  geom_vline(xintercept=0,color="grey")+
  scale_x_continuous(labels=dollar,
                     limits=c(-45,125),
                     breaks=c(-40,-20,0,20,40,60,80,100,120))+
  scale_fill_discrete(name=NULL,
                      labels=c("Low-income accounts","All accounts"))+
  labs(x="Monthly household loss (competitive bill minus corresponding basic bill)",
       y="Number of accounts (100,000s)")+
  theme(legend.position=c(.7,.8))
file_name = paste(plot_dir,"plot11_monthly_household_loss.png", sep = "")
png(file_name, width = 500, height = 500)
print(p11)
dev.off()
p11

################################
cs_by_month_region_income=monthly_cs_welfare %>%
  group_by_(.dots=c("date","region","income")) %>%
  summarize(avg_household_bill_difference=sum(bill_difference_pp*no_accts)/sum(no_accts),
            total_household_bill_difference=sum(bill_difference),
            no_accts=sum(no_accts),
            new_accts=sum(new_accts)) 
write_csv(cs_by_month_region_income,"../output/0111_cs_by_month_region_income.csv")

summary(cs_by_month_region_income)


#  check Direct Energy Services in WMA - is there a basic rate?
supplier_rates_by_region = monthly_cs %>%
  left_join(basic_rates) %>%
  group_by_(.dots=c("supplier","region","income")) %>%
  summarize(no_accts=sum(no_accts)/n(),
            rate=sum(amt_billed)/(sum(kwh)),
            avg_basic=sum(basic_rate*kwh)/sum(kwh),
            rate_difference=rate-avg_basic)
write_csv(supplier_rates_by_region,"../output/0111_supplier_rates_by_region.csv")

rate_averages=basic_rates %>%
  mutate(rate=basic_rate,
         income="basic",
         no_accts=1,
         kwh=1) %>%
  filter(region!="Fitchburg") %>%
  bind_rows(monthly_cs) %>%
  group_by_(.dots=c("date","region","income")) %>%
  summarize(avg_rate1=sum(no_accts*rate)/sum(no_accts),
            avg_rate2=sum(kwh*rate)/sum(kwh)) %>%
  ungroup() %>%
  mutate(region_income=interaction(region,income)) 


# ---- NSTAR + NRG summary (march 2018)

supplier_key=monthly_cs_welfare %>%
  ungroup() %>% 
  mutate(supplier_id=substr(supplier,1,5)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  select(supplier,supplier_id) %>%
  distinct()

supplier_by_month=monthly_cs_welfare %>% 
  left_join(supplier_key) %>% 
  group_by(region,supplier_id,date,income) %>% 
  summarize(no_accts=sum(no_accts,na.rm=T),
            kwh=sum(kwh,na.rm=T),
            amt_billed=sum(amt_billed),
            basic_rate=mean(basic_rate),
            min_rate=min(rate,na.rm=T),
            max_rate=max(rate,na.rm=T),
            rate_q01=quantile(rate, probs=0.01),
            rate_q25=quantile(rate, probs=0.25),
            rate_q50=quantile(rate, probs=0.5),
            rate_q75=quantile(rate, probs=0.75),
            rate_q99=quantile(rate, probs=0.99))%>% 
  mutate(avg_rate=amt_billed/kwh,
         kwh_per_acct=kwh/no_accts,
         bill_per_acct=amt_billed/no_accts,
         basic_bill=kwh*basic_rate,
         basic_bill_per_acct=kwh_per_acct*basic_rate,
         bill_diff_per_acct=bill_per_acct-basic_bill_per_acct,
         avg_premium=avg_rate-basic_rate) %>% 
  left_join(supplier_key) 
supplier_by_month=supplier_by_month[!duplicated(supplier_by_month[1:4]),]

# This is never used?
price_quants=monthly_cs_welfare %>% 
  left_join(supplier_key) %>% 
  group_by(region,supplier_id,rate,income) %>% 
  summarize(no_accts=sum(no_accts,na.rm=T),
            kwh=sum(kwh,na.rm=T),
            amt_billed=sum(amt_billed),
            basic_rate=mean(basic_rate)) 


supplier_by_year=supplier_by_month %>% 
  group_by(region,supplier,supplier_id,income) %>% 
  summarize(no_accts=sum(no_accts,na.rm=T),
            kwh=sum(kwh,na.rm=T),
            amt_billed=sum(amt_billed),
            basic_bill=sum(basic_bill),
            n_months_in_data=n()) %>% 
  mutate(avg_rate=amt_billed/kwh,
         avg_basic_rate=basic_bill/kwh,
         kwh_per_acct=kwh/no_accts,
         bill_per_acct=amt_billed/no_accts,
         basic_bill_per_acct=kwh_per_acct*avg_basic_rate,
         bill_diff_per_acct=bill_per_acct-basic_bill_per_acct,
         avg_premium=avg_rate-avg_basic_rate)

summary(supplier_by_month)
summary(supplier_by_year)

write_csv(supplier_by_month,"../output/0111_supplier_by_month.csv")
write_csv(supplier_by_year,"../output/0111_supplier_by_year.csv")



# ---- census data ----
# Note that some census rows have NA in the raw data

census_english=read_csv("census_english.csv",na="-") %>%
  mutate(zip=paste0("0",zip)) %>% 
  rename(zcta=zip)
census_race=read_csv("census_race.csv") %>%
  mutate(zip=paste0("0",zip)) %>% 
  rename(zcta=zip)
census_poverty=read_csv("census_poverty.csv",na=c("-","(X)")) %>%
  mutate(zip=paste0("0",zip)) %>% 
  rename(zcta=zip)
census_income=read_csv("census_income.csv",na=c("-","(X)")) %>%
  mutate(zip=paste0("0",zip)) %>% 
  rename(zcta=zip)
census_disability=read_csv("census_disability.csv",na=c("-","(X)")) %>%
  mutate(zip=paste0("0",zip)) %>% 
  rename(zcta=zip)
census_age=read_csv("census_age.csv",na=c("-","(X)")) %>%
  mutate(zip=paste0("0",zip)) %>% 
  rename(zcta=zip)

xwalk=read_excel("zip_to_zcta.xlsx") 


# ---- zipcode analysis - most disaggregated ----

all_files=list.files()
zipcode_cs_files=grep("^\\w+_q[4,5]_cs.xlsx",all_files,value=T)
zipcode_basic_files=grep("^\\w+_q[4,5]_basic.xlsx",all_files,value=T)
zipcode_agg_files=grep("^\\w+_q[4,5]_agg.xlsx",all_files,value=T)

basic_rates=read_excel("Basic_Rates_TH.xlsx")

zipcode_basic=lapply(zipcode_basic_files,function(x){
  return(read_excel(x,col_types = c("date","text","text","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=zip,
         municipality=str_to_title(municipality)) %>%
  rename(no_basic_accts=no_accts) %>%
  select(-date)

summary(zipcode_basic)

zipcode_agg=lapply(zipcode_agg_files,function(x){
  return(read_excel(x,col_types = c("date","text","text","text","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=zip,
         municipality=str_to_title(municipality)) %>%
  group_by(zip,municipality,region,income) %>%
  summarize(no_agg_accts=sum(no_accts)) 

summary(zipcode_agg)


# TH added a col for kwh, which is in the raw data, but not used below
zipcode_cs=lapply(zipcode_cs_files,function(x){
  return(read_excel(x,col_types = c("date","numeric","text","text","numeric","numeric","numeric","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=paste0("0",zip),
         municipality=str_to_title(municipality)) %>%
  group_by(zip,municipality,region,income) %>%
  summarize(avg_cs_rate=sum(rate*no_accts)/sum(no_accts),
            no_cs_accts=sum(no_accts),
            new_cs_accts=sum(new_accts)) 

summary(zipcode_cs)

suppliers_by_zip=lapply(zipcode_cs_files,function(x){
  return(read_excel(x,col_types = c("date","numeric","text","text","numeric","numeric","numeric","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=paste0("0",zip),
         municipality=str_to_title(municipality),
         supplier_id=substr(supplier,1,5)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  filter(income=="all") %>% 
  group_by(zip,municipality,region,supplier_id) %>%
  summarize(suppliers=1) %>% 
  group_by(zip,municipality,region) %>%
  summarize(suppliers=n()) 

summary(suppliers_by_zip)

zips=full_join(zipcode_basic,zipcode_cs,by=c("zip","municipality","region","income")) %>%
  full_join(zipcode_agg,by=c("zip","municipality","region","income")) %>%
  left_join(filter(basic_rates,date==max(basic_rates$date)), by = "region") %>% 
  select(-date) 

summary(zips)
# Above generates some NAs because not every ZIP has all kinds of accounts

zips[is.na(zips$no_basic_accts),"no_basic_accts"]=0
zips[is.na(zips$no_cs_accts),"no_cs_accts"]=0
zips[is.na(zips$new_cs_accts),"new_cs_accts"]=0
zips[is.na(zips$no_agg_accts),"no_agg_accts"]=0

zips=zips %>%
  mutate(rate_dif=avg_cs_rate-basic_rate,
         tot_accts=no_basic_accts+no_cs_accts+no_agg_accts) 

summary(zips)
# This has some NAs

zips=recast(zips,zip+municipality+region~income+variable, sum) %>%
  rename(basic_rate=all_basic_rate) %>%
  mutate(low_basic_rate=NULL)

zips[is.na(zips$low_tot_accts),"low_tot_accts"]=0
zips[is.na(zips$low_no_basic_accts),"low_no_basic_accts"]=0
zips[is.na(zips$low_no_cs_accts),"low_no_cs_accts"]=0
zips[is.na(zips$low_new_cs_accts),"low_new_cs_accts"]=0
zips[is.na(zips$low_no_agg_accts),"low_no_agg_accts"]=0

summary(zips)
# THis has NAs

zips=zips %>%
  group_by(zip,municipality,region) %>% 
  summarize(basic_rate=sum(basic_rate*all_tot_accts)/sum(all_tot_accts),
            all_avg_cs_rate=sum(all_avg_cs_rate*all_no_cs_accts)/sum(all_no_cs_accts),
            all_avg_rate_dif=ifelse(sum(all_no_cs_accts)>0,
                                    sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
                                    NA),
            all_tot_accts=sum(all_tot_accts),
            all_no_cs_accts=sum(all_no_cs_accts),
            all_no_agg_accts=sum(all_no_agg_accts),
            all_no_basic_accts=sum(all_no_basic_accts),
            low_avg_cs_rate=ifelse(sum(low_no_cs_accts)>0,
                                   sum(low_avg_cs_rate*low_no_cs_accts,na.rm=T)/sum(low_no_cs_accts),
                                   NA),
            low_avg_rate_dif=ifelse(sum(low_no_cs_accts)>0,
                                    sum(low_rate_dif*low_no_cs_accts,na.rm=T)/sum(low_no_cs_accts),
                                    NA),
            low_tot_accts=sum(low_tot_accts),
            low_no_cs_accts=sum(low_no_cs_accts),
            low_no_agg_accts=sum(low_no_agg_accts),
            low_no_basic_accts=sum(low_no_basic_accts)) %>% 
  ungroup() %>% 
  mutate(other_tot_accts=all_tot_accts-low_tot_accts,
         other_no_cs_accts=all_no_cs_accts-low_no_cs_accts,
         other_no_agg_accts=all_no_agg_accts-low_no_agg_accts,
         other_no_basic_accts=all_no_basic_accts-low_no_basic_accts,
         other_avg_cs_rate=ifelse(low_no_cs_accts>0&other_no_cs_accts>0,
                                  (all_no_cs_accts*all_avg_cs_rate-low_no_cs_accts*low_avg_cs_rate)/
                                    other_no_cs_accts,
                                  ifelse(low_no_cs_accts==0,all_avg_cs_rate,NA)),
         other_avg_rate_dif=other_avg_cs_rate-basic_rate,
         all_pct_cs=all_no_cs_accts/all_tot_accts,
         low_pct_cs=ifelse(low_tot_accts>0,
                           low_no_cs_accts/low_tot_accts,
                           NA),
         other_pct_cs=ifelse(other_tot_accts>0,
                             other_no_cs_accts/other_tot_accts,
                             NA),
         agg_present=ifelse(all_no_agg_accts>0,1,0),
         pct_low_accts=low_tot_accts/all_tot_accts) %>%
  left_join(suppliers_by_zip,by = c("zip","municipality","region")) %>% 
  left_join(xwalk) %>%
  left_join(census_english) %>%
  left_join(census_race) %>%
  left_join(census_income) %>%
  left_join(census_poverty) %>%
  left_join(census_age) %>%
  left_join(census_disability)
write_csv(zips,"../output/0111_master_zipcode.csv")

# this has some NA's, but not Acton-all...

agg=filter(zips,agg_present==1) %>%
  select(1:16)
write_csv(agg,"../output/0111_aggregator_present.csv")

zips_agg=zips %>% 
  group_by(agg_present) %>% 
  summarize(all_cs_accts=sum(all_no_cs_accts,na.rm=T),
            all_tot_accts=sum(all_tot_accts,na.rm=T),
            low_cs_accts=sum(low_no_cs_accts,na.rm=T),
            low_tot_accts=sum(low_tot_accts,na.rm=T)) %>% 
  mutate(all_particip=all_cs_accts/all_tot_accts,
         low_particip=low_cs_accts/low_tot_accts)
write_csv(zips_agg,"../output/0111_zips_agg_summary.csv")


# regressions ----

# simple
reg=list()
reg[[1]]=felm(low_pct_cs~pct_low_accts|region,
              data=filter(zips,low_tot_accts>9))
reg[[2]]=felm(low_pct_cs~log(hh_inc)|region,
              data=filter(zips,low_tot_accts>9))
reg[[3]]=felm(other_pct_cs~pct_low_accts|region,
              data=filter(zips,other_tot_accts>9&low_tot_accts>9))
reg[[4]]=felm(other_pct_cs~log(hh_inc)|region,
              data=filter(zips,other_tot_accts>9&low_tot_accts>9))
stargazer(reg,type="text")

reg=list()
reg[[1]]=felm(low_pct_cs~pct_low_accts:region+region,
              data=filter(zips,low_tot_accts>9))
reg[[2]]=felm(low_pct_cs~log(hh_inc):region+region,
              data=filter(zips,low_tot_accts>9))
reg[[3]]=felm(other_pct_cs~pct_low_accts:region+region,
              data=filter(zips,other_tot_accts>9&low_tot_accts>9))
reg[[4]]=felm(other_pct_cs~log(hh_inc):region+region,
              data=filter(zips,other_tot_accts>9&low_tot_accts>9))
stargazer(reg,type="text")


# other ~ inc + nw
reg=list()
reg[[1]]=felm(other_pct_cs~log(all_tot_accts)+pct_nw_or_h+log(hh_inc)+
                agg_present+suppliers|region|0|municipality,
              data=filter(zips,other_tot_accts>9))
# other ~ pct_low + nw
reg[[2]]=felm(other_pct_cs~log(all_tot_accts)+pct_nw_or_h+pct_low_accts+
                agg_present+suppliers|region|0|municipality,
              data=filter(zips,other_tot_accts>9))
# low ~ income + nw
reg[[3]]=felm(low_pct_cs~log(all_tot_accts)+pct_nw_or_h+log(hh_inc)+
                agg_present+suppliers|region|0|municipality,
              data=filter(zips,low_tot_accts>9))
# low ~ pct_low + nw
reg[[4]]=felm(low_pct_cs~log(all_tot_accts)+pct_nw_or_h+pct_low_accts+
                agg_present+suppliers|region|0|municipality,
              data=filter(zips,low_tot_accts>9))
# all ~ income + nw
reg[[5]]=felm(all_pct_cs~log(all_tot_accts)+pct_nw_or_h+log(hh_inc)+
                agg_present+suppliers|region|0|municipality,
              data=filter(zips,all_tot_accts>9))
# all ~ pct_low + nw
reg[[6]]=felm(all_pct_cs~log(all_tot_accts)+pct_nw_or_h+pct_low_accts+
                agg_present+suppliers|region|0|municipality,
              data=filter(zips,all_tot_accts>9))
stargazer(reg,type="text")

# # other ~ inc + nw
reg=list()
reg[[1]]=felm(other_avg_rate_dif~log(all_tot_accts)+pct_nw_or_h+log(hh_inc)+
                agg_present+suppliers|region|0|municipality,
              data=filter(zips,low_tot_accts>9))
# other ~ pct_low + nw
reg[[2]]=felm(other_avg_rate_dif~log(all_tot_accts)+pct_nw_or_h+pct_low_accts+
                agg_present+suppliers|region|0|municipality,
              data=filter(zips,low_tot_accts>9))
# low ~ income + nw
reg[[3]]=felm(low_avg_rate_dif~log(all_tot_accts)+pct_nw_or_h+log(hh_inc)+
                agg_present+suppliers|region|0|municipality,
              data=filter(zips,low_tot_accts>9))
# low ~ pct_low + nw
reg[[4]]=felm(low_avg_rate_dif~log(all_tot_accts)+pct_nw_or_h+pct_low_accts+
                agg_present+suppliers|region|0|municipality,
              data=filter(zips,low_tot_accts>9))
# all ~ income + nw
reg[[5]]=felm(all_avg_rate_dif~log(all_tot_accts)+pct_nw_or_h+log(hh_inc)+
                agg_present+suppliers|region|0|municipality,
              data=filter(zips,low_tot_accts>9))
# all ~ pct_low + nw
reg[[6]]=felm(all_avg_rate_dif~log(all_tot_accts)+pct_nw_or_h+pct_low_accts+
                agg_present+suppliers|region|0|municipality,
              data=filter(zips,low_tot_accts>9))
stargazer(reg,type="text")

reg=felm(all_pct_cs~pct_low_accts+pct_nw_or_h|municipality,
         data=filter(zips,all_tot_accts>9))

reg=lm(low_pct_cs~pct_low_accts,#+pct_nw_or_h+municipality,
       data=filter(zips,low_tot_accts>9))
summary(reg)

cor.test(filter(zips,low_tot_accts>9)$low_pct_cs,filter(zips,low_tot_accts>9)$pct_low_accts)

temp=zips %>%
  gather(key="income",value="dif",other_avg_rate_dif,low_avg_rate_dif)

reg=felm(dif~income+log(all_tot_accts)+pct_nw_or_h+pct_low_accts+
           agg_present+suppliers|region|0|municipality,
         data=filter(temp,low_tot_accts>9&!is.na(dif)))
summary(reg)




plotdata=filter(zips,low_tot_accts>9,
                municipality %in% c("Boston","Worcester",
                                    "East Boston","South Boston","S Boston",
                                    "Allston","Brighton","Charlestown",
                                    "Dorchester","Hyde Park","Jamaica Plain",
                                    "Mattapan","Roslindale","Roxbury","Roxbry Xng",
                                    "West Roxbury",
                                    "Springfield","Indian Orchard",
                                    "Worcester")) %>% 
  mutate(municipality=ifelse(municipality %in% c("East Boston","South Boston","S Boston",
                                                 "Allston","Brighton","Charlestown",
                                                 "Dorchester","Hyde Park","Jamaica Plain",
                                                 "Mattapan","Roslindale","Roxbury","Roxbry Xng",
                                                 "West Roxbury"),
                             "Boston",
                             municipality),
         municipality=ifelse(municipality %in% c("Indian Orchard"),
                             "Springfield",
                             municipality)) %>% 
  select(pct_low_accts,all_pct_cs,zip,municipality)
fwrite(plotdata,file="../output/scatter_plot.csv")

p12<- ggplot(data=plotdata,aes(x=pct_low_accts,y=all_pct_cs,label=zip,color=municipality))+
  geom_text(size=3,fontface="bold")+
  scale_x_continuous(labels=percent)+
  scale_y_continuous(labels=percent)+
  scale_color_discrete(name="Municipality",
                       labels=c("Boston","Springfield","Worcester"))+
  labs(x="Share of low income customers",y="Participation in the competitive supply market")+
  theme(legend.position = c(.86,.2))
p12
file_name = paste(plot_dir,"plot12_participation_vs_share_low_inc_zips.png", sep = "")
png(file_name, width = 500, height = 500)
print(p12)
dev.off()
p12

# large for printing

library(ggrepel)

theme_set(theme_gray(base_size = 50))
png(filename="../plots/plot13_bs.png",
    width=1.35*1250,
    height=1*1250)
ggplot(data=plotdata,aes(x=pct_low_accts,y=all_pct_cs,label=zip,color=municipality))+
  geom_text_repel(size=12,fontface="bold")+
  scale_x_continuous(labels=percent)+
  scale_y_continuous(labels=percent)+
  scale_color_discrete(name="Municipality",
                       labels=c("Boston","Springfield","Worcester"))+
  labs(x="Share of low income customers",y="Participation in the competitive supply market")+
  theme(legend.position = c(.86,.2),
        legend.margin=margin(2,1,1,1,"line"),
        legend.key.height=unit(4,"line"))
dev.off()


# This is really ugly - too full.  Not used?
# ggplot(data=filter(zips,low_tot_accts>9),aes(x=pct_nw_or_h,y=all_pct_cs,label=zip))+
#   geom_text(size=3)+
#   scale_x_continuous(labels=percent)+
#   scale_y_continuous(labels=percent)+
#   labs(x="Share of low income customers",y="Participation in the competitive supply market")



# cor.test(filter(zips,other_tot_accts>19)$other_pct_cs,filter(zips,other_tot_accts>19)$pct_low_accts)
# ggplot(data=filter(zips,other_tot_accts>19),aes(x=pct_low_accts,y=other_pct_cs))+
#   geom_point()

# ---- summaries by top 20 ----
rest_of_state_in_one_line=zips %>%
  filter(all_tot_accts>9) %>%
  select(zip,municipality,
         all_tot_accts,all_avg_rate_dif,all_pct_cs,all_no_cs_accts,
         low_tot_accts,low_avg_rate_dif,low_pct_cs,low_no_cs_accts,
         other_tot_accts,other_avg_rate_dif,other_pct_cs,other_no_cs_accts) %>%
  summarize(zip="-",
            municipality="Rest of State",
            all_tot_accts=sum(all_tot_accts),
            all_avg_rate_dif=sum(all_avg_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts))

maj_min=zips %>%
  filter(pct_nw_or_h>.5,
         all_tot_accts>9) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,pct_nw_or_h,
         all_tot_accts,all_avg_rate_dif,all_pct_cs,
         low_pct_cs,other_pct_cs,pct_low_accts) %>%
  arrange(desc(pct_nw_or_h)) 
summary=zips %>%
  filter(pct_nw_or_h>.5,
         all_tot_accts>9) %>%
  summarize(zip="-",
            municipality="Majority Minority",
            pct_nw_or_h=sum(pct_nw_or_h*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            all_avg_rate_dif=sum(all_avg_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts))
rest_of_state=zips %>%
  filter(!(is.na(pct_nw_or_h)), # 46 rows missing pct_nw_or_h
         pct_nw_or_h<=.5,
         all_tot_accts>9) %>%
  summarize(zip="-",
            municipality="Rest of State",
            pct_nw_or_h=sum(pct_nw_or_h*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            all_avg_rate_dif=sum(all_avg_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
maj_min=bind_rows(summary,rest_of_state,maj_min)
write_csv(maj_min,"../output/0111_majority_minority.csv")

top_20_bl=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_bl)) %>%
  head(.,n=20) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,pct_bl,
         all_tot_accts,all_avg_rate_dif,all_pct_cs,
         low_pct_cs,other_pct_cs,pct_low_accts) 
summary=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_bl)) %>%
  head(.,n=20) %>%
  summarize(zip="-",
            municipality="Top 20: Pct Black",
            pct_bl=sum(pct_bl*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            all_avg_rate_dif=sum(all_avg_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts))
rest_of_state=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_bl)) %>%
  filter(!is.na(pct_bl)) %>% # 46 rows missing pct_bl
  tail(.,n=nrow(.)-20) %>%
  summarize(zip="-",
            municipality="Rest of State",
            pct_bl=sum(pct_bl*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            all_avg_rate_dif=sum(all_avg_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
top_20_bl=bind_rows(summary,rest_of_state,top_20_bl)
write_csv(top_20_bl,"../output/0111_top_20_black.csv")

top_20_hi=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_hi)) %>%
  head(.,n=20) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,pct_hi,
         all_tot_accts,all_avg_rate_dif,all_pct_cs,
         low_pct_cs,other_pct_cs,pct_low_accts) 
summary=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_hi)) %>%
  head(.,n=20) %>%
  summarize(zip="-",
            municipality="Top 20: Pct Hispanic",
            pct_hi=sum(pct_hi*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            all_avg_rate_dif=sum(all_avg_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts))
rest_of_state=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_hi)) %>%
  filter(!is.na(pct_hi)) %>% # 46 rows missing pct_hi
  tail(.,n=nrow(.)-20) %>%
  summarize(zip="-",
            municipality="Rest of State",
            pct_hi=sum(pct_hi*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            all_avg_rate_dif=sum(all_avg_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
top_20_hi=bind_rows(summary,rest_of_state,top_20_hi)
write_csv(top_20_hi,"../output/0111_top_20_hisp.csv")

top_20_lep=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_lep)) %>%
  head(.,n=20) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts,
         pct_lep=pct_lep/100) %>%
  select(zip,municipality,pct_lep,
         all_tot_accts,all_avg_rate_dif,all_pct_cs,
         low_pct_cs,other_pct_cs,pct_low_accts) 
summary=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_lep)) %>%
  head(.,n=20) %>%
  summarize(zip="-",
            municipality="Top 20: Pct Lim English",
            pct_lep=sum(pct_lep*all_tot_accts)/(100*sum(all_tot_accts)),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            all_avg_rate_dif=sum(all_avg_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts))
rest_of_state=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(pct_lep)) %>%
  filter(!is.na(pct_lep)) %>% # 48 rows missing pct_lep
  tail(.,n=nrow(.)-20) %>%
  summarize(zip="-",
            municipality="Rest of State",
            pct_lep=sum(pct_lep*all_tot_accts)/(100*sum(all_tot_accts)),
            all_tot_accts=sum(all_tot_accts),
            all_avg_rate_dif=sum(all_avg_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
top_20_lep=bind_rows(summary,rest_of_state,top_20_lep)
write_csv(top_20_lep,"../output/0111_top_20_lep.csv")

top_20_income=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(hh_inc)) %>%
  head(.,n=20) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,hh_inc,
         all_tot_accts,all_avg_rate_dif,all_pct_cs,
         low_pct_cs,other_pct_cs,pct_low_accts) 
summary=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(hh_inc)) %>%
  head(.,n=20) %>%
  summarize(zip="-",
            municipality="Top 20: Med HH Inc",
            hh_inc=sum(hh_inc*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            all_avg_rate_dif=sum(all_avg_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts))
rest_of_state=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(hh_inc)) %>%
  filter(!is.na(hh_inc)) %>% # 63 rows missing hh_inc
  tail(.,n=nrow(.)-20) %>%
  summarize(zip="-",
            municipality="Rest of State",
            hh_inc=sum(hh_inc*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            all_avg_rate_dif=sum(all_avg_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
top_20_income=bind_rows(summary,rest_of_state,top_20_income)
write_csv(top_20_income,"../output/0111_top_20_income.csv")

bottom_20_income=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(hh_inc) %>%
  head(.,n=20) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,hh_inc,
         all_tot_accts,all_avg_rate_dif,all_pct_cs,
         low_pct_cs,other_pct_cs,pct_low_accts)  
summary=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(hh_inc) %>%
  head(.,n=20) %>%
  summarize(zip="-",
            municipality="Bottom 20: Med HH Inc",
            hh_inc=sum(hh_inc*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            all_avg_rate_dif=sum(all_avg_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts))
rest_of_state=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(hh_inc) %>%
  filter(!is.na(hh_inc)) %>% # 63 rows missing hh_inc
  tail(.,n=nrow(.)-20) %>%
  summarize(zip="-",
            municipality="Rest of State",
            hh_inc=sum(hh_inc*all_tot_accts)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            all_avg_rate_dif=sum(all_avg_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
bottom_20_income=bind_rows(summary,rest_of_state,bottom_20_income)
write_csv(bottom_20_income,"../output/0111_bottom_20_income.csv")

top_20_pct_li=zips %>%
  filter(all_tot_accts>9) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%  
  arrange(desc(pct_low_accts)) %>%
  head(.,n=20) %>%
  select(zip,municipality,
         all_tot_accts,all_avg_rate_dif,all_pct_cs,
         low_pct_cs,other_pct_cs,pct_low_accts,low_avg_rate_dif)  
summary=zips %>%
  filter(all_tot_accts>9) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%  
  arrange(desc(pct_low_accts)) %>%
  head(.,n=20) %>%
  summarize(zip="-",
            municipality="Top 20: Pct LI",
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            all_avg_rate_dif=sum(all_avg_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            low_avg_rate_dif=sum(low_avg_rate_dif*low_no_cs_accts)/sum(low_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts))
rest_of_state=zips %>%
  filter(all_tot_accts>9) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%  
  arrange(desc(pct_low_accts)) %>%
  tail(.,n=nrow(.)-20) %>%
  summarize(zip="-",
            municipality="Rest of State",
            all_tot_accts=sum(all_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts,
            all_avg_rate_dif=sum(all_avg_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            low_avg_rate_dif=sum(low_avg_rate_dif*low_no_cs_accts,na.rm=T)/sum(low_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts))
top_20_pct_li=bind_rows(summary,rest_of_state,top_20_pct_li)
write_csv(top_20_pct_li,"../output/0111_top_20_pct_li.csv")

top_50_markup_other=zips %>%
  filter(other_no_cs_accts>9,
         other_avg_rate_dif<.2) %>%
  arrange(desc(other_avg_rate_dif)) %>%
  head(.,n=50) %>%
  select(zip,municipality,
         other_tot_accts,other_avg_rate_dif,other_pct_cs)  
state_average=zips %>%
  filter(other_no_cs_accts>9,
         other_avg_rate_dif<.2) %>%
  summarize(zip="-",
            municipality="Statewide",
            other_tot_accts=sum(other_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/other_tot_accts,
            other_avg_rate_dif=sum(other_avg_rate_dif*other_no_cs_accts)/sum(other_no_cs_accts))
top_50_markup_other=bind_rows(state_average,top_50_markup_other)
write_csv(top_50_markup_other,"../output/0111_top_50_markup_other.csv")

top_50_markup_low=zips %>%
  filter(low_no_cs_accts>9, 
         low_avg_rate_dif<.2) %>%
  arrange(desc(low_avg_rate_dif)) %>%
  head(.,n=50) %>%
  select(zip,municipality,
         low_tot_accts,low_avg_rate_dif,low_pct_cs)  
state_average=zips %>%
  filter(low_no_cs_accts>9, 
         low_avg_rate_dif<.2) %>%
  summarize(zip="-",
            municipality="Statewide",
            low_tot_accts=sum(low_tot_accts),
            low_pct_cs=sum(low_no_cs_accts)/low_tot_accts,
            low_avg_rate_dif=sum(low_avg_rate_dif*low_no_cs_accts,na.rm=T)/sum(low_no_cs_accts))
top_50_markup_low=bind_rows(state_average,top_50_markup_low)
write_csv(top_50_markup_low,"../output/0111_top_50_markup_low.csv")

# ---- zip level ----

all_files=list.files()
zipcode_cs_files=grep("^\\w+_q[4,5]_cs.xlsx",all_files,value=T)
zipcode_basic_files=grep("^\\w+_q[4,5]_basic.xlsx",all_files,value=T)
zipcode_agg_files=grep("^\\w+_q[4,5]_agg.xlsx",all_files,value=T)

basic_rates=read_excel("Basic_Rates_TH.xlsx")

zipcode_basic_zip=lapply(zipcode_basic_files,function(x){
  return(read_excel(x,col_types = c("date","text","text","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=zip,
         municipality=str_to_title(municipality)) %>%
  select(-date) %>%
  group_by_(.dots=c("zip","region","income")) %>%
  summarize(no_basic_accts=sum(no_accts))

summary(zipcode_basic_zip)

zipcode_agg_zip=lapply(zipcode_agg_files,function(x){
  return(read_excel(x,col_types = c("date","text","text","text","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=zip,
         municipality=str_to_title(municipality)) %>%
  group_by_(.dots=c("zip","region","income")) %>%
  summarize(no_agg_accts=sum(no_accts)) 

summary(zipcode_agg_zip)

# TH added a numeric column to match the raw file output with "new_accts"
zipcode_cs_zip=lapply(zipcode_cs_files,function(x){
  return(read_excel(x,col_types = c("date","text","text","text","numeric","numeric","numeric","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=zip,
         municipality=str_to_title(municipality)) %>%
  group_by_(.dots=c("zip","region","income")) %>%
  summarize(avg_cs_rate=sum(rate*no_accts,na.rm=T)/sum(no_accts,na.rm=T),
            no_cs_accts=sum(no_accts,na.rm=T),
            new_cs_accts=sum(new_accts,na.rm=T)) 

zips_zip=full_join(zipcode_basic_zip,zipcode_cs_zip,by=c("zip", "region","income")) %>%
  full_join(zipcode_agg_zip,by=c("zip", "region","income")) %>%
  left_join(filter(basic_rates,date==max(basic_rates$date)), by = "region") %>% 
  select(-date)

zips_zip[is.na(zips_zip$no_basic_accts),"no_basic_accts"]=0
zips_zip[is.na(zips_zip$no_cs_accts),"no_cs_accts"]=0
zips_zip[is.na(zips_zip$new_cs_accts),"new_cs_accts"]=0
zips_zip[is.na(zips_zip$no_agg_accts),"no_agg_accts"]=0
#zips_zip[is.na(zips_zip$avg_cs_rate),"avg_cs_rate"]=0

zips_zip=zips_zip %>%
  mutate(tot_accts=no_basic_accts+no_cs_accts+no_agg_accts) 

zips_zip=recast(zips_zip,zip+region~income+variable) %>%
  rename(basic_rate=all_basic_rate) %>%
  mutate(low_basic_rate=NULL)

zips_zip[is.na(zips_zip$low_tot_accts),"low_tot_accts"]=0
zips_zip[is.na(zips_zip$low_no_basic_accts),"low_no_basic_accts"]=0
zips_zip[is.na(zips_zip$low_no_cs_accts),"low_no_cs_accts"]=0
zips_zip[is.na(zips_zip$low_new_cs_accts),"low_new_cs_accts"]=0
zips_zip[is.na(zips_zip$low_no_agg_accts),"low_no_agg_accts"]=0
#zips_zip[is.na(zips_zip$low_avg_cs_rate),"low_avg_cs_rate"]=0

zips_zip_map=zips_zip %>%
  group_by(zip, region) %>% 
  summarize(all_tot_accts=sum(all_tot_accts),
            all_no_basic_accts=sum(all_no_basic_accts),
            all_no_agg_accts=sum(all_no_agg_accts),
            all_no_cs_accts=sum(all_no_cs_accts),
            low_tot_accts=sum(low_tot_accts),
            low_no_basic_accts=sum(low_no_basic_accts),
            low_no_agg_accts=sum(low_no_agg_accts),
            low_no_cs_accts=sum(low_no_cs_accts)) %>% 
  ungroup() %>% 
  mutate(other_tot_accts=all_tot_accts-low_tot_accts,
         other_no_cs_accts=all_no_cs_accts-low_no_cs_accts,
         other_no_agg_accts=all_no_agg_accts-low_no_agg_accts,
         all_pct_cs=all_no_cs_accts/all_tot_accts,
         low_pct_cs=ifelse(low_tot_accts>0,
                           low_no_cs_accts/low_tot_accts,
                           NA),
         other_pct_cs=ifelse(other_tot_accts>0,
                             other_no_cs_accts/other_tot_accts,
                             NA)) %>%
  filter(all_tot_accts>9) %>%
  mutate(agg_present=ifelse(all_no_agg_accts>9,1,0)) %>% 
  select(zip,region,all_pct_cs,low_pct_cs,other_pct_cs,agg_present,all_tot_accts)

write_csv(filter(zips_zip_map,agg_present==1),"../output/0111_zips_map_agg_sep19.csv")
write_csv(filter(zips_zip_map,agg_present==0),"../output/0111_zips_map_noagg_sep19.csv")
write_csv(zips_zip_map,"../output/0111_zips_map_sep19.csv")


# ---- visualizations and charts----
melt_all=filter(zips_zip_map,all_tot_accts>10) %>%
  select(zip,region,
         grep("all_",colnames(.))) %>%
  melt(.,id.vars=c("zip","region")) %>%
  mutate(variable=substr(as.character(variable),5,100)) %>%
  spread(variable,value) %>%
  mutate(income="all")

melt_other=filter(zips_zip_map,all_tot_accts>10) %>%
  select(zip,region,
         grep("other_",colnames(.))) %>%
  melt(.,id.vars=c("zip","region")) %>%
  mutate(variable=substr(as.character(variable),7,100)) %>%
  spread(variable,value) %>%
  mutate(income="other")

melt_low=filter(zips_zip_map,all_tot_accts>10) %>%
  select(zip,region,
         grep("low_",colnames(.))) %>%
  melt(.,id.vars=c("zip","region")) %>%
  mutate(variable=substr(as.character(variable),5,100)) %>%
  spread(variable,value) %>%
  mutate(income="low")

tidy_zips=bind_rows(melt_all,melt_other,melt_low) %>%
  left_join(xwalk) %>%
  left_join(census_english) %>%
  left_join(census_race) %>%
  left_join(census_income) %>%
  left_join(census_poverty) %>%
  left_join(census_age) %>%
  left_join(census_disability) %>%
  rename("region"="region") %>%
  filter(income %in% c("low","other","all")) %>%
  mutate(income=factor(income),
         income=relevel(income,"other"),
         race_quant=factor(ntile(pct_nw_or_h,4)),
         inc_quant=factor(ntile(hh_inc,4)),
         poverty_quant=factor(ntile(pct_pov,4)),
         lep_quant=factor(ntile(pct_lep,4)))

summary(lm(data=tidy_zips,pct_cs~income:pct_lep:region+income:pct_nw_or_h:region+
             income:I(hh_inc/10000):region+income:pct_over_65:region+income:pct_disability:region))
# TH adding this
tidy_zips$no_cs_accts <- tidy_zips$pct_cs*tidy_zips$tot_accts

data_for_charts=tidy_zips %>%
  group_by(income,region) %>%
  mutate(pct_cs_region=sum(no_cs_accts)/sum(tot_accts)) %>%
  group_by(race_quant,income) %>%
  mutate(pct_cs_race=sum(no_cs_accts)/sum(tot_accts)) %>%
  group_by(inc_quant,income) %>%
  mutate(pct_cs_inc=sum(no_cs_accts)/sum(tot_accts)) %>%
  group_by(lep_quant,income) %>%
  mutate(pct_cs_lep=sum(no_cs_accts)/sum(tot_accts))

#  this has no good outcome.  bad data, or bad idea?
# p14<-ggplot(filter(data_for_charts,income %in% c("other","low"), is.na(pct_cs_region)==F),aes(x=region,y=pct_cs_region,fill=income))+
#   geom_col(position="dodge")+
#   #scale_y_continuous(labels=percent)+
#   labs(x=NULL,
#        y="Accounts enrolled in competitive supply")+
#   scale_fill_discrete(name="Income",
#                       labels=c("All other accounts","Low income accounts"))+
#   theme(legend.position = "bottom")
# png("../plots/plot14_percent_cs_low_income_by_region.png")
# print(p14)
# dev.off()
# p14
# 
# p15<- ggplot(filter(data_for_charts,!(is.na(race_quant)) & income %in% c("other","low")),
#              aes(x=race_quant,y=pct_cs_race,fill=income))+
#   geom_bar(stat="identity",position="dodge")+
#   scale_y_continuous(labels=percent)+
#   scale_x_discrete(labels=c("Low","Med-low","Med-high","High"))+
#   labs(x="Percent nonwhite households (zip level)",
#        y="Accounts enrolled in competitive supply")+
#   scale_fill_discrete(name="Income",
#                       labels=c("All other accounts","Low income accounts"))+
#   theme(legend.position = "bottom")
# png("../plots/plot15_percent_cs_by_nonwhite.png")
# print(p15)
# dev.off()
# p15
# 
# p16<-ggplot(filter(data_for_charts,!(is.na(inc_quant)) & income %in% c("other","low")),
#             aes(x=inc_quant,y=pct_cs_inc,fill=income))+
#   geom_bar(stat="identity",position="dodge")+
#   scale_y_continuous(labels=percent)+
#   scale_x_discrete(labels=c("Low","Med-low","Med-high","High"))+
#   labs(x="Median household income (zip level)",
#        y="Accounts enrolled in competitive supply")+
#   scale_fill_discrete(name="Income",
#                       labels=c("All other accounts","Low income accounts"))+
#   theme(legend.position = "bottom")
# png("../plots/plot16_percent_cs_by_income.png")
# print(p16)
# dev.off()
# p16


# ---- muni-level (loss) ---- 
all_files=list.files()
zipcode_cs_files=grep("^\\w+_q[4,5]_cs.xlsx",all_files,value=T)
zipcode_basic_files=grep("^\\w+_q[4,5]_basic.xlsx",all_files,value=T)
zipcode_agg_files=grep("^\\w+_q[4,5]_agg.xlsx",all_files,value=T)

basic_rates=read_excel("Basic_Rates_TH.xlsx")

zipcode_basic_muni=lapply(zipcode_basic_files,function(x){
  return(read_excel(x,col_types = c("date","text","text","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=zip,
         municipality=str_to_title(municipality)) %>%
  select(-date) %>%
  group_by(municipality,income) %>%
  summarize(no_basic_accts=sum(no_accts))

summary(zipcode_basic_muni)

zipcode_basic_other=zipcode_basic_muni %>% 
  group_by(municipality) %>%
  summarize(no_basic_accts=ifelse(n()==2,max(no_basic_accts)-min(no_basic_accts),
                                  ifelse(income=="all",no_basic_accts,NA)),
            income="other") %>%
  filter(!is.na(no_basic_accts))

summary(zipcode_basic_other)

zipcode_basic_muni=bind_rows(zipcode_basic_muni,zipcode_basic_other)
rm(zipcode_basic_other)

zipcode_agg_muni=lapply(zipcode_agg_files,function(x){
  return(read_excel(x,col_types = c("date","text","text","text","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=zip,
         municipality=str_to_title(municipality)) %>%
  group_by(municipality,income) %>%
  summarize(no_agg_accts=sum(no_accts))

summary(zipcode_agg_muni)

zipcode_agg_other=zipcode_agg_muni %>% 
  group_by(municipality) %>%
  summarize(no_agg_accts=ifelse(n()==2,max(no_agg_accts)-min(no_agg_accts),
                                ifelse(income=="all",no_agg_accts,NA)),
            income="other") %>%
  filter(!is.na(no_agg_accts))

summary(zipcode_agg_other)

zipcode_agg_muni=bind_rows(zipcode_agg_muni,zipcode_agg_other)
rm(zipcode_agg_other)

zipcode_cs_muni=lapply(zipcode_cs_files,function(x){
  return(read_excel(x,col_types = c("date","text","text","text","numeric","numeric","numeric","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=zip,
         municipality=str_to_title(municipality)) 

summary(zipcode_cs_muni)
# here are some NAs because Unitil didn't report kwh at this resolution.
# so I apply average kwh per acct (separately for low and all)
# from the FITCHBURG area as a whole (Q1 and Q2)
zipcode_cs_muni[which(is.na(zipcode_cs_muni$kwh) == T & 
                        zipcode_cs_muni$income == "low"),]$kwh <- 
  zipcode_cs_muni[which(is.na(zipcode_cs_muni$kwh) == T & 
                 zipcode_cs_muni$income == "low"),]$no_accts*236

zipcode_cs_muni[which(is.na(zipcode_cs_muni$kwh) == T & 
                        zipcode_cs_muni$income == "all"),]$kwh <- 
  zipcode_cs_muni[which(is.na(zipcode_cs_muni$kwh) == T & 
  zipcode_cs_muni$income == "all"),]$no_accts*407

summary(zipcode_cs_muni)

zipcode_cs_muni=zipcode_cs_muni %>%
  group_by(date,zip,municipality,supplier,rate,region,income) %>%
  summarize(no_accts=sum(no_accts,na.rm=T),
            new_accts=sum(new_accts,na.rm=T)) 
# No NAs here...

zipcode_cs_other=zipcode_cs_muni %>% 
  group_by(date,zip,municipality,supplier,rate,region) %>% 
  summarize(no_accts=ifelse(n()==2,max(no_accts)-min(no_accts),
                            ifelse(income=="all",no_accts,NA)),
            new_accts=ifelse(n()==2,max(new_accts)-min(new_accts),
                             ifelse(income=="all",new_accts,NA)),
            income="other") %>%
  filter(!is.na(no_accts))

zipcode_cs_muni=bind_rows(zipcode_cs_muni,zipcode_cs_other) 
rm(zipcode_cs_other)


use_sept19=monthly_cs_welfare %>%
  filter(as.Date(date)=="2019-09-01") %>%
  group_by(region,income,supplier,rate) %>%
  summarize(kwh=sum(kwh),
            no_accts=sum(no_accts),
            kwh_per_acct=ifelse(kwh==0|no_accts==0,0,kwh/no_accts)) %>% 
  select(region,income,supplier,rate,kwh_per_acct)

use_sept19[use_sept19$region=="Fitchburg"&use_sept19$supplier=="GEXA-NEXTERA","supplier"]="NEXTERA ENERGY LLC"

# zipcode_cs_muni=as.data.table(zipcode_cs_muni) 
# setkey(zipcode_cs_muni,supplier,region,income,rate)
# setDT(use_sept19)
# setkey(use_sept19,supplier,region,income,rate)
# zipcode_muni=use_sept19[zipcode_cs_muni,roll="nearest"] 

zipcode_cs_muni_sept19 <- zipcode_cs_muni %>% filter(as.Date(date) == '2019-09-01')

zipcode_muni <- inner_join(use_sept19, zipcode_cs_muni_sept19, 
                          by.x=c("supplier","region","income","rate"),
                          by.y=c("supplier","region","income","rate"))

zipcode_muni=as.data.frame(zipcode_muni) %>% 
  left_join(basic_rates) %>%
  group_by(municipality,income) %>%
  summarize(cs_bill=sum(rate*no_accts*kwh_per_acct,na.rm=T),
            cs_kwh=sum(no_accts*kwh_per_acct,na.rm=T),
            estimated_use=sum(no_accts*kwh_per_acct,na.rm=T)/sum(no_accts,na.rm=T),
            no_cs_accts=sum(no_accts),
            new_cs_accts=sum(new_accts),
            avg_cs_rate=sum(rate*kwh_per_acct*no_accts,na.rm=T)/sum(kwh_per_acct*no_accts,na.rm=T),
            avg_basic_rate=sum(basic_rate*kwh_per_acct*no_accts,na.rm=T)/sum(kwh_per_acct*no_accts,na.rm=T),
            would_be_basic_bill=sum(no_accts*kwh_per_acct*basic_rate)) %>% 
  mutate(bill_difference=cs_bill-would_be_basic_bill,
         avg_loss_per_hh=bill_difference/no_cs_accts,
         rate_dif=avg_cs_rate-avg_basic_rate) %>% 
  left_join(zipcode_basic_muni) %>% 
  left_join(zipcode_agg_muni) %>% 
  mutate(no_basic_accts=ifelse(is.na(no_basic_accts),0,no_basic_accts),
         no_agg_accts=ifelse(is.na(no_agg_accts),0,no_agg_accts),
         tot_accts=no_cs_accts+no_basic_accts+no_agg_accts,
         pct_cs_participation=no_cs_accts/(no_cs_accts+no_basic_accts+no_agg_accts)) %>% 
  arrange(municipality)

zipcode_muni=zipcode_muni %>% 
  filter(income!="other") %>% 
  mutate(agg_present=ifelse(no_agg_accts>0,1,0)) %>% 
  select(municipality,income,no_cs_accts,bill_difference,
         avg_loss_per_hh,rate_dif,pct_cs_participation,agg_present)

write_csv(zipcode_muni,"../output/0111_municipal_loss_sept19.csv")
