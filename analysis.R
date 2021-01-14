# data input 
library(readr)
library(readxl)

# data mgmt
library(dplyr)
library(stringr)
library(tidyr)
library(zipcode)
library(reshape2)
library(data.table)
library(dtplyr)
library(lubridate)

# regressions
library(lfe)
library(stargazer)

# data visuals
library(ggplot2)
library(scales)

# change this path for your computer so it points to wherever you put the "shared_jun18" folder
# setwd("/Users/irenejacqz/Box/work-for-mom/MA-AG 3")

all_files=list.files("input/")
monthly_cs_files=grep("^\\w+_q[1,2]_cs.xlsx",all_files,value=T)
monthly_basic_files=grep("^\\w+_q[1,2]_basic.xlsx",all_files,value=T)
monthly_agg_files=grep("^\\w+_q[1,2]_agg.xlsx",all_files,value=T)

# get basic rates frome excel
basic_rates=read_excel("basic_rates.xlsx") %>% 
  mutate(date=as.Date(date))

# read each monthly CS excel into one big file
monthly_cs=lapply(monthly_cs_files,function(x){
  read_excel(paste0("input/",x)) 
})
monthly_cs=bind_rows(monthly_cs) %>% 
  mutate(date=as.Date(date)) %>% 
  group_by(date,supplier,rate,region,income) %>%
  summarize(kwh=sum(kwh,na.rm=T),
            amt_billed=sum(amt_billed,na.rm=T),
            no_accts=sum(no_accts,na.rm=T),
            new_accts=sum(new_accts,na.rm=T)) %>% 
  filter(!is.na(date)) 

# join in basic rates to calculate counterfactual bills at observed usage 
monthly_cs_welfare=left_join(monthly_cs,basic_rates, by = c("date", "region")) %>%
  ungroup() %>% 
  mutate(basic_bill=kwh*basic_rate,  
         bill_difference=amt_billed-basic_bill,  
         bill_difference_pp=bill_difference/no_accts,  
         rate_difference=rate-basic_rate,
         kwh_per_acct=ifelse(kwh==0|no_accts==0,0,kwh/no_accts))

# read each basic customer excel into one big file
monthly_basic=lapply(monthly_basic_files,function(x){
  return(read_excel(paste0("input/",x)))
})
monthly_basic=bind_rows(monthly_basic) %>% 
  rename(basic_kwh=kwh,
         basic_billed=amt_billed,
         basic_accts=no_accts) %>% 
  mutate(date=as.Date(date))

# read each agg excel into one big file
monthly_agg=lapply(monthly_agg_files,function(x){
  return(read_excel(paste0("input/",x)))
})
monthly_agg=bind_rows(monthly_agg) %>% 
  group_by(date,region,income) %>% 
  summarize(kwh=sum(kwh,na.rm=T),
            amt_billed=sum(amt_billed,na.rm=T),
            no_accts=sum(no_accts,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(date=as.Date(date)) %>% 
  rename(agg_kwh=kwh,
         agg_billed=amt_billed,
         agg_accts=no_accts)

rm(all_files,monthly_agg_files,monthly_basic_files,monthly_cs_files)

# ----- rate summary plots 

rate_summary_plot=monthly_cs_welfare %>% 
  group_by(date,income) %>%
  summarize(avg_rate=sum(kwh*rate,na.rm=T)/sum(kwh,na.rm=T),
            avg_basic_rate=sum(kwh*basic_rate)/sum(kwh,na.rm=T),
            kwh=sum(kwh,na.rm=T),
            amt_billed=sum(amt_billed,na.rm=T)) 

rate_summary_plot_o=rate_summary_plot %>% 
  group_by(date) %>% 
  summarize(kwh=max(kwh)-min(kwh),
            amt_billed=max(amt_billed)-min(amt_billed),
            income="other") %>% 
  mutate(avg_rate=amt_billed/kwh)

rate_summary_plot=bind_rows(rate_summary_plot,rate_summary_plot_o) %>% 
  ungroup() %>% 
  mutate(rate=ifelse(income=="all",avg_basic_rate,avg_rate),
         income=factor(income,levels=c("low","other","all"))) %>%
  ungroup()

july_points=filter(rate_summary_plot,date=="2019-06-01") %>%
  mutate(date=as.Date("2019-07-01"))

rate_summary_plot=bind_rows(rate_summary_plot,july_points)
rm(july_points)

ggplot(rate_summary_plot,aes(x=date,y=rate,color=factor(income))) +
  geom_step(size=1) +
  scale_y_continuous(labels=dollar,limits=c(0,NA)) +
  scale_fill_discrete(NULL) +
  scale_color_discrete(name=NULL,
                       labels=c("\nCompetitive: \nLow income \n",
                                "\nCompetitive: \nNon-low \nincome \n",
                                "\nBasic \nservice \n")) +
  labs(x=NULL,y="State-wide average rate (dollars per kWh)")
ggsave("annual_rates_longaxis.png",
       path="plots",
       width=6,height=3,units="in")
ggsave("annual_rates_longaxis_larger.png",
       path="plots",
       width=8,height=4,units="in")

ggplot(rate_summary_plot,aes(x=date,y=rate,color=factor(income))) +
  geom_step(size=1) +
  scale_y_continuous(labels=dollar) +
  scale_fill_discrete(NULL) +
  scale_color_discrete(name=NULL,
                       labels=c("\nCompetitive: \nLow income \n",
                                "\nCompetitive: \nNon-low \nincome \n",
                                "\nBasic \nservice \n")) +
  labs(x=NULL,y="State-wide average rate (dollars per kWh)")
ggsave("annual_rates_shortaxis.png",
       path="plots",
       width=6,height=3,units="in")
ggsave("annual_rates_shortaxis_larger.png",
       path="plots",
       width=8,height=4,units="in")
rm(rate_summary_plot,rate_summary_plot_o)

july_points_basic=filter(basic_rates,date=="2019-06-01") %>%
  mutate(date=as.Date("2019-07-01"))

basic_rate_summary_plot=bind_rows(basic_rates,july_points_basic) %>% 
  filter(region!="Nantucket")
rm(july_points_basic)

ggplot(basic_rate_summary_plot,aes(x=date,y=basic_rate,color=factor(region))) +
  geom_step(size=1) +
  scale_y_continuous(labels=dollar) +
  scale_fill_discrete(NULL) +
  scale_color_discrete(name=NULL) +
  labs(x=NULL,y="Basic rate") 
ggsave("annual_basic_rates_shortaxis.png",
       path="plots",
       width=6,height=3,units="in")
rm(basic_rate_summary_plot)

# annual summary of CS net/loss/gain ------------------------
net=monthly_cs_welfare %>% 
  group_by(income) %>%
  summarize(amt_billed=sum(amt_billed,na.rm=T),
            tot_bill_difference=sum(bill_difference,na.rm=T),
            no_accts=sum(no_accts,na.rm=T),
            kwh=sum(kwh,na.rm=T)) %>%
  mutate(avg_monthly_kwh_per_hh=kwh/no_accts,
         avg_loss_per_month=tot_bill_difference/no_accts,
         avg_loss_per_kwh=tot_bill_difference/kwh,
         avg_loss_per_year=avg_loss_per_month*12,
         tag="net")

over=filter(monthly_cs_welfare,bill_difference>0) %>%  
  group_by(income) %>%
  summarize(amt_billed=sum(amt_billed,na.rm=T),
            tot_bill_difference=sum(bill_difference,na.rm=T),
            no_accts=sum(no_accts,na.rm=T),
            kwh=sum(kwh,na.rm=T)) %>%
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

annual_summary=bind_rows(net,under,over)
write_csv(annual_summary,"output/annual_summary.csv")
rm(annual_summary,net,over,under)

# participation relative to basic, agg ------------------------
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
            agg_billed=sum(agg_billed,na.rm=T),
            agg_accts=sum(agg_accts,na.rm=T),
            agg_kwh=sum(agg_kwh,na.rm=T)) %>% 
  ungroup() %>% 
  mutate(pct_cs_accts=no_accts/(no_accts+basic_accts+agg_accts),
         tot_accts=no_accts+basic_accts+agg_accts,
         pct_cs_billed=amt_billed/(amt_billed+basic_billed+agg_billed),
         pct_cs_kwh=kwh/(kwh+basic_kwh+agg_kwh),
         tot_kwh=kwh+basic_kwh+agg_kwh)
write_csv(cs_vs_basic_agg,"output/cs_vs_basic_agg.csv")
rm(cs_vs_basic_agg)

# supplier summary ------------------------
supplier_key=monthly_cs_welfare %>%
  ungroup() %>% 
  mutate(supplier_id=substr(supplier,1,5)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CLRVI","CLEAR",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CNE C","CNE",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="DIRCT","DIREC",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="EDF I","EDF E",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="HCG D","HAMPS",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="NXTER","NEXTE",supplier_id)) %>%
  select(supplier,supplier_id) %>%
  arrange(supplier_id) %>% 
  distinct()
write_csv(supplier_key,"output/supplier_key.csv")

monthly_cs_welfare %>% 
  group_by(supplier) %>% 
  summarize(avg_rate_by_kwh=sum(rate*kwh,na.rm=T)/sum(kwh,na.rm=T),
            avg_basic_by_kwh=sum(basic_rate*kwh,na.rm=T)/sum(kwh,na.rm=T),
            no_accts=sum(no_accts,na.rm=T)) %>% 
  mutate(df=avg_rate_by_kwh-avg_basic_by_kwh) %>% 
  View()

rm(supplier_key)

# suppliers=monthly_cs_welfare %>%
#   mutate(supplier_id=substr(supplier,1,5)) %>%
#   mutate(supplier_id=ifelse(supplier_id=="CLRVI","CLEAR",supplier_id)) %>%
#   mutate(supplier_id=ifelse(supplier_id=="CNE C","CNE",supplier_id)) %>%
#   mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
#   mutate(supplier_id=ifelse(supplier_id=="DIRCT","DIREC",supplier_id)) %>%
#   mutate(supplier_id=ifelse(supplier_id=="EDF I","EDF E",supplier_id)) %>%
#   mutate(supplier_id=ifelse(supplier_id=="HCG D","HAMPS",supplier_id)) %>%
#   mutate(supplier_id=ifelse(supplier_id=="NXTER","NEXTE",supplier_id)) %>%
#   group_by(supplier_id,income) %>%
#   summarize(no_months=length(unique(date)),
#             avg_rate_by_accts=sum(rate*no_accts,na.rm=T)/sum(no_accts,na.rm=T),
#             avg_basic_by_accts=sum(basic_rate*no_accts,na.rm=T)/sum(no_accts,na.rm=T),
#             avg_rate_by_kwh=sum(rate*kwh,na.rm=T)/sum(kwh,na.rm=T),
#             avg_basic_by_kwh=sum(basic_rate*kwh,na.rm=T)/sum(kwh,na.rm=T),
#             no_accts=sum(no_accts,na.rm=T),
#             kwh=sum(kwh,na.rm=T),
#             net_loss=sum(bill_difference,na.rm=T),
#             sum_overchargebills=sum(bill_difference[which(bill_difference>0)]),
#             sum_underchargebills=sum(bill_difference[which(bill_difference<0)]))%>%
#   ungroup() %>%
#   mutate(avg_premium_by_accts=net_loss/no_accts,
#          avg_premium_by_kwh=net_loss/kwh) %>% 
#   group_by(income) %>%
#   mutate(share_of_accts=no_accts/sum(no_accts),
#          share_of_kwh=kwh/sum(kwh),
#          share_of_overcharge=sum_overchargebills/sum(sum_overchargebills),
#          share_of_undercharge=sum_underchargebills/sum(sum_underchargebills)) %>% 
#   unique()
# write_csv(suppliers,"output/supplier_summary.csv")
# 
# suppliers_subset=suppliers %>% 
#   group_by(supplier_id) %>% 
#   filter(income=="all") %>% 
#   mutate(all_months=no_months[which(income=="all")]) %>% 
#   filter(no_accts>=100,
#          all_months==12)
# write_csv(suppliers_subset,"output/supplier_summary_subset.csv")

suppliers=monthly_cs_welfare %>%
  mutate(supplier_id=substr(supplier,1,5)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CLRVI","CLEAR",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CNE C","CNE",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="DIRCT","DIREC",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="EDF I","EDF E",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="HCG D","HAMPS",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="NXTER","NEXTE",supplier_id)) %>%
  group_by(supplier_id,income) %>%
  summarize(no_months=length(unique(date)),
            first_month=min(date),
            last_month=max(date),
            avg_rate_by_accts=sum(rate*no_accts,na.rm=T)/sum(no_accts,na.rm=T),
            avg_basic_by_accts=sum(basic_rate*no_accts,na.rm=T)/sum(no_accts,na.rm=T),
            avg_rate_by_kwh=sum(rate*kwh,na.rm=T)/sum(kwh,na.rm=T),
            avg_basic_by_kwh=sum(basic_rate*kwh,na.rm=T)/sum(kwh,na.rm=T),
            no_accts=sum(no_accts,na.rm=T),
            kwh=sum(kwh,na.rm=T),
            net_loss=sum(bill_difference,na.rm=T),
            sum_overchargebills=sum(bill_difference[which(bill_difference>0)]),
            sum_underchargebills=sum(bill_difference[which(bill_difference<0)]))%>%
  ungroup() %>%
  mutate(avg_premium_by_accts=net_loss/no_accts,
         avg_premium_by_kwh=net_loss/kwh) %>% 
  group_by(income) %>%
  mutate(share_of_accts=no_accts/sum(no_accts),
         share_of_kwh=kwh/sum(kwh),
         share_of_overcharge=sum_overchargebills/sum(sum_overchargebills),
         share_of_undercharge=sum_underchargebills/sum(sum_underchargebills))
write_csv(suppliers,"output/supplier_summary.csv")

rm(suppliers)

# rates by top three suppliers ------------------------
top3suppliers=monthly_cs_welfare %>%
  mutate(supplier_id=substr(supplier,1,5)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  filter(supplier_id %in% c("NRG R","DIREC","CONST"),
         kwh>0,
         rate>0,
         income=="all") %>% 
  mutate(markup_bin=cut(rate_difference,c(-Inf,seq(-.02,.09,.01),Inf))) %>% 
  group_by(markup_bin,supplier_id) %>%
  summarize(no_accts=sum(no_accts)) 

all_levels=expand.grid(supplier_id=unique(top3suppliers$supplier_id),
                       markup_bin=unique(top3suppliers$markup_bin))
top3suppliers=left_join(all_levels,top3suppliers)
rm(all_levels)

ggplot(filter(top3suppliers),
       aes(x=markup_bin,y=no_accts,fill=supplier_id))+
  geom_bar(stat="identity",position="dodge")+
  scale_x_discrete(labels=c("< -2","(-2 - 1]","(-1 - 0]","(0 - 1]","(1 - 2]","(2 - 3]",
                            "(3 - 4]","(4 - 5]","(5 - 6]","(6 - 7]","(7 - 8]","(8 - 9]","> 9"))+
  scale_fill_discrete(name=NULL,
                      labels=c("Supplier 1","Supplier 2","Supplier 3"))+
  labs(x="Competitive markup in cents ($0.01)",
       y="Number of accounts")+
  theme(legend.position="bottom")
ggsave("top_3_suppliers_rates.png",
       path="plots",
       width=6,height=4,units="in")
rm(top3suppliers)


# ---- zipcode files --------

all_files=list.files("input/")
zipcode_cs_files=grep("^\\w+_q[4,5]_cs.xlsx",all_files,value=T)
zipcode_basic_files=grep("^\\w+_q[4,5]_basic.xlsx",all_files,value=T)
zipcode_agg_files=grep("^\\w+_q[4,5]_agg.xlsx",all_files,value=T)

clean_muni_names=function(df){
  df %>% 
    mutate(municipality=str_to_title(municipality)) %>% 
    left_join(fread("clean_muni_names.csv")) %>% 
    mutate(municipality=ifelse(!is.na(clean_muni),clean_muni,municipality)) %>% 
    select(-clean_muni)
}

# census xwalk
census_english=read_csv("census_data/census_english.csv",na="-") %>%
  mutate(zip=clean.zipcodes(zip)) %>% 
  rename(zcta=zip)
census_race=read_csv("census_data/census_race.csv") %>%
  mutate(zip=clean.zipcodes(zip)) %>% 
  rename(zcta=zip)
census_poverty=read_csv("census_data/census_poverty.csv",na=c("-","(X)")) %>%
  mutate(zip=clean.zipcodes(zip)) %>% 
  rename(zcta=zip)
census_income=read_csv("census_data/census_income.csv",na=c("-","(X)")) %>%
  mutate(zip=clean.zipcodes(zip)) %>% 
  rename(zcta=zip)
census_disability=read_csv("census_data/census_disability.csv",na=c("-","(X)")) %>%
  mutate(zip=clean.zipcodes(zip)) %>% 
  rename(zcta=zip)
census_age=read_csv("census_data/census_age.csv",na=c("-","(X)")) %>%
  mutate(zip=clean.zipcodes(zip)) %>% 
  rename(zcta=zip)

xwalk=read_excel("census_data/zip_to_zcta.xlsx") %>%
  mutate(zip=clean.zipcodes(zip),
         zcta=clean.zipcodes(zcta))

suppliers_by_zip=lapply(zipcode_cs_files,function(x){
  read_excel(paste0("input/",x)) %>%
    select(-date) %>% 
    mutate(zip=clean.zipcodes(zip),
           supplier_id=substr(supplier,1,5)) %>% 
    clean_muni_names()
}) %>%
  bind_rows()  %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CLRVI","CLEAR",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CNE C","CNE",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="DIRCT","DIREC",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="EDF I","EDF E",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="HCG D","HAMPS",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="NXTER","NEXTE",supplier_id)) %>%
  filter(income=="all") %>% 
  group_by(zip,municipality,region,supplier_id) %>%
  summarize(suppliers=1) %>% 
  group_by(zip,municipality,region) %>%
  summarize(suppliers=n()) 

# ---- zipcode analysis --------

basic_rates=read_excel("basic_rates.xlsx")

zipcode_basic=lapply(zipcode_basic_files,function(x){
  read_excel(paste0("input/",x)) %>%  
    select(-date) %>%
    mutate(zip=clean.zipcodes(zip)) %>% 
    clean_muni_names() %>%
    rename(no_basic_accts=no_accts) 
}) %>%
  bind_rows() 

zipcode_agg=lapply(zipcode_agg_files,function(x){
  read_excel(paste0("input/",x)) %>% 
    select(-date) %>% 
    mutate(zip=clean.zipcodes(zip)) %>% 
    clean_muni_names() %>%
    group_by(zip,municipality,region,income) %>%
    summarize(no_agg_accts=sum(no_accts)) 
}) %>%
  bind_rows() 

zipcode_cs=lapply(zipcode_cs_files,function(x){
  read_excel(paste0("input/",x)) %>% 
    select(-date) %>%
    mutate(zip=clean.zipcodes(zip)) %>% 
    clean_muni_names()
}) %>%
  bind_rows() 

# need: fitchburg use by supplier/rate/income/zip for jun18 - was not provided. approx usage with supplier/rate/month/income data
attach=monthly_cs_welfare %>% 
  filter(region=="Fitchburg",
         date=="2018-09-01") %>% 
  select(rate,supplier,income,kwh_per_acct)

zipcode_cs_ok=zipcode_cs %>% 
  filter(region!="Fitchburg")
zipcode_cs_fix=zipcode_cs %>% 
  filter(region=="Fitchburg") %>% 
  select(-kwh) %>% 
  left_join(attach) %>% 
  mutate(kwh=no_accts*kwh_per_acct) %>% 
  select(-kwh_per_acct)

zipcode_cs=bind_rows(zipcode_cs_ok,zipcode_cs_fix) %>% 
  mutate(amt_billed=kwh*rate) %>% 
  group_by(zip,municipality,region,income) %>%
  summarize(no_cs_accts=sum(no_accts,na.rm=T),
            new_cs_accts=sum(new_accts,na.rm=T),
            cs_kwh=sum(kwh,na.rm=T),
            cs_amt_billed=sum(amt_billed,na.rm=T)) %>% 
  mutate(avg_cs_rate=ifelse(cs_kwh!=0,cs_amt_billed/cs_kwh,NA)) 

# There are a FEW zips with >0 accts but no assigned usage

rm(zipcode_cs_ok,zipcode_cs_fix)

zips=full_join(zipcode_basic,zipcode_cs,by=c("zip","municipality","region","income")) %>%
  full_join(zipcode_agg,by=c("zip","municipality","region","income")) %>%
  left_join(filter(basic_rates,date==max(basic_rates$date)), by = "region") %>% 
  select(-date) 

zips[is.na(zips$no_basic_accts),"no_basic_accts"]=0
zips[is.na(zips$no_cs_accts),"no_cs_accts"]=0
zips[is.na(zips$new_cs_accts),"new_cs_accts"]=0
zips[is.na(zips$no_agg_accts),"no_agg_accts"]=0

zips=zips %>%
  mutate(rate_dif=avg_cs_rate-basic_rate,
         tot_accts=no_basic_accts+no_cs_accts+no_agg_accts) 

zips=recast(zips,zip+municipality+region~income+variable) %>%
  rename(basic_rate=all_basic_rate) %>%
  mutate(low_basic_rate=NULL)

zips[is.na(zips$low_tot_accts),"low_tot_accts"]=0
zips[is.na(zips$low_no_basic_accts),"low_no_basic_accts"]=0
zips[is.na(zips$low_no_cs_accts),"low_no_cs_accts"]=0
zips[is.na(zips$low_new_cs_accts),"low_new_cs_accts"]=0
zips[is.na(zips$low_no_agg_accts),"low_no_agg_accts"]=0

zips=zips %>%
  ungroup() %>% 
  mutate(other_cs_kwh=all_cs_kwh-low_cs_kwh,
         other_cs_amt_billed=all_cs_amt_billed-low_cs_amt_billed,
         other_tot_accts=all_tot_accts-low_tot_accts,
         other_no_cs_accts=all_no_cs_accts-low_no_cs_accts,
         other_no_agg_accts=all_no_agg_accts-low_no_agg_accts,
         other_no_basic_accts=all_no_basic_accts-low_no_basic_accts,
         other_avg_cs_rate=ifelse(other_cs_kwh>0,other_cs_amt_billed/other_cs_kwh,NA),
         other_rate_dif=other_avg_cs_rate-basic_rate,
         all_pct_cs=all_no_cs_accts/all_tot_accts,
         low_pct_cs=ifelse(low_tot_accts>0,
                           low_no_cs_accts/low_tot_accts,
                           NA),
         other_pct_cs=ifelse(other_tot_accts>0,
                             other_no_cs_accts/other_tot_accts,
                             NA),
         agg_present=ifelse(all_no_agg_accts>0,1,0),
         pct_low_accts=low_tot_accts/all_tot_accts) %>%
  mutate(other_rate_dif=ifelse(other_rate_dif<0,0,other_rate_dif)) %>% 
  left_join(suppliers_by_zip,by = c("zip","municipality","region")) %>% 
  left_join(xwalk) %>%
  left_join(census_english) %>%
  left_join(census_race) %>%
  left_join(census_income) %>%
  left_join(census_poverty) %>%
  left_join(census_age) %>%
  left_join(census_disability)
write_csv(zips,"output/master_zipcode.csv")



# ---- scatter plot ----


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
fwrite(plotdata,file="plots/scatter_plot.csv")

library(ggrepel)

theme_set(theme_gray(base_size = 50))
png(filename="plots/scatter.png",
    width=1.35*1250,
    height=1*1250)
ggplot(data=plotdata,aes(x=pct_low_accts,y=all_pct_cs,label=zip,color=municipality))+
  geom_text_repel(size=12,fontface="bold")+
  scale_x_continuous(labels = percent_format(accuracy = 2))+
  scale_y_continuous(labels = percent_format(accuracy = 2))+
  scale_color_discrete(name="Municipality",
                       labels=c("Boston","Springfield","Worcester"))+
  labs(x="Share of low income customers",y="Participation in the competitive supply market")+
  theme(legend.position = c(.13,.85),
        legend.margin=margin(2,1,1,1,"line"),
        legend.key.height=unit(4,"line"))
dev.off()
theme_set(theme_gray(base_size = 11))


# ---- summaries by top 20 ----
# rest_of_state_in_one_line=zips %>%
#   filter(all_tot_accts>9) %>%
#   select(zip,municipality,
#          all_tot_accts,all_avg_rate_dif,all_pct_cs,all_no_cs_accts,
#          low_tot_accts,low_avg_rate_dif,low_pct_cs,low_no_cs_accts,
#          other_tot_accts,other_avg_rate_dif,other_pct_cs,other_no_cs_accts) %>%
#   summarize(zip="-",
#             municipality="Rest of State",
#             all_tot_accts=sum(all_tot_accts),
#             all_avg_rate_dif=sum(all_avg_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
#             all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
#             pct_low_accts=sum(low_tot_accts)/all_tot_accts,
#             low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
#             other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts))

maj_min=zips %>%
  filter(pct_nw_or_h>.5,
         all_tot_accts>9) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,pct_nw_or_h,
         all_tot_accts,all_rate_dif,all_pct_cs,
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
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts,na.rm=T)/sum(other_tot_accts,na.rm=T))
rest_of_state=zips %>%
  filter(!(is.na(pct_nw_or_h)), # 46 rows missing pct_nw_or_h
         pct_nw_or_h<=.5,
         all_tot_accts>9) %>%
  summarize(zip="-",
            municipality="Rest of State",
            pct_nw_or_h=sum(pct_nw_or_h*all_tot_accts,na.rm=T)/sum(all_tot_accts),
            all_tot_accts=sum(all_tot_accts),
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts,na.rm=T)/sum(other_tot_accts,na.rm=T),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
maj_min=bind_rows(summary,rest_of_state,maj_min) %>% 
  mutate(other_pct_cs=ifelse(other_pct_cs<0,0,other_pct_cs))
write_csv(maj_min,"output/majority_minority.csv")

top_20_income=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(desc(hh_inc)) %>%
  head(.,n=20) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,hh_inc,
         all_tot_accts,all_rate_dif,all_pct_cs,
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
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
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
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
top_20_income=bind_rows(summary,rest_of_state,top_20_income)
write_csv(top_20_income,"output/top_20_income.csv")

bottom_20_income=zips %>%
  filter(all_tot_accts>9) %>%
  arrange(hh_inc) %>%
  head(.,n=20) %>%
  mutate(pct_low_accts=low_tot_accts/all_tot_accts) %>%
  select(zip,municipality,hh_inc,
         all_tot_accts,all_rate_dif,all_pct_cs,
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
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
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
            all_rate_dif=sum(all_rate_dif*all_no_cs_accts,na.rm=T)/sum(all_no_cs_accts),
            all_pct_cs=sum(all_no_cs_accts)/all_tot_accts,
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts)/sum(other_tot_accts),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
bottom_20_income=bind_rows(summary,rest_of_state,bottom_20_income)
write_csv(bottom_20_income,"output/bottom_20_income.csv")

# agg participation

muni_agg=muni %>%
  group_by(agg_present) %>% 
  summarize(all_tot=sum(all_tot_accts,na.rm=T),
            all_cs=sum(all_no_cs_accts,na.rm=T),
            all_agg=sum(all_no_agg_accts,na.rm = T),
            avg_rate_dif=sum(all_cs_kwh*all_rate_dif,na.rm=T)/sum(all_cs_kwh,na.rm=T),
            n_munis=n_distinct(municipality)) %>% 
  mutate(cs_participation=all_cs/all_tot)
write_csv(muni_agg,"output/muni_agg.csv")

# muni level grouping across EDCS, with suppliers

all_files=list.files("input/")
zipcode_cs_files=grep("^\\w+_q[4,5]_cs.xlsx",all_files,value=T)
zipcode_basic_files=grep("^\\w+_q[4,5]_basic.xlsx",all_files,value=T)
zipcode_agg_files=grep("^\\w+_q[4,5]_agg.xlsx",all_files,value=T)

basic_rates=read_excel("basic_rates.xlsx")

muni_basic=lapply(zipcode_basic_files,function(x){
  read_excel(paste0("input/",x)) %>%  
    select(-date) %>%
    mutate(zip=clean.zipcodes(zip)) %>%
    clean_muni_names() %>%
    rename(no_basic_accts=no_accts) 
}) %>%
  bind_rows() %>%
  group_by(municipality,income) %>% 
  summarize(no_basic_accts=sum(no_basic_accts,na.rm=T))

muni_agg=lapply(zipcode_agg_files,function(x){
  read_excel(paste0("input/",x)) %>% 
    select(-date) %>% 
    mutate(zip=clean.zipcodes(zip)) %>% 
    clean_muni_names() 
}) %>%
  bind_rows() %>%
  group_by(municipality,income) %>%
  summarize(no_agg_accts=sum(no_accts,na.rm=T)) 

muni_cs=lapply(zipcode_cs_files,function(x){
  read_excel(paste0("input/",x)) %>% 
    select(-date) %>%
    mutate(zip=clean.zipcodes(zip)) %>% 
    clean_muni_names()
}) %>%
  bind_rows() %>%  
  left_join(filter(basic_rates,date==max(basic_rates$date)), by = "region") %>% 
  select(-date) %>% 
  mutate(supplier_id=substr(supplier,1,5)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CLRVI","CLEAR",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CNE C","CNE",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="DIRCT","DIREC",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="EDF I","EDF E",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="HCG D","HAMPS",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="NXTER","NEXTE",supplier_id)) 

# need: fitchburg use by supplier/rate/income/zip for jun18 - was not provided. approx usage with supplier/rate/month/income data
attach=monthly_cs_welfare %>% 
  filter(region=="Fitchburg",
         date=="2018-09-01") %>% 
  select(rate,supplier,income,kwh_per_acct)

muni_cs_ok=muni_cs %>% 
  filter(region!="Fitchburg")
muni_cs_fix=muni_cs %>% 
  filter(region=="Fitchburg") %>% 
  select(-kwh) %>% 
  left_join(attach) %>% 
  mutate(kwh=no_accts*kwh_per_acct) %>% 
  select(-kwh_per_acct)

muni_cs=bind_rows(muni_cs_ok,muni_cs_fix) %>% 
  mutate(amt_billed=kwh*rate) %>% 
  group_by(municipality,income) %>%
  summarize(weighted_basic_rate=sum(basic_rate*kwh,na.rm=T)/sum(kwh,na.rm=T),
            multiple_EDCs=ifelse(n_distinct(region)>1,1,0),
            no_cs_accts=sum(no_accts,na.rm=T),
            new_cs_accts=sum(new_accts,na.rm=T),
            cs_kwh=sum(kwh,na.rm=T),
            cs_amt_billed=sum(amt_billed,na.rm=T),
            suppliers=paste0(sort(unique(supplier_id)),collapse=", ")) %>% 
  mutate(avg_cs_rate=ifelse(cs_kwh!=0,cs_amt_billed/cs_kwh,NA))

rm(muni_cs_ok,muni_cs_fix)

muni=full_join(muni_basic,muni_cs) %>%
  full_join(muni_agg) 

muni[is.na(muni$no_basic_accts),"no_basic_accts"]=0
muni[is.na(muni$no_cs_accts),"no_cs_accts"]=0
muni[is.na(muni$new_cs_accts),"new_cs_accts"]=0
muni[is.na(muni$no_agg_accts),"no_agg_accts"]=0

suppliers_present=muni %>%
  filter(income=="all") %>% 
  select(municipality,suppliers,multiple_EDCs) %>% 
  rename(all_suppliers=suppliers)

muni=muni %>%
  ungroup() %>% 
  mutate(rate_dif=avg_cs_rate-weighted_basic_rate,
         tot_accts=no_basic_accts+no_cs_accts+no_agg_accts) %>% 
  select(-suppliers,-multiple_EDCs)

muni=recast(muni,municipality~income+variable) 

muni[is.na(muni$low_tot_accts),"low_tot_accts"]=0
muni[is.na(muni$low_no_basic_accts),"low_no_basic_accts"]=0
muni[is.na(muni$low_no_cs_accts),"low_no_cs_accts"]=0
muni[is.na(muni$low_new_cs_accts),"low_new_cs_accts"]=0
muni[is.na(muni$low_no_agg_accts),"low_no_agg_accts"]=0

muni=muni %>%
  ungroup() %>% 
  mutate(all_pct_cs=all_no_cs_accts/all_tot_accts,
         low_pct_cs=ifelse(low_tot_accts>0,
                           low_no_cs_accts/low_tot_accts,
                           NA),
         agg_present=ifelse(all_no_agg_accts>0,1,0),
         pct_low_accts=low_tot_accts/all_tot_accts) %>% 
  left_join(suppliers_present)

muni_small=muni %>% 
  mutate(all_cs_loss_over_basic=all_cs_amt_billed-all_weighted_basic_rate*all_cs_kwh,
         all_loss_per_cs_hh=all_cs_loss_over_basic/all_no_cs_accts,
         low_cs_loss_over_basic=low_cs_amt_billed-low_weighted_basic_rate*low_cs_kwh,
         low_loss_per_cs_hh=low_cs_loss_over_basic/low_no_cs_accts)%>% 
  select(municipality,
         all_rate_dif,all_cs_loss_over_basic,
         all_loss_per_cs_hh,all_no_cs_accts,
         all_tot_accts,all_pct_cs,
         low_rate_dif,low_cs_loss_over_basic,
         low_loss_per_cs_hh,low_no_cs_accts,
         low_tot_accts,low_pct_cs,
         pct_low_accts,agg_present,
         multiple_EDCs,
         all_suppliers) 

write_csv(muni_small,"output/muni_level2.csv")




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


# # other ~ inc + nw
# reg=list()
# reg[[1]]=felm(other_pct_cs~log(all_tot_accts)+pct_nw_or_h+log(hh_inc)+
#                 agg_present+suppliers|region|0|municipality,
#               data=filter(zips,other_tot_accts>9))
# # other ~ pct_low + nw
# reg[[2]]=felm(other_pct_cs~log(all_tot_accts)+pct_nw_or_h+pct_low_accts+
#                 agg_present+suppliers|region|0|municipality,
#               data=filter(zips,other_tot_accts>9))
# # low ~ income + nw 
# reg[[3]]=felm(low_pct_cs~log(all_tot_accts)+pct_nw_or_h+log(hh_inc)+
#                 agg_present+suppliers|region|0|municipality,
#               data=filter(zips,low_tot_accts>9))
# # low ~ pct_low + nw
# reg[[4]]=felm(low_pct_cs~log(all_tot_accts)+pct_nw_or_h+pct_low_accts+
#                 agg_present+suppliers|region|0|municipality,
#               data=filter(zips,low_tot_accts>9))
# # all ~ income + nw
# reg[[5]]=felm(all_pct_cs~log(all_tot_accts)+pct_nw_or_h+log(hh_inc)+
#                 agg_present+suppliers|region|0|municipality,
#               data=filter(zips,all_tot_accts>9))
# # all ~ pct_low + nw
# reg[[6]]=felm(all_pct_cs~log(all_tot_accts)+pct_nw_or_h+pct_low_accts+
#                 agg_present+suppliers|region|0|municipality,
#               data=filter(zips,all_tot_accts>9))
# stargazer(reg,type="text")


reg=lm(low_pct_cs~pct_low_accts,
       data=filter(zips,low_tot_accts>9))
summary(reg)

reg=lm(low_pct_cs~log(hh_inc),
       data=filter(zips,low_tot_accts>9))
summary(reg)

reg=lm(all_rate_dif~log(hh_inc),
       data=filter(zips,low_tot_accts>9))
summary(reg)

reg=lm(low_rate_dif~log(hh_inc),
       data=filter(zips,low_tot_accts>9))
summary(reg)

reg=lm(all_rate_dif~pct_low_accts,
       data=filter(zips,low_tot_accts>9))
summary(reg)

reg=lm(low_rate_dif~pct_low_accts,
       data=filter(zips,low_tot_accts>9))
summary(reg)

reg=lm(low_rate_dif~pct_low_accts,
       data=filter(zips,low_tot_accts>9))
summary(reg)

reg=lm(all_rate_dif~pct_nw_or_h+pct_low_accts,
       data=filter(zips,low_tot_accts>9))
summary(reg)


cor.test(filter(zips,low_tot_accts>9)$low_pct_cs,filter(zips,low_tot_accts>9)$pct_low_accts)
cor.test(filter(zips,low_tot_accts>9)$other_pct_cs,filter(zips,low_tot_accts>9)$pct_low_accts)
cor.test(filter(zips,low_tot_accts>9)$all_rate_dif,filter(zips,low_tot_accts>9)$pct_low_accts)
cor.test(filter(zips,low_tot_accts>9)$low_rate_dif,filter(zips,low_tot_accts>9)$pct_low_accts)
cor.test(filter(zips,low_tot_accts>9)$all_rate_dif,log(filter(zips,low_tot_accts>9)$hh_inc))
cor.test(filter(zips,low_tot_accts>9)$low_rate_dif,log(filter(zips,low_tot_accts>9)$hh_inc))
cor.test(filter(zips,low_tot_accts>9)$all_rate_dif,log(filter(zips,low_tot_accts>9)$all_pct_cs))
cor.test(filter(zips,low_tot_accts>9 &!is.na(pct_nw_or_h))$all_pct_cs,log(filter(zips,low_tot_accts>9 &!is.na(pct_nw_or_h))$pct_nw_or_h))
cor.test(filter(zips,low_tot_accts>9)$low_rate_dif,log(filter(zips,low_tot_accts>9)$hh_inc))

  
