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
library(lubridate)


# data visuals
library(lfe)
library(ggplot2)
library(scales)
library(stargazer)
library(ggrepel)

setwd("raw_data")

# full year summary ----
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

# fill a gap in Unitil reporting. ----
# Unitil did not provide the kwh in the zip code files, so I use these averages
# to estimate it down below.
FIT_low_avg_kwh_Sept19<-monthly_cs %>% filter(region == "FIT") %>%
  filter(as.Date(date) == "2019-09-01") %>%
  filter(income == "low") %>%
  group_by() %>%
  summarize(avg_kwh_per_acct = sum(kwh)/sum(no_accts))
FIT_low_avg_kwh_Sept19 <- as.integer(FIT_low_avg_kwh_Sept19)

FIT_all_avg_kwh_Sept19<-monthly_cs %>% filter(region == "FIT") %>%
  filter(as.Date(date) == "2019-09-01") %>%
  filter(income == "all") %>%
  group_by() %>%
  summarize(avg_kwh_per_acct = sum(kwh)/sum(no_accts))
FIT_all_avg_kwh_Sept19 <- as.integer(FIT_all_avg_kwh_Sept19)

# Some records have kwh>0, but no accts.  
# Change no_accts from 0 to 1 where it is currently 0 but kwh>0
monthly_cs[which(monthly_cs$no_accts ==0 & monthly_cs$kwh >0),]$no_accts <- 1

# Remove records where kwh = 0 and number of accts = 0
monthly_cs <- monthly_cs %>% filter(kwh>0 & no_accts > 0)

# create monthly_cs_welfare ----
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


plot_dir = "../plots/"

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




# annual summary ----

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

write_csv(bind_rows(net,under,over),"../output/annual_summary.csv")




# cs_vs_basic_agg ----
# estimate participation in competitive supply relative to basic and agg
monthly_basic <- monthly_basic[,c(1,2,3,4,5,6)]
cs_vs_basic_agg=monthly_cs_welfare %>% 
  group_by(region,income,date) %>%
  summarize(amt_billed=sum(amt_billed),
            tot_bill_difference=sum(bill_difference),
            no_accts=sum(no_accts),
            cs_kwh=sum(kwh)) %>%
  left_join(monthly_basic) %>% 
  left_join(monthly_agg) %>% 
  group_by(income) %>% 
  summarize(amt_billed=sum(amt_billed),
            no_accts=sum(no_accts),
            cs_kwh=sum(cs_kwh),
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
         pct_cs_kwh=cs_kwh/(cs_kwh+basic_kwh+agg_kwh),
         tot_kwh=cs_kwh+basic_kwh+agg_kwh)

write_csv(cs_vs_basic_agg,"../output/cs_vs_basic_agg.csv")




# supplier key ----
supplier_key=monthly_cs_welfare %>%
  ungroup() %>% 
  mutate(supplier_id=substr(supplier,1,5)) %>%
  select(supplier,supplier_id) %>%
  arrange(supplier_id) %>% 
  distinct()
write_csv(supplier_key,"../output/supplier_key.csv")

monthly_cs_welfare %>% 
  group_by(supplier) %>% 
  summarize(avg_rate_by_kwh=sum(rate*kwh,na.rm=T)/sum(kwh,na.rm=T),
            avg_basic_by_kwh=sum(basic_rate*kwh,na.rm=T)/sum(kwh,na.rm=T),
            no_accts=sum(no_accts,na.rm=T)) %>% 
  mutate(df=avg_rate_by_kwh-avg_basic_by_kwh) 

rm(supplier_key)

# supplier summary ----
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
write_csv(suppliers,"../output/supplier_summary.csv")




# rates by top three suppliers ----
# Previously the top three supplier were hard-coded as VIRID, CON E, and CONSO.
# This was based on total number of accounts by supplier.
# Now the top three are different.

supplier_totals <- suppliers %>% group_by(supplier_id) %>% summarise(total_accts = sum(no_accts))
top_suppliers_ordered <- supplier_totals %>% arrange(desc(total_accts))
top_three_suppliers <- top_suppliers_ordered[1:3,]
top_three_suppliers

# Now they are NRG R, DIRECT, & CONST.
# VIRID dropped well down, with only 242907 accounts.




# function: clean_muni_names ----
clean_muni_names=function(df){
  df %>% 
    mutate(municipality=str_to_title(municipality)) %>% 
    left_join(fread("clean_muni_names.csv")) %>% 
    mutate(municipality=ifelse(!is.na(clean_muni),clean_muni,municipality)) %>% 
    select(-clean_muni)
}

# function: clean_supplier_names ----
clean_supplier_names=function(df){
  df %>% 
    left_join(fread("clean_supplier_names.csv")) %>% 
    mutate(supplier=ifelse(!is.na(corrected_supplier),corrected_supplier,supplier)) %>% 
    select(-corrected_supplier)
}



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
  clean_muni_names() %>%
  group_by(zip,municipality,region,income) %>%
  summarize(no_basic_accts = sum(no_basic_accts)) %>%
  #select(-date) %>%
  clean_muni_names()

summary(zipcode_basic)

zipcode_agg=lapply(zipcode_agg_files,function(x){
  return(read_excel(x,col_types = c("date","text","text","text","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=zip,
         municipality=str_to_title(municipality)) %>%
  clean_muni_names() %>%
  group_by(zip,municipality,region,income) %>%
  summarize(no_agg_accts=sum(no_accts)) 

summary(zipcode_agg)


# TH added a col for kwh, and changed calculation of avg_cs_rate to use kwh for weighting, rather than no_accts
zipcode_cs=lapply(zipcode_cs_files,function(x){
  return(read_excel(x,col_types = c("date","numeric","text","text","numeric","numeric","numeric","numeric","text","text")))
}) %>%
  bind_rows() %>%
  mutate(zip=paste0("0",zip),
         municipality=str_to_title(municipality)) %>%
  clean_muni_names() %>%
  group_by(zip,municipality,region,income) %>%
  summarize(avg_cs_rate=sum(rate*kwh)/sum(kwh),
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

# debug
# write_csv(zips, "../output/zips1.csv")


zips=recast(zips,zip+municipality+region~income+variable, sum) %>%
  rename(basic_rate=all_basic_rate) %>%
  mutate(low_basic_rate=NULL)

zips[is.na(zips$low_tot_accts),"low_tot_accts"]=0
zips[is.na(zips$low_no_basic_accts),"low_no_basic_accts"]=0
zips[is.na(zips$low_no_cs_accts),"low_no_cs_accts"]=0
zips[is.na(zips$low_new_cs_accts),"low_new_cs_accts"]=0
zips[is.na(zips$low_no_agg_accts),"low_no_agg_accts"]=0

# write_csv(zips, "../output/zips2.csv")



zips=zips %>%
  group_by(zip,municipality,region) %>%
  summarize(basic_rate=sum(basic_rate*all_tot_accts)/sum(all_tot_accts),
            all_avg_cs_rate=sum(all_avg_cs_rate*all_no_cs_accts)/sum(all_no_cs_accts),
            all_rate_dif=ifelse(sum(all_no_cs_accts)>0,
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
write_csv(zips,"../output/master_zipcode.csv")




# make plot of participation rate versus low-income accounts ----
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


# large for printing
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


# ---- summaries by top 20 ----
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
            all_pct_cs=sum(all_no_cs_accts)/sum(all_tot_accts),
            low_pct_cs=sum(low_no_cs_accts)/sum(low_tot_accts),
            other_pct_cs=sum(other_no_cs_accts,na.rm=T)/sum(other_tot_accts,na.rm=T),
            pct_low_accts=sum(low_tot_accts)/all_tot_accts)
maj_min=bind_rows(summary,rest_of_state,maj_min) %>% 
  mutate(other_pct_cs=ifelse(other_pct_cs<0,0,other_pct_cs))
write_csv(maj_min,"../output/majority_minority.csv")



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
write_csv(top_20_income,"../output/top_20_income.csv")


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
write_csv(bottom_20_income,"../output/bottom_20_income.csv")



# muni_level2 -  grouping across EDCS, with suppliers ----

zipcode_cs_files=grep("^\\w+_q[4,5]_cs.xlsx",all_files,value=T)
zipcode_basic_files=grep("^\\w+_q[4,5]_basic.xlsx",all_files,value=T)
zipcode_agg_files=grep("^\\w+_q[4,5]_agg.xlsx",all_files,value=T)

basic_rates=read_excel("Basic_Rates_TH.xlsx")

muni_basic=lapply(zipcode_basic_files,function(x){
  read_excel(paste0(x)) %>%  
    select(-date) %>%
    clean_muni_names() %>%
    rename(no_basic_accts=no_accts) 
}) %>%
  bind_rows() %>%
  group_by(municipality,income) %>% 
  summarize(no_basic_accts=sum(no_basic_accts,na.rm=T))

muni_agg=lapply(zipcode_agg_files,function(x){
  read_excel(paste0(x)) %>% 
    select(-date) %>% 
    clean_muni_names() 
}) %>%
  bind_rows() %>%
  group_by(municipality,income) %>%
  summarize(no_agg_accts=sum(no_accts,na.rm=T)) 

muni_cs=lapply(zipcode_cs_files,function(x){
  read_excel(paste0(x)) %>% 
    select(-date) %>%
    clean_muni_names()
}) %>%
  bind_rows() %>%  
  left_join(filter(basic_rates,date==max(basic_rates$date)), by = "region") %>% 
  select(-date) %>% 
  clean_supplier_names() %>%
  mutate(supplier_id=substr(supplier,1,5)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CLRVI","CLEAR",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CNE C","CNE",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="CON E","CONSO",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="DIRCT","DIREC",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="EDF I","EDF E",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="HCG D","HAMPS",supplier_id)) %>%
  mutate(supplier_id=ifelse(supplier_id=="NXTER","NEXTE",supplier_id))



#  fill in gaps in kwh from calculations above ----
muni_cs[which(is.na(muni_cs$kwh) == T & 
                muni_cs$income == "low"),]$kwh <- 
  muni_cs[which(is.na(muni_cs$kwh) == T & 
                  muni_cs$income == "low"),]$no_accts*FIT_low_avg_kwh_Sept19

muni_cs[which(is.na(muni_cs$kwh) == T & 
                muni_cs$income == "all"),]$kwh <- 
  muni_cs[which(is.na(muni_cs$kwh) == T & 
                  muni_cs$income == "all"),]$no_accts*FIT_all_avg_kwh_Sept19

muni_cs_2=muni_cs %>% 
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



muni=full_join(muni_basic,muni_cs_2) %>%
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

write_csv(muni_small,"../output/muni_level2.csv")



# correlations ----
# used this one 1/29/2021
cor.test(filter(zips,low_tot_accts>9)$all_pct_cs,filter(zips,low_tot_accts>9)$pct_low_accts)

cor.test(filter(zips,low_tot_accts>9)$all_rate_dif,filter(zips,low_tot_accts>9)$pct_low_accts)


