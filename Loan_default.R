library(ggplot2)
library(scales)
library(dplyr)
#Customer demographics, Loan variables and Customer behaviors

loan <- read.csv("loan.csv",stringsAsFactors = F)
summary(loan)

#Below columns will not be considered as they
#contains maximum NA values,hence ignoring the below columns

loan$total_il_high_credit_limit <- NULL
loan$total_bc_limit <- NULL
loan$total_bal_ex_mort <- NULL
loan$tot_hi_cred_lim <- NULL
loan$pct_tl_nvr_dlq <- NULL
loan$num_tl_op_past_12m <- NULL
loan$num_tl_90g_dpd_24m <- NULL
loan$num_tl_30dpd <- NULL
loan$num_tl_120dpd_2m <- NULL
loan$num_sats <- NULL
loan$num_rev_tl_bal_gt_0 <- NULL
loan$num_rev_accts <- NULL
loan$num_op_rev_tl <- NULL
loan$num_il_tl <- NULL
loan$num_bc_tl <- NULL
loan$num_bc_sats <- NULL
loan$num_actv_rev_tl <- NULL
loan$num_actv_bc_tl <- NULL
loan$num_accts_ever_120_pd <- NULL
loan$mths_since_recent_revol_delinq <- NULL
loan$mths_since_recent_inq <- NULL
loan$mths_since_recent_bc_dlq <- NULL
loan$mo_sin_rcnt_rev_tl_op <- NULL
loan$mo_sin_old_rev_tl_op <- NULL
loan$mort_acc <- NULL
loan$mo_sin_rcnt_tl <- NULL
loan$mths_since_recent_bc <- NULL
loan$bc_open_to_buy <- NULL
loan$bc_util <- NULL
loan$mo_sin_old_il_acct <- NULL
loan$avg_cur_bal <- NULL
loan$acc_open_past_24mths <- NULL
loan$inq_last_12m <- NULL
loan$total_cu_tl <- NULL
loan$inq_fi <- NULL
loan$total_rev_hi_lim <- NULL
loan$all_util <- NULL
loan$max_bal_bc <- NULL
loan$open_rv_12m <- NULL
loan$il_util <- NULL
loan$total_bal_il <- NULL
loan$mths_since_rcnt_il <- NULL
loan$open_il_24m <- NULL
loan$open_il_12m <- NULL
loan$open_il_6m <- NULL
loan$open_acc_6m <- NULL
loan$tot_cur_bal <- NULL
loan$tot_coll_amt<- NULL
loan$acc_now_delinq<- NULL
loan$verification_status_joint<- NULL
loan$dti_joint<- NULL
loan$annual_inc_joint<- NULL
loan$mths_since_last_major_derog<- NULL
loan$percent_bc_gt_75  <- NULL
loan$open_rv_24m <- NULL
loan$next_pymnt_d<- NULL
loan$id<- NULL
loan$member_id<- NULL

#Below columns contains the same values through out
# Hence are of no use for analysis, hence removing these colums as well

loan$application_type<- NULL
loan$policy_code<- NULL
loan$initial_list_status<- NULL
loan$pymnt_plan<- NULL

#Below set of columns contains either 0 or NA values,
# Hence ignoring these columsn as well

# 0 and NA values
loan$delinq_amnt <- NULL
loan$collections_12_mths_ex_med <- NULL
loan$tax_liens <- NULL
loan$chargeoff_within_12_mths <- NULL
loan$mths_since_last_delinq <- NULL
loan$mths_since_last_record <- NULL
loan$total_pymnt <- NULL


#Below are the Customer demographics variable which will not
# add any value to the analysis, hence ignoring these as well
loan$title<- NULL
loan$desc<- NULL
loan$url<- NULL
loan$emp_title<- NULL
loan$zip_code <- NULL


#Below are basically the loan parameters which will
#not be available at the time of loan application.
#Hence ignoring these as well.
#After loan parameters which we get only after loan is sanctioned

loan$last_credit_pull_d <- NULL
loan$last_pymnt_amnt <- NULL
loan$last_pymnt_d <- NULL
loan$collection_recovery_fee <- NULL
loan$recoveries <- NULL
loan$total_rec_late_fee <- NULL
loan$total_rec_int <- NULL
loan$total_rec_prncp <- NULL
loan$total_pymnt_inv <- NULL
loan$out_prncp <- NULL
loan$out_prncp_inv <- NULL


#Customer behavior attributes to be ignored as they will not be available at the time of
#loan request
loan$delinq_2yrs <- NULL
loan$total_acc<- NULL
loan$pub_rec<- NULL
loan$open_acc<- NULL
loan$inq_last_6mths <- NULL
loan$earliest_cr_line <- NULL



#Cleaning columns data

#emp_length contains + < > special characters, we will remove them
loan$emp_length <- gsub("\\D+", "", loan$emp_length)
factor(loan$emp_length)
#On factor we come to know that there are  n/a(lower case).
sum(is.na(loan$emp_length))
sum(loan$emp_length=="")
#there are around 1K blank values in emp_length which we will equalise with mean of entire emp_lenghth
loan$emp_length[loan$emp_length==""] <- median(loan$emp_length,na.rm = T)


#annual income contains outliers, hence removing the outliers
ann_inc <- boxplot(loan$annual_inc)
loan$annual_inc[(loan$annual_inc %in% ann_inc$out)] <- NA


#int_rate columns have %, this needs to be removed
loan$int_rate <-  gsub("%","",loan$int_rate)
loan$int_rate <- as.numeric(loan$int_rate)

#term column we don't need months string

loan$term <- gsub("months","",loan$term)
class(loan$term)
loan$term <- as.numeric(loan$term)
loan$term <- as.factor(loan$term)


#pub_rec_bankruptcies contains 118 NA values, we will assign 0 to all the NAs

loan$pub_rec_bankruptcies[is.na(loan$pub_rec_bankruptcies)] <- 0


#Checking for any type of missing values in the remaining columns
sum(is.na(loan$loan_amnt))
sum(loan$loan_amnt=="")
sum(loan$term=="")
sum(is.na(loan$term))
sum(is.na(loan$int_rate))
sum(loan$int_rate=="")
sum(is.na(loan$installment))
sum(loan$installment=="")
sum(loan$grade=="")
sum(is.na(loan$grade))
sum(loan$sub_grade=="")
sum(is.na(loan$sub_grade))
sum(loan$home_ownership=="")
sum(is.na(loan$home_ownership))
sum(loan$annual_inc=="")
sum(is.na(loan$annual_inc))
sum(loan$verification_status=="")
sum(is.na(loan$verification_status))
sum(loan$loan_status=="")
sum(is.na(loan$loan_status))
sum(loan$purpose=="")
sum(is.na(loan$purpose))
sum(loan$addr_state=="")
sum(is.na(loan$addr_state))
sum(loan$dti=="")
sum(is.na(loan$dti))
sum(loan$pub_rec_bankruptcies=="")
sum(is.na(loan$pub_rec_bankruptcies))
#All the above columns no missing value.


#Binning loan_amount, annual_income and installments

loan$loan_amnt_bin <- cut(loan$loan_amnt,seq(500,36000,2500))
loan$loan_amnt_bin <- as.numeric(loan$loan_amnt_bin)

loan$annual_inc_bin <- cut(loan$annual_inc,seq(0,150000,10000))
loan$annual_inc_bin <- as.numeric(loan$annual_inc_bin)

loan$installment_bin <- cut(loan$installment,seq(0,1500,150))
loan$installment_bin <- as.numeric(loan$installment_bin)
#Factoring variables

factor(loan$sub_grade)
loan$sub_grade <- as.factor(loan$sub_grade)

factor(loan$home_ownership)
loan$home_ownership <- as.factor(loan$home_ownership)

factor(loan$verification_status)
loan$verification_status <- as.factor(loan$verification_status)

factor(loan$loan_status)
loan$loan_status <- as.factor(loan$loan_status)

factor(loan$purpose)
loan$purpose <- as.factor(loan$purpose)

#The final dataframe to be put on analaysis
loan_chargedoff_fullypaid <- filter(loan, loan_status=="Charged Off" | loan_status=="Fully Paid")

#Univariate analysis

ggplot(loan_chargedoff_fullypaid,aes(x=loan_status,fill=loan_status)) + geom_bar(aes(y=((..count..)/sum(..count..))),position = position_dodge())+
  ggtitle("Loan Status Vs Default/Success Rate ") + labs(x="Loan Status",y="Rate")


ggplot(loan_chargedoff_fullypaid,aes(x=loan_chargedoff_fullypaid$purpose,fill=loan_status)) + geom_bar(aes(y=((..count..)/sum(..count..))),position = position_dodge())+
  ggtitle("Loan Purpose Vs Rate") + labs(x="Loan Purpose",y="Rate")
# Here we get top 5 purposes for which there are max defaults happening
# 1. debt_consolidation 2.credit card 3. small business 4. home improvement 5. major purchase


# Now we will do all kinds of analysis on the above top five purposes
top_5_purposes_defaulting <- c("credit_card","debt_consolidation","home_improvement","major_purchase","small_business")
loan_chargedoff <- filter(loan_chargedoff_fullypaid, purpose %in% top_5_purposes_defaulting)

emp_length_vector <- c("1","2","3","4","5","6","7","8","9","10")
loan_amount_bins <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
loan_amount_bins_label <- c("0-2.5K","2.5k-5k","5k-7.5k","7.5k-10k","10k-12.5k","12.5k-15k","15k-17.5k",
                            "17.5k-20k","20k-22.5k","22.5k-25k","25k-27.5k","27.5k-30k","30k-32.5k","32.5k-35k",
                            ">35k")

annual_inc_bins <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
annual_inc_bins_label <- c("0-10k","10k-20k","20k-30k","30k-40k","40k-50k","50k-60k","60k-70k",
                           "70k-80k","80k-90k","90k-1L","1L-1.1L","1.1L-1.2L","1.2L-1.3L","1.3L-1.4L",">1.5L")
##UniVariate analysis

ggplot(loan_chargedoff, aes(x=as.factor(emp_length),fill=loan_status)) + geom_bar(position = position_dodge())  + facet_wrap(~purpose) + scale_x_discrete(limits=emp_length_vector)+
ggtitle("Employment Length Vs Count") + labs(x="Employment Length",y="Count")
#The trend here we observe is that as the number of years of experience increases defaulting rate decreases and suddenly 
#increases for people of experience arounnd 10 years. Thus the very freshers and very experineced defaults the most. 

ggplot(loan_chargedoff,aes(x=home_ownership,fill=loan_status)) + geom_bar(position = position_dodge())+ facet_wrap(~purpose)+
  ggtitle("Home Ownership Vs Count") + labs(x="Home Ownership",y="Count")
#People whose house is either on rent or mortaged defaults the most.

ggplot(loan_chargedoff,aes(x=term,fill=loan_status)) + geom_bar(position = position_dodge()) + facet_wrap(~purpose)+
  ggtitle("Term Vs Count") + labs(x="Term",y="Count")
#Loans with term of 36 months are defaulting more as conmpared to loans with term 60 months.

ggplot(loan_chargedoff,aes(x=grade,fill=loan_status)) + geom_bar(position = position_dodge()) + facet_wrap(~purpose)+
  ggtitle("Grade Vs Count") + labs(x="Grade",y="Count")
#Highest defailts are in case of Grades A, B and C

ggplot(loan_chargedoff,aes(x=verification_status,fill=loan_status)) + geom_bar(position = position_dodge()) + facet_wrap(~purpose)+
  ggtitle("Verification Status Vs Count") + labs(x="Verification Status",y="Count")
#We don't get have much effect of this parameter as the results are comparable for all.

ggplot(loan_chargedoff,aes(x=as.factor(addr_state),fill=loan_status)) + geom_bar(position = position_dodge()) +theme(
  axis.text.y=element_blank(), axis.ticks=element_blank(),
  axis.title.y=element_blank(),axis.text.x = element_text(angle = 90, hjust = 1)
)+
  ggtitle("Default Frequencies accross states") + labs(x="State",y="Count")


##Segemented Univariate Analysis
ggplot(loan_chargedoff,aes(x=as.factor(loan_amnt_bin),fill=loan_status)) + geom_bar(position = position_dodge()) + facet_wrap(~purpose) + scale_x_discrete(limits=loan_amount_bins,labels=loan_amount_bins_label)+theme(
  axis.text.y=element_blank(), axis.ticks=element_blank(),
  axis.title.y=element_blank(),axis.text.x = element_text(angle = 90, hjust = 1)
)+
  ggtitle("Loan Amounts vs Count") + labs(x="Loan Amount",y="Count")
#Loan amount between 5000-10000 has the highest default rate

ggplot(loan_chargedoff,aes(x=as.factor((annual_inc_bin)),fill=loan_status)) + geom_bar(position = position_dodge()) + facet_wrap(~purpose) + scale_x_discrete(limits=annual_inc_bins,labels = annual_inc_bins_label)+theme(
  axis.text.y=element_blank(), axis.ticks=element_blank(),
  axis.title.y=element_blank(),axis.text.x = element_text(angle = 90, hjust = 1)
)+
  ggtitle("Annual Income vs Count") + labs(x="Annual Income",y="Count")

ggplot(loan_chargedoff,aes(x=int_rate,fill=loan_status)) + geom_histogram(position = position_dodge()) + facet_wrap(~purpose)+
  ggtitle("Interest Rate vs Count") + labs(x="Interest Rate",y="Count")
#There is a trend that for each purpose max defaults are between 7-15% interest rate.

ggplot(loan_chargedoff,aes(x=installment,fill=loan_status)) + geom_histogram(position=position_dodge()) + scale_x_continuous(breaks=seq(0, 600000, by=100))+ facet_wrap(~purpose)+
  ggtitle("Installments vs Count") + labs(x="Installments",y="Count")

#Max defauls are between installment between 50-500 and highest between 100-300

ggplot(loan_chargedoff,aes(x=dti,fill=loan_status)) + geom_histogram(position = position_dodge()) + facet_wrap(~purpose)+
  ggtitle("DTI vs Count") + labs(x="DTI",y="Count")

#Bivariate Analysis

ggplot(loan_chargedoff,aes(x=as.factor(loan_amnt_bin),int_rate,fill=loan_status)) + geom_bar(position = position_dodge(),stat = "summary", fun.y = "mean") + scale_x_discrete(limits=loan_amount_bins,labels=loan_amount_bins_label)+theme(
  axis.text.y=element_blank(), axis.ticks=element_blank(),axis.text.x = element_text(angle = 90, hjust = 1)
)+ggtitle("Loan Amount and Interest Rate") + labs(x="Loan Amount",y="Interest Rate")

ggplot(loan_chargedoff,aes(x=as.factor(term),(int_rate),fill=loan_status)) + geom_bar(position = position_dodge(),stat = "summary", fun.y = "mean")+
  ggtitle("Term and Interest Rate") + labs(x="Term",y="Interest Rate")

ggplot(loan_chargedoff,aes(x=as.factor(emp_length),(annual_inc),fill=loan_status)) + geom_bar(position = position_dodge(),stat = "summary", fun.y = "mean")+scale_x_discrete(limits=emp_length_vector)+
  ggtitle("Employment Length and Annual Income") + labs(x="Employment Length",y="Annual Income")



