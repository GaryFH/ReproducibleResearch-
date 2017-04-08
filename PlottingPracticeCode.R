##To practice the plotting techniques you have learned so far, you will be making a graphic that 
##explores relationships between variables. This practice is useful since we will later cover
##creating reproducible graphics in this class. You will be looking at a subset of a United States 
##medical expenditures dataset with information on costs for different medical conditions and in 
##different areas of the country.  You should do the following:  

        ## 1)  Make a plot that answers the question: what is the relationship 
##between mean covered charges (Average.Covered.Charges) and mean total payments (Average.Total.Payments) ##
##in New York? 
        ## 2) Make a plot (possibly multi-panel) that answers the question: how does the 
##relationship between mean covered charges (Average.Covered.Charges) and mean total payments 
##(Average.Total.Payments) vary by medical condition (DRG.Definition) and the state in which 
##care was received (Provider.State)?

## Read in data from working Directory and convert to tbl_df

c1<-read.csv("_e143dff6e844c7af8da2a4e71d7c054d_payments.csv", stringsAsFactors = FALSE)
d1<-tbl_df(c1)

##separate into two dataframes (d2 - payment info & d3 provider info - could combine with provider Id)
d2<-select(d1,Provider.Id,Provider.State,DRG.Definition,Total.Discharges:Average.Medicare.Payments)
d3<-select(d1,Provider.Id: Hospital.Referral.Region.Description)

##separate only NY
d4<-filter(d2,Provider.State=="NY")


q1<-qplot(Average.Covered.Charges/1000,Average.Total.Payments/1000, data = d4,geom = c("point","smooth"))

