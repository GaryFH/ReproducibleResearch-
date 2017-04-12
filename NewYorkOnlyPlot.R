##To practice the plotting techniques you have learned so far, you will be making a graphic that 
##explores relationships between variables. This practice is useful since we will later cover
##creating reproducible graphics in this class. You will be looking at a subset of a United States 
##medical expenditures dataset with information on costs for different medical conditions and in 
##different areas of the country.  You should do the following:  

## 1)  Make a plot that answers the question: what is the relationship 
##between mean covered charges (Average.Covered.Charges) and mean total payments (Average.Total.Payments) ##
##in New York? 

## Read in data from working Directory and convert to tbl_df

c1<-read.csv("_e143dff6e844c7af8da2a4e71d7c054d_payments.csv", stringsAsFactors = FALSE)
d1<-tbl_df(c1)

##separate into two dataframes (d2 - payment info & d3 provider info - could combine with provider Id)
d2<-select(d1,Provider.Id,Provider.State,DRG.Definition,Total.Discharges:Average.Medicare.Payments)
d3<-select(d1,Provider.Id: Hospital.Referral.Region.Description)

##separate only NY
d4<-filter(d2,Provider.State=="NY")


q1<-qplot(Average.Covered.Charges/1000,Average.Total.Payments/1000, data = d4,geom = c("point","smooth"))
##q1 is not very interesting since it seems obvious that payments will increase 
## as the amount billed increases - therefore I decided to see if the percentage
##paid changes with the amount billed.


##make a new variable showing Payment receive/cvrd charges
d5<-mutate(d4,Percentage.Paid=Average.Total.Payments/Average.Covered.Charges)
q2<-qplot(Average.Covered.Charges/1000,Percentage.Paid, data = d5,geom = c("point","smooth"))

## q2 looks a lot more interesting - the percentage paid goes down at first as the 
## amount billed increases - it levels off.   
