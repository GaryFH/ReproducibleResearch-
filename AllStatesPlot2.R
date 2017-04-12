##To practice the plotting techniques you have learned so far, you will be making a graphic that 
##explores relationships between variables. This practice is useful since we will later cover
##creating reproducible graphics in this class. You will be looking at a subset of a United States 
##medical expenditures dataset with information on costs for different medical conditions and in 
##different areas of the country.  You should do the following:  

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


##Make a plot (possibly multi-panel) that answers the question: how does the 
##relationship between mean covered charges (Average.Covered.Charges) and mean total payments 
##(Average.Total.Payments) vary by medical condition (DRG.Definition)
q3<-qplot(Average.Covered.Charges/1000,Average.Total.Payments/1000, data = d4,geom = c("smooth"), color=DRG.Definition)

d35<-mutate(d2,Percentage.Paid=Average.Total.Payments/Average.Covered.Charges)
q35<-qplot(DRG.Definition,Percentage.Paid, data = d35,geom = "boxplot",color=Provider.State)

q4<-qplot(Average.Covered.Charges/1000,Percentage.Paid, data = d5,geom = c("smooth"),color=DRG.Definition)
q41<-qplot(Average.Covered.Charges/1000,Percentage.Paid, data = d35,geom = c("smooth"),color=DRG.Definition,se=FALSE, facets =Provider.State~.)

q5<-qplot(Average.Covered.Charges/1000,Percentage.Paid, data = d5,geom = c("smooth"))

q36<-qplot(Provider.State,Percentage.Paid, data = d35,geom = "boxplot",color=DRG.Definition)
q37<-qplot(Provider.State,Percentage.Paid, data = d35,geom = "boxplot",facets=.~DRG.Definition)



##now try ggplot

g<-ggplot(d35,aes(Average.Covered.Charges/1000,Percentage.Paid,color=factor(DRG.Definition)))
q6<-g+geom_smooth(size=1,method = "lm",se=FALSE)+facet_grid(Provider.State~.,margins = TRUE)+labs(title="Percentage Paid by Condition & State")+labs(x="Total amount billed in 1000's dollars",y="Percentage of total billed that was Paid")

##      q41 is best 
