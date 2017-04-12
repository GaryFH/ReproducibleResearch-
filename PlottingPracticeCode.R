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

##make a new variable showing Payment receive/cvrd charges
d5<-mutate(d4,Percentage.Paid=Average.Total.Payments/Average.Covered.Charges)
q2<-qplot(Average.Covered.Charges/1000,Percentage.Paid, data = d5,geom = c("point","smooth"))
q22<-qplot(Average.Covered.Charges/1000,Percentage.Paid, data = d5,geom = c("point","smooth"),facets=DRG.Definition~.)
##Make a plot (possibly multi-panel) that answers the question: how does the 
##relationship between mean covered charges (Average.Covered.Charges) and mean total payments 
##(Average.Total.Payments) vary by medical condition (DRG.Definition)
q3<-qplot(Average.Covered.Charges/1000,Average.Total.Payments/1000, data = d4,geom = c("smooth"), color=DRG.Definition)

d35<-mutate(d2,Percentage.Paid=Average.Total.Payments/Average.Covered.Charges)
q35<-qplot(DRG.Definition,Percentage.Paid, data = d35,geom = "boxplot",color=Provider.State)

q4<-qplot(Average.Covered.Charges/1000,Percentage.Paid, data = d5,geom = c("smooth"),color=DRG.Definition)
q41<-qplot(Average.Covered.Charges/1000,Percentage.Paid, data = d35,geom = c("smooth"),color=DRG.Definition,se=FALSE, facets =Provider.State~.)

q5<-qplot(Average.Covered.Charges/1000,Percentage.Paid, data = d5,geom = c("smooth"))

##clrs<-c("red",'blue','yellow','brown','green','orange')


q36<-qplot(Provider.State,Percentage.Paid, data = d35,geom = "boxplot",color=DRG.Definition)
q37<-qplot(Provider.State,Percentage.Paid, data = d35,geom = "boxplot",facets=.~DRG.Definition)

##q41 is best qplot - q37 is second best (boxplot)

##now try ggplot

g<-ggplot(d35,aes(Average.Covered.Charges/1000,Percentage.Paid,color=factor(DRG.Definition)))
q6<-g+geom_smooth(size=1,method = "lm",se=FALSE)+facet_grid(Provider.State~.,margins = TRUE)+labs(title="Percentage Paid by Condition & State")+labs(x="Total amount billed in 1000's dollars",y="Percentage of total billed that was Paid")







##example: g+geom_point(aes(color=drv),size=2,alpha=1/2)+geom_smooth(size=4,linetype=3,
##method = "lm",se=FALSE)+labs(title="Swirl Rules!")+labs(x="Displacement",y="Hwy Mileage")

##  This is step one
##g<-ggplot(mpg,aes(displ,hwy,color=factor(year)))   3 arguments, x set equal to displ, y set equal to hwy,
##and color set equal to factor(year). This last will allow us to distinguish between the two manufacturing 
##years (1999 and 2008) in our data.

##      This is step two
####g+geom_point()

##      This is step three
#### g+geom_point()+facet_grid(drv~cyl,margins = TRUE)
##A 4 by 5 plot, huh? The margins argument tells ggplot to display the marginal totals over 
##each row and column, soinstead of seeing 3 rows (the number of drv factors) and 4 columns 
##(the number of cyl factors) we see a 4 by 5 display. Note that the panel in position 
##(4,5) is a tiny version of the scatterplot of the entire dataset

##      This is step four
##g+geom_point()+facet_grid(drv~cyl,margins = TRUE)+geom_smooth(method = "lm",se=FALSE,size=2,color="black")
##

##      This is step five
##g+geom_point()+facet_grid(drv~cyl,margins = TRUE)+geom_smooth(method = "lm",se=FALSE,size=2,color="black")
        ##+labs(x="Displacement",y="Highway Mileage",title="Swirl Rules!")



##more examples of qplot range(diamonds$price)- returns max and min "[1] 326 18823" range=18497
##      diamonds in dataset that comes with ggplot

##this prints a histogram with 30 bins - default  qplot(price,data = diamonds)
##this allows you to force 30 bins qplot(price,data = diamonds,binwidth=18497/30)

##this adds another dimension qplot(price,data = diamonds,binwidth=18497/30,fill=cut)

##Now we'll replot the histogram as a density function which will show the proportion of diamonds in each bin.
##qplot(price,data = diamonds,geom = "density",color=cut)

##      Let's move on to scatterplots
## swirl asks qplot(carat,price,data = diamonds,shape=cut) and then qplot(carat,price,data = diamonds,color=cut)


                ##SMOOTHING COMMENTS:
##TOO MANY points - this is my idea: qplot(carat,price,data = diamonds,geom=("smooth"),se=FALSE, color=cut,method="loess")
## LOWESS (Locally Weighted Scatterplot Smoothing), sometimes called LOESS (locally weighted smoothing), is a popular tool
        ##used in regression analysis that creates a smooth line through a timeplot or scatter plot to help you to see 
        ##relationship between variables and foresee - method="lm" produces straight line linear regression 
##method="gam" appears to be qplot default  In statistics, a generalized additive model (GAM) is a generalized linear model
##in which the linear predictor depends linearly on unknown smooth functions of some predictor variables, 
##and interest focuses on inference about these smooth functions.  R Smooth terms are specified in a gam formula using 
##s , te , ti and t2 terms. Various smooth classes are available, for different modelling tasks.

##swirl says: qplot(carat,price,data = diamonds,color=cut,facets=.~cut)+geom_smooth(method="lm")
##I say: qplot(carat,price,data = diamonds,geom=c("point","smooth"),se=FALSE, color=cut,method="loess",facets=.~cut)


                ##CUT COMMENTS (QUANTILE NOTES)
        ## R has a handy command, cut, which allows you to divide your data into sets and label each  
        ## entry as belonging to one of the sets, in effect creating a new factor.
##Let's divide the data into 3 pockets, so 1/3 of the data falls into each. We'll use the R command "quantile" to do this

## ex: cutpoints<-quantile(diamonds$carat,seq(0,1,length=4),na.rm=TRUE) note that seq of 4 gives 3 bins with 
                        ##size/values that add up to 1 (percentages) thus splitting the data into 3 equal "bins"
                        ##the printout "cutpoints" shows the ranges of carats in each bin (thus needing 4 values)

## Using the four quantile values as imput for the cut function
##  diamonds$car2<-cut(diamonds$carat,cutpoints) this creats a new variable in the diamond dataset
## reset g to add the new variable  g<-ggplot(diamonds,aes(depth,price))
## now add to g to get desired plot   g+geom_point(alpha=1/3)+facet_grid(cut~car2)  return 4x4 plot grid or
##g+geom_point(alpha=1/3) + facet_grid(cut~car2) + geom_smooth(method = "lm", size=3,color="pink")

##Play with boxplot: ggplot(diamonds,aes(carat,price))+geom_boxplot()+facet_grid(.~cut)











