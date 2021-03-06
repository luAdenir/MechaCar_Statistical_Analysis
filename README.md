# MechaCar_Statistical_Analysis


## Linear Regression to Predict MPG

<img width="655" alt="mechaCar_lm" src="https://user-images.githubusercontent.com/71112826/105279571-ae0ae300-5b5c-11eb-93a8-8ee9ef8c77e2.png">




<img width="513" alt="mechaCar_lm_summary" src="https://user-images.githubusercontent.com/71112826/105279748-18bc1e80-5b5d-11eb-9408-d1227f38d477.png">


After performing linear regression to predict MPG of MechaCar, the results above shows that Intercept, vehicle length, and ground clearance provided a non-random values of variance to the mpg values in the dataset. It means Vehicle Length(6.267) and Ground clearance (3.546) at “0” level of significance, have significant impact on the mechaCar prototype mpg. 

The slope on this linear model is not zero. Because some on the independent variables has a significant effect on the dependent variable, mpg. 

The R-squared value shows that this linear model is 71% fit and effective to predict mpg of MechaCar prototype. 



## Summary Statistics on Suspension Coils

#### Total Summary
<img width="290" alt="total_summary" src="https://user-images.githubusercontent.com/71112826/105279905-73ee1100-5b5d-11eb-8cee-d89f45757c28.png">




#### Lot Summary
<img width="441" alt="lot_summary" src="https://user-images.githubusercontent.com/71112826/105279910-781a2e80-5b5d-11eb-84ed-bf58213c5e0d.png">


The design specifications for the MechaCar suspension coils dictate that the variance of the suspension coils must not exceed 100 pounds per square inch. Base on the table above it shows that Lot1 with 0.98 and Lot2 with 7.47 Variance, meet the suspension coils design specification. On the other hand, Lot3 exceeds with 170.29 PSI. 


## T-Tests on Suspension Coils

<img width="421" alt="tTest_manufacturingLot" src="https://user-images.githubusercontent.com/71112826/105281674-85d1b300-5b61-11eb-923c-ab5ac34eb3ba.png">



#### Lot 1

<img width="526" alt="Lot1" src="https://user-images.githubusercontent.com/71112826/105457739-25637400-5c3c-11eb-8b0d-2937a33d00b1.png">



#### Lot 2

<img width="506" alt="Lot2" src="https://user-images.githubusercontent.com/71112826/105457742-272d3780-5c3c-11eb-9710-4c58b46370e7.png">



#### Lot 3

<img width="515" alt="Lot3" src="https://user-images.githubusercontent.com/71112826/105457745-27c5ce00-5c3c-11eb-9eec-2898150757b5.png">

Lot 3 with p-value of 0.04 shows that it has statistically significant difference from the population mean of 1,500 PSI but not Lot 1 and Lot 2 with p-value of 1 and 0.61, respectively.

## Study Design: MechaCar vs Competition

Study design to show if there is significant relationship between cars different drivetrain (AWD, FWD, RWD), Engine Size and maintenance cost. 

Null hypothesis - Annual cost for all types of car’s drivetrain and engine size are the same. 
Alternative Hypothesis -  They don’t cost the same.


For this study design, we will use ANOVA test since we are comparing maintenance cost of three type of cars drivetrain and different engine size. Using the data collected, we will show if there is a correlation between engine sizes of different drivetrain of cars to its maintenance cost. After doing a test, visualization of results through scatter plot, y value = maintenance cost, x value = engine types and Drivetrain (AWD, FWD, RWD) as scatter plot and color labels. 

Data will be collected from 500 samples of cars with different drivetrain, engine size and its corresponding maintenance cost for three years will be use for this study design. 
