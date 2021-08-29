# YBFS
ShinyApp code to explore various measures of preschool enrolment and the target population of children, AKA year before first year of School (YBFS).

This app is designed to assist evaluation of the performance indicators of access to early childhood education. The side-by-side comparison of the three existing measures of preschool enrolment along with three population denominators provides important context for decisions on future measurement and reporting requirements.


See it in action at: https://atyepa.shinyapps.io/YBFS/

# YBFS_lm 
This app is designed to compare and visualise existing and proposed measures of the YBFS – the potential preschool population to be used as a denominator for preschool enrolments (numerator). 

For the historical series (2013 to 2019), this adjustment was done by applying a factor to the first year of school count which is the inverse of the proportional change in ERP between 4 year olds in year n-1 and 5 year olds in year n.

This derived measure from 2013-19 (called 'New YBFS (actual)' in this app) can then provide the basis for establishing relationship between the ERP and new YBFS through a linear regression.

Regression models to predict YBFS were fitted for each state and territory based on the ERP of 4 year olds and 5 year olds (as separate predictor variables). Based on the optimal fit of these two covariates, jurisdictions were either given the simple model (just ERP of 4 year olds) or the model accounting for both ERP of 4 year olds and the ERP of 5 year olds.

See:  https://atyepa.shinyapps.io/YBFS_lm/
