####################################################################################################
##
## This is the script for the R package 
## "flirt: Flexible Item Response Theory modeling with efficient maximum likelihood estimation"
## To cite package <flirt>  in publications:
##
## Jeon, M.,  Rijmen, F. & Rabe-Hesketh, S. (2014). 
## Flexible item response theory modeling with FLIRT. Applied Psychological Methods, 38, 404-405
##
## For any questions or bug reports, contact Minjeong Jeon at jeon.117@osu.edu
##
####################################################################################################


####################################################################################################
## 
##  Note on quadrature points:
##  
##  In the examples below, Gaussian quadrature with 5 quadrature points is sometimes used 
##  for an illustration purpose.
##  For accurate results, more quadrature points may be needed (the package default is 20). 
##  
##  For models with person covariates for which computation can be slower,  
##  a smaller number of quadrature points can be used with adaptive quadrature. 
##
##  In general, computation can be slower with adaptive quadrature although
##  a small number of quadrature points may be sufficient with adaptive quadrature. 
## 
####################################################################################################


# download the package using the function below (or download it from the flirt website) 
library(downloader)
download.file("http://faculty.psy.ohio-state.edu/jeon/lab/files/flirt_1.15.tar.gz", "flirt_1.15.tar.gz")

# set directory where the .tar file is located.
# 64 bit machine 
install.packages("flirt_1.15.tar.gz", type="source", repos=NULL) 
library(flirt)


# 32 bit machine 
download.file("http://faculty.psy.ohio-state.edu/jeon/lab/files/flirt.x32_1.15.tar.gz", "flirt.x32_1.15.tar.gz")
install.packages("flirt.x32_1.15.tar.gz", type="source", repos=NULL) 
library(flirt.x32)


# try this if you get an error with above 
#install.packages("flirt", type="source", repos=NULL) 


# check out built-in datasets 
#data(package="flirt")

#######################################################
## verbal aggression 
#######################################################

## Unidimensional 1PL models ##

# binary data 
data(verb2)

# have a look
verb2[1:5,1:6]

# binary item responses  
resp_uns <- verb2 

# person covariates
# gender is a person covariate or group membership. group membeship should have consecutive numbers starting at 0
# gender should be a matrix of size N x p (where N is the number of people and p is the number of person covariates)
data(person_design)
person_cov_uns <- person_design

#for multigroup analysis, responses and person covariates need to be ordered by group
sort_results<-sort(person_cov_uns[,1],index.return=T)
resp<-resp_uns[sort_results$ix,]

# sort the porson covariates by group (same order as resp)
person_cov<-matrix(person_cov_uns[sort_results$ix,],ncol=1)


# item covariates: including intercept 
data(item_design_bin)
item_cov <- item_design_bin

head(item_cov)


# 1PL model 
result1 <- flirt(data=resp, control=list(nq=5) )

beta <- result1@pars[2:11,1]
# item information function for first four items 
Item_info(beta= beta[1:4])

# test information function 
Test_info(beta= beta)
            
# item resonse curve for four items             
IRF(beta=beta[1:4])


# Dif 
result2 <- flirt(data=resp, dif=list(on=T, dif_beta=c(1,2,3) ), mg=list(on=T, group_matrix=person_cov), control=list(nq=5) )

# Latent regression 
result3 <- flirt(data=resp, person_cov=list(on=T, person_matrix=person_cov), control=list(nq=5) )

# LLTM
result4 <- flirt(data=resp, item_cov=list(on=T, item_matrix_beta=item_cov, item_matrix_alpha=NULL ) , control=list(nq=5))

# MG 
result5 <- flirt(data=resp, mg=list(on=T, group_matrix=person_cov), control=list(nq=5) )

# output 

result5 

summary(result5)

coef(result5)

logLik(result5)

anova(result1, result5) # result1 is nested within result5

# flirtClass slots 
result5@pars # parameter estimates and SEs
result5@info_num # information matrix (if se_num is used )


## more models: other combinations 

# LLTM and MG 

comb1 <- flirt(data=resp, item_cov=list(on=T, item_matrix_beta=item_cov), mg=list(on=T, group_matrix=person_cov), control=list(nq=5) )

# LLTM and latent regression 

comb2 <- flirt(data=resp, item_cov=list(on=T, item_matrix_beta=item_cov), person_cov=list(on=T, person_matrix=person_cov) , control=list(nq=5))

## LLTM, MG, latent regression

comb3 <- flirt(data=resp, item_cov=list(on=T, item_matrix_beta=item_cov), 
     person_cov=list(on=T, person_matrix=person_cov), mg=list(on=T, group_matrix=person_cov) , control=list(nq=5))


# Test : some options 
result01 <- flirt(data=resp, post=T )

result02 <- flirt(data=resp, select=1:10, subset=1:100, control=list(nq=10, se_num=F, se_emp=T) )

result03 <- flirt(data=resp, select=1:10, subset=1:100, control=list(max_it=3, verbose=T) )

# plot observed vs expected sum score
exp_score<-result01@post$exp
obs_score<- rowSums(resp)
eap<-result01@post$eap
postvar<-result01@post$eap_var
irt_rel<-result01@post$rel

plot(obs_score,exp_score)
plot(obs_score,eap)
plot(eap,postvar)

cor(exp_score, obs_score)
cor(eap, obs_score)


## Unidimensional 2pl model 

# regular, a(th+b)
result20 <- flirt(data=resp, loading=list(on=T, inside=T), control=list(nq=5, alp_bounds_up=5, alp_bounds_low=-5, adapt=T))

# regular, a*th + b 
result21 <- flirt(data=resp, loading=list(on=T, inside=F), control=list(nq=5)  )


# latent regression 
result22 <- flirt(data=resp, loading=list(on=T, inside=F) ,person_cov=list(on=T, person_matrix=person_cov) , control=list(nq=5) )


# lltm : beta  
result23 <- flirt(data=resp, loading=list(on=T, inside=T), item_cov=list(on=T, item_matrix_beta=item_cov)  , 
            control=list(nq=5, alp_bounds_up=5), post=T)

post = result23@post
names(post)
#[1] "eap"     "eap_var" "eap_cov" "exp"     "rel"    
length(post$eap)
#[1] 316
length(post$eap_var)
#[1] 316
length(post$eap_cov)
#[1] 316
length(post$exp)
#[1] 316
length(post$rel)


# lltm + person covariate + mg 
result232 <- flirt(data=resp, loading=list(on=T, inside=T), item_cov=list(on=T, item_matrix_beta=item_cov, item_matrix_alpha=item_cov)  , 
            mg=list(on=T, group_matrix=person_cov),
            person_cov=list(on=T, person_matrix=person_cov) ,
            control=list(nq=5, conv=0.01))



# mg 
result24 <- flirt(data=resp, loading=list(on=T, inside=T), mg=list(on=T, group_matrix=person_cov), control=list(nq=5)  )

# dif 
# dif on alpha and beta cannot be tested when the item intercept is regressed on item covariates
result25 <- flirt(data=resp, loading=list(on=T, inside=F), dif=list(on=T, dif_beta=c(1,2), dif_alpha=c(1) ), 
                mg=list(on=T, group_matrix=person_cov) , control=list(nq=5))


# latent regression + lltm 
result26 <- flirt(data=resp, loading=list(on=T, inside=T), item_cov=list(on=T, item_matrix_beta=item_cov) , 
            person_cov=list(on=T, person_matrix=person_cov) , control=list(nq=5) )



## Multidimensional 1PL 

# regular 
result31 <- flirt(data=resp, mul=list(on=T, dim_info=list(dim1=1:12, dim2=13:24)) , 
    control=list(nq=5, alp_bounds_up=5), post=T )

eap <- result31@post
names(eap) 
aa <- eap$eap 
dim(aa)
head(aa)
bb <- eap$eap_cov # ndim x ndim is for each person 
dim(bb)
head(bb)

# latent regression
result32 <- flirt(data=resp, person_cov=list(on=T, person_matrix=person_cov ), 
                mul=list(on=T, dim_info=list(dim1=1:12, dim2=13:24), cov_info=list(dim1=1, dim2=1)) , control=list(nq=5) )

# lltm 
result33 <- flirt(data=resp, item_cov=list(on=T, item_matrix_beta=item_cov), 
                mul=list(on=T, dim_info=list(dim1=1:12, dim2=13:24)), control=list(nq=7, verbose=T) )


# mg 
result34 <- flirt(data=resp, mg=list(on=T, group_matrix=person_cov), 
                mul=list(on=T, dim_info=list(dim1=1:12, dim2=13:24)) , control=list(nq=5))

# dif 
result35 <- flirt(data=resp, dif=list(on=T, dif_beta=c(1,2) ), mg=list(on=T, group_matrix=person_cov), 
                        mul=list(on=T, dim_info=list(dim1=1:12, dim2=13:24)), control=list(nq=7) )

# within-item model with lltm 
result36 <- flirt(data=resp, item_cov=list(on=T, item_matrix_beta=item_cov), 
                mul=list(on=T, dim_info=list(dim1=1:12, dim2=c(1,2,13:24)) ) , control=list(nq=7) )


## Multidimensional 2pl model 

# no alpha bounds are allowed 
# regular, a*th + b ( a(th+b) is not available for multidimensional 2pl models)
result41 <- flirt(data=resp, loading=list(on=T, inside=F),  
            mul=list(on=T, dim_info=list(dim1=1:12, dim2=13:24)) , control=list(nq=5))

summary(result41)
# Note that the mg_sd11, mg_sd12, and mg_cov_11 are unstandardized standardized deviations for dimension 1 and 2 
# and covariance between dimension 1 and 2. 
# flirt estimates Choleksy matrix for covariance matrix
# To access the estimated Cholesy elements, use 
result41@parms 

# Standardized covariance matrix (correlation matrix) and standized loading estimates 
# use the function stat_coef and std_cov 

est_alp <- result41@pars[1:24,1]  # vector of alpha estimates 
est_cov <- result41@pars[51:53,1]  # vector of covariance matrix estimates 
cov_matrix <- matrix(c(est_cov[1]^2,est_cov[3],est_cov[3],est_cov[2]^2),2,2, byrow=F) # covariance matrix 
dim_info <- list(dim1=1:12, dim2=13:24) # list 

test0 <- std_coef(est=est_alp, dim_info=dim_info, cov_matrix= cov_matrix) 

test1 <- std_cov(dim_info=dim_info, cov_matrix= cov_matrix)     


            
# constraints 
est <- coef(result41)[,1]
result412 <- flirt(data=resp,  loading=list(on=T, inside=F), 
                mul=list(on=T, dim_info=list(dim1=1:12, dim2=13:24)), 
                constraint=list(on=T, npar= length(est), cons_info= 53 , cons_value=0 ) , 
                control=list(nq=5, conv=0.01) )


# latent regression : takes long 
result42 <- flirt(data=resp, loading=list(on=T, inside=F),  
            mul=list(on=T, dim_info=list(dim1=1:12, dim2=13:24) , cov_info=list(dim1=1, dim2=1))  ,
                person_cov=list(on=T, person_matrix=person_cov) , control=list(nq=5, conv=0.1) )

result422 <- flirt(data=resp, loading=list(on=T, inside=F),  mul=list(on=T, dim_info=list(dim1=1:12, dim2=13:24) , cov_info=list(dim1=1, dim2=0))  ,
                person_cov=list(on=T, person_matrix=person_cov) , control=list(nq=5, conv=0.1) )
                
# different n of quadrature points for general and specific dimensions                                 
result423 <- flirt(data=resp, loading=list(on=T, inside=F),  mul=list(on=T, dim_info=list(dim1=1:12, dim2=13:24) , cov_info=list(dim1=1, dim2=0))  ,
                person_cov=list(on=T, person_matrix=person_cov) , control=list(nq=c(5,7), conv=0.1) )
                
                
# lltm :within-item: alpha 
result43 <- flirt(data=resp, loading=list(on=T, inside=F),  mul=list(on=T, dim_info=list(dim1=1:24, dim2=13:24)) , 
            item_cov=list(on=T, item_matrix_alpha=item_cov) , 
            control=list(nq=5, conv=0.01) )

post = result43@post
names(post)

# lltm :between-item: beta, alpha 
result432 <- flirt(data=resp, loading=list(on=T, inside=F),  mul=list(on=T, dim_info=list(dim1=c(1:12), dim2=13:24)) , 
            item_cov=list(on=T, item_matrix_beta=item_cov, item_matrix_alpha=item_cov) , 
            control=list(nq=5, conv=0.01)  )


# mg 
result44 <- flirt(data=resp, loading=list(on=T, inside=F),  mul=list(on=T, dim_info=list(dim1=1:12, dim2=13:24)) ,
                mg=list(on=T, group_matrix=person_cov), control=list(nq=5, conv=0.01)  )

# dif 
result45 <- flirt(data=resp, loading=list(on=T, inside=F),  mul=list(on=T, dim_info=list(dim1=1:12, dim2=13:24)) ,
                dif=list(on=T, dif_beta=c(1,2), dif_alpha=c(1,2) ), 
                mg=list(on=T, group_matrix=person_cov) , control=list(nq=5, conv=0.01) )


## Bifactor model 

# regular (make sure no within-item design for testlet models)
result51 <- flirt(data=resp,  bifac=list(on=T, dim_info=list(dim1=1:12, dim2=13:24)) , control=list(nq=5, conv=0.01) ,post=T)


# latent regression : takes long  (main =F)
result52 <- flirt(data=resp,  bifac=list(on=T, dim_info=list(dim1=1:12, dim2=13:24), cov_info=list(dim0=1, dim1=0, dim2=1)) ,
                person_cov=list(on=T, person_matrix=person_cov) , control=list(nq=5, conv=0.01) )

# try different n of quadrature points for general and specific dimensions 
result522 <- flirt(data=resp, bifac=list(on=T, dim_info=list(dim1=1:12, dim2=13:24), cov_info=list(dim0=0,dim1=1, dim2=0)) ,
                person_cov=list(on=T, person_matrix=person_cov, main=T) , control=list(nq=c(7,5,5), conv=0.01) )

# lltm beta and alpha 
result53 <- flirt(data=resp, bifac=list(on=T, dim_info=list(dim1=1:12, dim2=13:24)) ,
            item_cov=list(on=T, item_matrix_beta=item_cov, item_matrix_alpha=item_cov) , 
            control=list(nq=5, conv=0.01) )

    
# mg 
result54 <- flirt(data=resp, bifac=list(on=T, dim_info=list(dim1=1:12, dim2=13:24)) ,
                mg=list(on=T, group_matrix=person_cov), control=list(nq=5)  )

# dif 
result55 <- flirt(data=resp,  bifac=list(on=T, dim_info=list(dim1=1:12, dim2=13:24)) ,
                dif=list(on=T, dif_beta=c(1,2), dif_alpha=c(1,2) ), 
                mg=list(on=T, group_matrix=person_cov) , control=list(nq=5) )



## Second-order model 
# 2PL versions (a_s *th_s + a_g*th_g + b) 
# here a_g/a_s is the second-order loading for specific factor s on the general factor 
# a_g/a_s is the constant for items within specific factor s  
# SE is not currenlty available 

# regular (make sure no within-item design for testlet models)
result61 <- flirt(data=resp,  second=list(on=T, dim_info=list(dim1=1:8, dim2=9:16, dim3=17:24)) , 
            control=list(nq=5, conv=0.01), post=T )


#for first order factor loadings 
result61@first_order 

#for second order factor loadings 
result61@second_order 

#for variances for first order factors 
result61@variances_first 

#for standardized first order factor loadings 

result61@stand_first_order 

#for correlation between first order factors and second order factors 
result61@cor_first_second


# lltm alpha (only for the general factor)
resul62 <- flirt(data=resp, second=list(on=T, dim_info=list(dim1=1:8, dim2=9:16, dim3=17:24)) ,
           item_cov=list(on=T, item_matrix_beta=item_cov, item_matrix_alpha=item_cov) , 
            control=list(nq=5, conv=0.01) , post=T  )
    
# mg 
result63 <- flirt(data=resp, second=list(on=T, dim_info=list(dim1=1:8, dim2=9:16, dim3=17:24)) ,
                mg=list(on=T, group_matrix=person_cov), control=list(nq=5, conv=0.01) , post=T )

# dif beta 
result64 <- flirt(data=resp,  second=list(on=T, dim_info=list(dim1=1:8, dim2=9:16, dim3=17:24)) ,
                dif=list(on=T, dif_beta=c(1,2) ), 
                mg=list(on=T, group_matrix=person_cov) , control=list(nq=5, conv=0.01) , post=T  )

# dif alpha 
# only for the general factor 
result642 <- flirt(data=resp,  second=list(on=T, dim_info=list(dim1=1:8, dim2=9:16, dim3=17:24)) ,
                dif=list(on=T,  dif_alpha=c(1,2)), 
                mg=list(on=T, group_matrix=person_cov) , control=list(nq=5, conv=0.01), post=T  )
       
# dif beta + alpha 
result643 <- flirt(data=resp,  second=list(on=T, dim_info=list(dim1=1:8, dim2=9:16, dim3=17:24)) ,
                dif=list(on=T,  dif_beta=c(1,2) , dif_alpha=c(1,2)), 
                mg=list(on=T, group_matrix=person_cov) , control=list(nq=5, conv=0.01) , post=T )                
     

# latent regression with person covariates 
result65 <- flirt(data=resp,  second=list(on=T, dim_info=list(dim1=1:8, dim2=9:16, dim3=17:24), 
                cov_info=list(dim0=1, dim1=0, dim2=0, dim3=0)) ,   
                person_cov=list(on=T, person_matrix=person_cov) , control=list(nq=5, conv=0.01))

# main effects 
result652 <- flirt(data=resp,  second=list(on=T, dim_info=list(dim1=1:8, dim2=9:16, dim3=17:24), 
                cov_info=list(dim0=1, dim1=0, dim2=0, dim3=0)) ,   
                person_cov=list(on=T, person_matrix=person_cov, main=T) , control=list(nq=5, conv=0.01))
                

## 3PL model 
# currenlty only the basic model works 

# regular 
result71 <- flirt(data=resp,  guess=list(on=T), 
            control=list(nq=5, conv=0.01) )
        
coef(result71, digits=3)
        
        
        
        
#########################################################################
## spelling data : binary responses with person covariate (male) 
#########################################################################
data(spelling)

result00 <- flirt(data=spelling,  select=c(2:5), 
            loading=list(on=T, inside=F), control=list(nq=5))

# for testing, adding two random covariates 
spelling$group5 <- c(rep(0:4,131), rep(4,3)) # group membership with 5 groups 
spelling$cov1 <- rnorm(nrow(spelling))  # continuous covariate 

# note: group membership should start from 0
# note: if there are covariates in the data (other than item responses, select should be specified )

## 1PL model 

# Latent regression with more than 1 person covariates 
result01a <- flirt(data=spelling, select=c(2:5), 
        person_cov=list(on=T, person_matrix=c(1,7) ), control=list(nq=5) )

result01b <- flirt(data=spelling, select=c(2:5), 
        person_cov=list(on=T, person_matrix=c("male","cov1") ), control=list(nq=5) )

# MG : person membership has 5 groups 
result02 <- flirt(data=spelling, select=c(2:5), mg=list(on=T, group_matrix="group5"), control=list(nq=5) )

# starting values 
result11 <- flirt(data=spelling, select=c(2:5), control=list(nq=4) )
est <- result11@pars[,1]

result12 <- flirt(data=spelling, select=c(2:5), start=list(on=T, npar= 5, start_info= 2:5, start_value=est[2:5] ), control=list(nq=4) )

# note: to check the number of total number of parameters(npar), run the the same model with a few quadrature points (e.g., result11). 
# the length of result02@pars (est) is npar. 
# starting values for the variance components should be the Cholesky elements of lower triangular matrix L (cov = LL')

# parameter constraint
result13 <- flirt(data=spelling, select=c(2:5), constraint=list(on=T, npar= 5, cons_info= c(3,4) , cons_value=c(0,0) ) )

# note: in cons_info, c(3,4) means the third and fourth parameters in result02@pars are constrained.
# in cons_value, c(0,0) means we want to constrain parameters specified in cons_info to c(0,0), respectively.
# parameter values (to be fixed at) for the variance components should be the Cholesky elements of lower triangular matrix L (cov = LL')


# evaluate log likelihood at given parameter values 
result14 <- flirt(data=spelling, select=c(2:5), evaluate=list(on=T,  eval_value=est  ) )

# note: eval_value should be the same length of npar. 
# parameter values (to be fixed at) for the variance components should be the Cholesky elements of lower triangular matrix L (cov = LL')


## spelling data with frequency weights 
data(spelling_w) 

result10 <- flirt(data=spelling_w, select=c(2:5), weight=list(on=T, weight_matrix="wt2") )


#######################################################
## charity data : polytomous responses with missing 
#######################################################

data(charity)

# polytomous response model with cumulative link function (graded response model)

result01 <- flirt(data=charity, subset=1:100, control=list(minpercent=0.05, nq=5, link="cumulative"))
# for polytomous responses, the minimum category should be 0
# minpercent: minimum percentage for categories; categories with frequencies lower than minpercent are collapsed.
# note: to see which/how item categories might be collapsed, use control$show=TRUE 

#######################################################
## science data: two-dimensional polytomous data 
#######################################################

data(science)

## multidimensional 1PL with adjacent logit link (partial credit model)

result02 <- flirt(data=science, mul=list(on=T, dim_info=list(dim1=c(1,3,4,7), dim2=c(2,5,6))), 
            control=list(nq=5, link="adjacent") )
est <- result02@pars[,1]
 

            
###############################################             
### IRT Linking example with two test forms 
###############################################       

data(linking)
colnames(linking)

form12 <- c(rep(0,20), rep(1,10))


# fit a unidimensional model (theta on the same scale)
result03 <- flirt(data=linking, control=list(nq=5), post=T  )

# fit a multiple group (different mean/sd for two groups with different test forms) 
result04 <- flirt(data=linking, mg=list(on=T, group_matrix=form12), control=list(nq=5), post=T  )
