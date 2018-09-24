### 180719 -22 Applied Linear Regression Wu Yuxiao #######################################

ssc install eststo, estout, tabout, outreg

## Basics
ttest = coefficient/standard error
    
* ANOVA/ANCOVA
    SSM = SST - SSR (unexplained variance when forming group)
    determinant coefficient (R^2)= SSM/SST
        between group variance: 
        within group varance: 
    R^2 = SSm/SSt
    F distribution: (f1-x, f2-y)
    Standard Error: standard deviation of standard deviations from sampled regression models while the total population is unknown 
    F (k-1,n-k) = MSm/MSr       # k-1 between group; n-k within group
        Ftail (k-1,n-k,F)       # calculate f probability 
    Regression table(...)
        df
            model: number of variables
            residual: total - model
            model: number of observations - variables
* Regression
        determinant coefficient (R^2)= SSM/SST
        between group variance: 
        within group varance: 
    R^2 = SSm/SSt = r(correlation coefficient)^2
    F distribution: (f1-x, f2-y)
    Standard Error: standard deviation of standard deviations from sampled regression models while the total population is unknown 
    F (k-1,n-k) = MSm/MSr       # k-1 between group; n-k within group; k being number of variables; n being the number of sampling 
        Ftail (k-1,n-k,f)       # calculate f probability 
    Regression table(...)
        df
            model: number of variables
            residual: total - model
            model: number of observations - variables
    Post-regression command:
        predict yhat e, re      # predicted items of yhat and e in addition to y; "re" being the error item
    graph: 
        scatter 
        lowess var1 var2        # smooth correlation
        lfit    # regression fit line
            twoway(scatter y x) (lfitci y x [,range(0,4)]) (lowess y x) ylabel (0(5)30)     # compare lowess and regression result for better explaination
    Covariance(Sxy): +/0+/- mutual variance of two variables 
    Correlation: Sxy/(Sx*Sy): standarized covariance
    Simple linear Regression 
    Dumy variables 

## MLR
* Assumptions
    1. linearity: kdensity, hettest; OTHERWISE problematic
    2. mean(x)=mean(y)=0
    3. e(DV) - N(0,sigma^2): qnorm;rv(residuals scattered evenly across the line); predict e,re; REQUIRED with small sample size
    4. homoscedasiticity - variance of residual being consistent: 
    5. y-E(y|x): e is not relevant to x; OTHERWISE problematic
    6. no self-correlation with same variance: residuals being independent with same distribution; OTHERWISE: GLS, robust regression
    BLUE: Best Linear Unbaised Estimates
* False Correlation
    statistical control: variables controlling (on the same level)
    omission of variables: variances can exist in errors where cannot be well explained
    Chain relationship - interaction/intervening/moderating effect
    Suppressor variables
* Statisical interaction
* Multi variables: whether effects exist among independent variables 
* Categorical Variables (n > 2)
    tab var, gen(var)
    regress dv b1.iv
* Stadardized coefficient
    every unit of increase in X's std may lead to increase of x unit in y
    contribution of each iv can be compared and evlauated   
* Adjusted R square
    can assist to check model validation
* nested models
    controlling variables added before core variables
* model comparisons
    compare R squared of based model and testmodel # baseline models to start with; revised models with core variables to compare
    test baseVar testVar 
    test testVar   
* model table generator
    findit eststo
* interaction
    * categorical * categorical
    main effect: y=a+b1x1+b2x2
    interaction effect: y=a+b1x1+b2x2+b3x1x2
        if x1x2 being significant indicating significnat ainteraction effect
        male(1,0) + party(1,0) + male* party
            male party = male + party + male*party
            female party = party 
            male nonparty = male 
            female nonparty =  constant
    regress dv iv##iv       # ivs near '##'' being moderator to the effect of iv on dv
    * continuous * categorical
    interaction effect: y=a+b1x1co+b2x3ca+b3x1cox3ca
    feduy(contianous) + male(0,1) + feduy*male
        edu higher male: feduy + male + feduy*male + con
        edu lower male: male + con
        edu higher female: feduy + con
        edu lower female: con
    refress dv c.iv##iv     # indicates iv categoricalcontinuous
    twoway (scatter educ_y feduy) (lfit educ_y feduy)
    * continuous * categorical (n>2)
    main effect: wage =eduy + region
        regress wage eduy b1.region                         # regression with no interaction
    interaction effect: wage = eduy + region + eduy*region
        generate inter1 = region2*eduy
        generate inter2 = region3*eduy
        regress wage eduy region2 region3 inter1 inter2     # 2 interaction effects evaluated with all ivs
            regress wage c.eduy##b1.region                  # eduy as continuous variable, region(item1 being as reference group)
    margins region, at(educy=(0(2)20))
    * continuous * continuous
        egen mage=mean(age)             # 
        gen cage=age-mean(age)          # 
        sum1 cage age
            reg wage c.eduy##c.cage     # 
    * quadratic functions and model
        tw(lowess dv iv) (lfit dv iv) (qfit dv iv)      # smooth curve, regression line and quadratic regression line
        gen ivsq = iv^2
            reg dv iv ivsq                              # polynominal regression model 
        regress dv c.iv##c.iv                           # iv squared as interaction term
            wherext iv ivsqt                            # show extreme value
        regress dv b.mv#c.iv##c.iv                      # moderator, iv and ivsq
        gladder var if condition=1 # show all possible transformation methods
        * note: significant quadratic term and non-significant linear term indicates non-linear change of slope of quadratic functions
    * model comparison
        [after regressino table] 
        test ivn                        # Ftest and p
    * NOTES
        interaction term and main effect model are together presented


## File
set more off, permanently
save filename, replace          # file saved as the name to replace the past file
outfile/outsheet [varlist] using filename [if] [in] [, opt]
use "path filename" [, clear]   # load external dataset
sysuse datafile.dat             # load internal dataset
infile/insheet [varlist] using filename
reset file association: edit-preference-reset

## Weight
    fw: frequency, integer
    pw: reversed [most used]
    aw: reversed variance
    iw: customized "importance"

a* b* # all variables begin with a and b
command var1 var2 # var1 being dv or on y axis if assigned
help commandname # seek help
notes: writing notes  # writing notes

## Common Expression
    [prefix:] command [varlist] [= exp.] [if exp.] [using filename] [in range] [weight = ] [, options]
        [by varlist:]           # grouping for command, require sort in advance
        [bysort var:]           # sorting before grouping 
        [by var, sort:]
        [in range]              # var1-var10 # 10 variables in between; in -5 # the last 5th variable
    command ()                  # refer to all variables
    var1-var10                  # 10 variables in between
    in -5                       # the last 5 variables
    a* b*                       # all variables begin with a and b
    command var1 var2           # var1 being dv or on y axis if assigned
    help commandname            # seek help
    notes variable: description # writing notes
    set more off                # disable the splited views of long list
* operations
    priority: 
        operation > relation > logic
        "^" > "*/" > "+-"
        & > |
    round(pi, 0.01)  # output 3.14
    round(32,10) = 30
    display 3+4; di 1-normal(1.96); di invnorm(0.95)  # operations
* functions, help functions


## Data Management
* procedure
    1. missing values
    2. distribution
    3. summary
* memory
    set memory 10m # set memory to a value
    memory # view memory usage
* data editing
    excel directly paste to file editor
    order var3 var2 var1        # put var3-1 in order
    move var2 var1              # move to front
    sort variable
        sort var 1 var2                     # sort observations ascendingly 
        gsort -var1 +var2                   # sort numbers of var1 descendingly and var2 ascendingly
        bysort var1: sum var2 var3          # or by var1: sum var2 var3, sort - do the analysis between groups
            sort inc; gen inc10g=group(10)
        by var1 list var2 if var3==1
        bysort male: tabulate agegroup healthgd, row nonfreq chi2
    generate/egenerate
        generate newvar = exp. [if] [in]    # only valid for new variable
        generate newvar = var*10
        tab var1, gen(var1)                 # generate dummy variables 
            tab1 var1.1 var1.2 var1.3...    # tabulate separate tables  
        generate var1 = var2==1             # give value 1 to var1 if var2 is 1; missing value will turn to 0
        egen meanhdi=mean(hdi)              # generate variable of mean
        bysort continent: egenerate meanhdi2 = mean(hdi)[max,min]    # generate mean hdi by different continent
        egen v1_5min = rmeann(v1-v5) [min,max]                       # row mean of variables
        bysort h_id: egen size = count(h_id)
    replace
        replace var1=0 if var2~=1           # for old variables
    recode
        recode age min/29=1 30/39=2 40/49=3 50/59=4 60/.=5,gen(agenew)        # recode into a different variables with coding; "." indicate the max values
        gen incg6=recode(inc, 300,600,1000,1500,3000,50000)
        gen agegroup=recode(age,29,39,49,64); label var agegroup "age group"; label definition agegroup 1 "20-29" 2"30-39"...
        gen ageg5 = autocode(var, number of group, min, max) 
            gen ageg5 = autocode(age, 9, 20, 65)    # divide into 9 groups evenly between age of 20 to 65, the group will be labels instead of numberic numbers
    group(n)
        sort income; group inc10g=group(number of observation in each group)          

* data loading
    use data                                    # use data file named "data"
    use data [,clear]                           # use data file named "data"; [options] clear data before new input
    use quotation mark if space exists
    use https://outlook.live.com/mail/inbox     # designate to website  
    sysuse name, clear
        sysuse dir
    describe                                    # data description and structure
    codebook variable1 variable2                # display coding of variable
    list var1 var2 var3 hdi if hdi>20 in 40/50  # locate observations of 40 to 50th position and show designated varaibles
    browse if var1 < 10 in 1/100                # read observations that meet the filter
    edit                                        # the same to 'browse' only if it can edit
    infile [varlist] using dataname.raw
    help oneway                                 # search help info on oneway ANOVA
    preserve/restore                            # save certain dataset and allow operation in between

* coding issues
    cd working directory 
    unicode analyze*                            # check file issues with automatic fix
    unicode encoding set gb18030                # change coding to gb
    unicode translate*,invalid(ignore)          # translate files to be fixed; ignore invalid coding

* data transformation
    * DV being skewly distributed - log/ln transformation
        * Regression interprets log as 1 unit of change in x lead to n% in y
        y=e(b)-1 [b>0] | 1-e(b) [b<0]
    * IV being skewly distributed - log/ln
        y = log(x/b) 
    * DV and IV are non-linearly correlated - poly nominal regression
        * y=a+b1x+b2x^2+e [b1*b2<0]
        tw(lowess dv iv) (lfit dv iv) (qfit dv iv)      # smooth curve, regression line and quadratic regression line
        gen ivsq = iv^2
            reg dv iv ivsq                              # polynominal regression model 
        regress dv c.iv##c.iv                           # iv squared as interaction term
            wherext iv ivsqt                            # show extreme value
        regress dv b.mv#c.iv##c.iv                      # moderator, iv and ivsq
    gladder var if condition=1 # show all possible transformation methods

* data merge
    panel data - wide data
    longitudinal data - long data
    reshape long prefixnames,i(id)j(year)           # wide to long, i with prefix while j with suffix
    reshape wide prefixnames,i(id)j(year)           # long to wide
    reshape long a@b,i(a)j(year)                    # a1b,a2b,a3b
    reshape long prefixnames,i()j() string          # string suffix
    reshape wide inc,i(hid sex)j(year)              # hid and sex to match

* append data/variable
    append using filename [, options]               # vertical merge, adding observations
    append using data2                              # add observations of data2 to data1
    append using "", gen(newvar) keep(var1-var5)    # generate dummy variable upon loading file
    merge [varlist] using filename [filename ...] [, options]
    merge idvar using merge2, update sort           # merge sorted variables by the id index; update data of discrepencies from the latter data
    merge idvar using merge2, update replace sort   # merge sorted variables by the id index; replace data of discrepencies from the latter data
    * 
    merge 1:1 varlist using "location/filename"[,generate newvar]     # match accurately by make on variables
    merge m:1 varlist using "location/filename"[,options]
    merge 1:m varlist using "location/filename"[,options]
    * m:m
    joinby familyID using children [,unmatched(both)]                 # preloaded with parent dataset; merge with observations and variables
    cross using data2 # cross merge to meet every possible match

* labeling
    list var in 11/20 # list observations numbered from 11 to 20 of the variable
    label data "dataname"
    note: note content                                          # notes for dataset
        notes                                                   # show notes
    label variable variable1 "variable1's label"
        tab var1, nolabel                                       # view value but label
    labeling values: label define [,add/modify]
        label define labelname 1 "text1" 2 "text2" ...
            such as: label define continent 1 "Africa" 2 "Asia" 3 "Europe", modify  # quotation mark can be omitted if there is no space in label values
        label value variable labelname
        label define labelname 3 "new text", add
        label define labelname 3 "newer text", modify
    label dir
    label list labelname
    label drop 
    rename oldname newname
    order var1 var2                                             # order the positions of variables
    move var1 var2                                              # move var1 to the position of var2
    lookfor                                                     # search variables or labels

* sampling
    sample retio                    # ratio
    sample samplesize,count         # randomized sampling 
    sample samplesize,count by(male)# hierarchical sampling
    sample # [if] [in] [, count by(groupvars)]
* data saving
    save "directory"                # save data
    save filename, replace          # file saved to replace original file
    saveold filename [,replace]     # save as old version
        saveold location, repalce version(11)
    outfile/outsheet [varlist] using filename [if] [in] [,opt]
    use filename [, clear]
    infile/insheet [varlist] using filename
* Trouble Shooting
    * missing data
        1. MCAR/MAR: deletion
        2. imputation with meansdummy variables as controlling vairables in regression
        3. regressional imputation
    * influential data or outliers
        1. deletion of cases
            scatter y x
            stadarized residuals: predict e,re
            predict e2, rstudent                # standarized residuals; r < 3 suggested
                list var if e2>3 & e2!=.
                count if e2>3 & e2!=.           # deletion of outliers
        2. rregress y x                         # robust regression, dealing with outliers
    * colliearity - high correlation (r>0.9) of IVs while not contributing to model
        # higher S.E. and lower t; high r^2 and insignificant p
        VIF: the highest VIF > 10 (except for polynominal model) and mean(VIFs) (significantly)> 1
            estat vif 
                variable's p values： insignificant 
                r square： high
                model's p values: insignificant 
        1. keep one of variables
        2. EFA
    * omitted variables
        1. proxy vairalbes - nested model: latent variables nested in e item, in which equals to b*lv+e  # endogenous issues
        variables of hierachical level
        2. IV (instrumental variables): Cov(Z,xi) ~=0 & Cov(Z,e)=0                # exogeneous variable
        3. lagged DV: Crime = a + b1Unemp + b2Consum + b3Crime-1 + e              # previous crime incidents can be influential
        4. fixed effect models
        5. twin sampling
        6. counterfactual analysis model

## Variables
* variable 
    numeric: 
        "."             # missing value
        < "."           # not missing value
        x > 10 & x <.   # [10,+infinitive] not including missin values
    string: 
        ""      # null
        " "     # space
        "."     # dot, not missing values
* Data format: %w.de/f/g(c)
    w: width
    d: digits
    e: scientific number
    f: fixed format, s-string
    g: general/default format
    c: comma as delimiter
    s: string
    set var1 float; set graphics on; set more off; cd e:\data
* dummy variable
    gen/replace elderly=1 if age>=60
    gen/replace elderly=age>=60
    *[application]
    reg var1 var2[categorical]          # var2 ==0 is not valid
    var1.1 = var1==1; var1.2 = var1==2  # set separate dummy variables
    reg var1 i.var2                     # dummy variable
    reg var1 b2.var2                    # set the second vategory as the reference group
    * tab var1, gen(var1)               # generate dummy variables 
        regress dv var1.1 var1.2 var1.3 # regression with all dummy variables; reference group automatically set up    
        regress dv var1.1 var1.2        # var1.3 to be set as reference group for regression
        regress dv b3.var1              # var1.3 to be reference group inside var1
* numbering categorical variable
    encode nation, gen(nationx)         # stirng to numeric (ascendingly ordered)
    decode contin, gen(continx)         # numeric to string (with numeric labels)
    tostring var, replace               # change to string
    destring var, replace ignore($)     # change to numeric by taking out $ sign
    egen meanhdi=mean(hdi)

* keep and drop observations or variables
    keep if continent==1|contin==3 keep observations conditionally
    keep rank contin-gdp # keep variables
    use hdr2004.dta if contin==1 # keep observations conditinally upon file extraction
    drop if contin==2|contin>3

## Analysis
# Descriptive Analysis
* interval estimation
    ci means var1 [,level(99)]          # Confidence Interval, 95% by default; level can be set
* summarize
    summarize varlist [,detail]         # basic description of variable; [show variance, skewness, kurtosis percentiles with sum of wight]
        return list                         # show details of certain variable
        list                                # list dataset
        "-"                                 # var1 - var10
        _all                                # all variables
* tables
    tabstat varlist [,by(groupvar) stat(mean median max min n sd var)]       # customized statistical analysis 
    kdensity,[normal]                   # kernal density estimates with normal distribution
    tabulate var1 (var2)[,option]       # tabulate a variable or two variables 
        sort                                # sort by frequency
        generate(var)                       # dummy variables
        row                                 # row percentage
        nof/nolabel                         # no frequency/no label
        column                              # column percentage
        cell                                # all stats
        sum(var2/var3)                           # show stats of income inside cells
    tab1 varlist                            # separated tables of 1 variables
    tab2 varlist                            # separated tables of 2 variables 
        bysort var3, tab var1 var2              # 3 way table
    table var1 var2 var3                    # 3 way table, only show frequency no percentage
        bysort var4, table var1 var2 var3       # 4 way table
    table var1 var2, content(mean var3 sd var4 n var5...[stats varn])  # equivalent to tab var1 var2, sum(var3)
        [content:]
            freq                 frequency
            mean varname         mean of varname
            sd varname           standard deviation
            semean varname       standard error of the mean (sd/sqrt(n))
            sebinomial varname   standard error of the mean, binomial distribution (sqrt(p(1-p)/n))
            sepoisson varname    standard error of the mean, Poisson distribution (sqrt(mean))
            sum varname          sum
            rawsum varname       sums ignoring optionally specified weight
            count varname        count of nonmissing observations
            n varname            same as count
            max varname          maximum
            min varname          minimum
            median varname       median
    * tabout: 
        1. cannot summarize two statistics at a time
        2. can form oneway or twoway table
        3. append for table merge 
    tabout male edug using "C:\Users\ThinPak\Desktop\test.xls",c(row) append
    tabout male edug using "C:\Users\ThinPak\Desktop\test.xls",c(col) append
    tabout male edug using "C:\Users\ThinPak\Desktop\test.xls", c(mean inc) sum append      # summarize mean for inc and output to the designated location

# Inferential Analysis
    * parameters (total population) and statistic(sampled population)
    * sampling distribution: sampling with replacemnet for multiple times
    * Sampling error: SS and  MSE = SS/(n-2)
    * Confidence Interval
        for continuous and categorical variables
        M +/- se*p(alpha=0.95)                  # level of alpha 95 by default
        ci [varlist] [if] [in] [weight] [, options]
            ci var,level(99)                    # alpha level setting
            ci var, level(95) binomial          # binomial distribution
                methods: exact, wald, wilson, agresti, jeffreys
            ci var, poisson                     # poisson being a case of binomial with big n and small p; exposure statsitics 
* ttest - Z score calculation satisfy when n is big enough
    * One sample T test
    ttest var == [value],level(alpha)           # value is a set reference point 
        SE = sd/sqrt(n); df = n-1; t=(Xhat-value)/SE
    * Two independent T test
    ttesti obs1 mean1 std1 obs2 mean2 std2, unequal # nonparametric t test between two defined groups
    ttest var1 if variable<3. by(var2) unequal  # t test comparing var1 and grouped by var2, assuming variance of two groups are different
        Variance being S if variance of two groups are the same
        Variance being if variance are different
        df = n-2                                # two IVs are used in test
    * Paired T test
    ttest a==b,unpaired unequal                 # a and b from the same sample are not paired            
    ttest after == before                       # paired ttest the paired subsamples from same sample
        ttest after = before                        
            gen diff = after - before
        ttest diff = 0                          
* Chi-square - categorical variables - stats evaluating diff between observed and expected values
    df = (r-1)(c-1)
    significance: observed and expected values are significantlly different
    marginal distribution: marginal stats
    conditional distribution: cell stats
    two independent variables: marginal dist=conditional distribution
        Nij/Ni* = N*j/N
    tab var1 var2 [, option]
        exp                         # show values of expectancy
        chi2                        # show chi square statistics
        chi2 cchi2                  # show difference 
        lrchi2
        v
        Gamma
        taub
* Likelihood Ratio - categorical variables - for smaller sample size
     tab var1 var2, chi2 lrchi2     # likelihood ration chi square
     tab var1 var2, lrchi2 clrchi2  # likelihood ration chi square cells
* Fisher's exact analysis
    tab var1 var2, exact            # significance indicates differences among cells
* Cross tab coefficient - tab var
    Cramer's V                      # nominal vars
    Gamma                           # order vars
    Kendall's Tau-b                  
* ANOVA - comparisons of means of multiple groups - compare within group variance
    DV: continuous; IVs: categorical
    Ho: all groups all equal
    H1: at least two groups are different; variance of group means beyond limits
    F = MSSb/MSSw|MSSr                          # bigger within group variance indicates smaller between group variance
    
    sum weight; return list
    oneway weight region [,tabulate sheffe]     # methods include sheffe (two-group comparisons)
    ANOVA var1 var2                             # showing determinant coefficient in addition to ONEWAY function
        SSt:                                    # df = n-1
        SSr:                                    # df = n-dfm
        SSm|SSb: sum of partial SS if not overlapping # df = k-1 for each IV
        F = MSSb/MSSw
        [,options]
            sequnetial                          # distribute overlapping variance to the first var entered into model
    * Interaction
        SSt:                                    # df = n-1
        SSr:                                    # df = n-dfm
        SSm|SSb: sum of partial SS if not overlapping # df = k-1
        interaction                             # df = (n1-1)(n2-1)
        F = MSSb/MSSw
* Correlation - cannot validate non-linear correlation
    # missing values - entire observation/row is not calculated for any missing value
    correlate var1 var2 var3... [, cov]         # casewise, only show covariance statistics
    pwcorrelate var1 var2 var3 [,options]
        star(0.05)                              # pairwise, check significance level
        sig                                     # significance
        obs                                     # show observations
        casewise                                # delete entire observation/row
    graph matrix varlist 
    
* Regression
    E(y|x) = f(x)
    e = Y (actual obs)- Yhat (expected obs)
    regress dv iv1 iv2 ...
    twoway (scatter var1 var2) (var1 var2) (var var...)
* Interaction/Moderation Effect
    twoway (scatter y x) (lfit y x if moderator=1) (lfit y x if moderator=2)    # interaction effect made by gender on salary by educational level 

## Graph
    scatter var1 var2 
    graph twoway (scatter var1 var2 || lfit var1 var2), by (var3)   # scatter plots with 2 variables in addition to lfit function; and arraged by groups var3 
    twoway (scatter var1 var2) (lowess var1 var2)  
    * margine plot 
    margines mv, at(iv=(0(2)20))        # mv being grouping var, iv on xaxis, 2 being smallest unit on 0-20 scale; dv is omitted here
    marginsplot[,noci]                  # no confidence interval
    histogram var, normal               # histogram with line of normal distribution
    xtline varlist                      # stacked graphs of time series 
    
    

## Result presentation: "ssc install prackagename"
tabout using filename   # export tables
outreg using filename   # export regression result

## Configuration
query                   # system configuration
set logtype text, permanently
update query； update all
    db update           # update files from specified location

## Help
offline: pdf documentation, help
online: findit, search

# tables
tabstat age height weight, stats(mean sd range max skewness) # designate statistical analysis
tabstat age, stats(mean sd range max skewness) by(edu)
tabulate var1 (var2)                # tabulate a variable or two variables 
tabulate var1, summarize(var2)
table edu # simple frequency analysis
tab1 var1 var2 var3                 # generate 3 single frequency tables 
tab2 var1 var2 var3                 # generate cross tables between every two variables
bysort male: tabulate agegroup healthgd, row nonfreq chi2

ttest height=165                    # single sample ttest
signtest height=165                 # sign test
ttest pretest=posttest
ttest var1 if variable<3, by(var2)  # t test comparing var1 and grouped by var2
ttest mentalhealth, by(male)

anova mentalhealth education        # compare mental states by education levels
anova inc male edu male*edu         # compare income by gender, education levels and interacting effects
anova mental male edu weight, continuous(weight) # ANCOVA designating weight as continuous variable
anova mental male edu weight, continuous(weight) regress # regression can be done at the same time

correlate var1 var2 var3...
correlate var1-var10
pwcorr var1-var10, sig           # correlation with significance level 
correlate var1-var10, covariance # matrix of covariance
graph matrix var1-var10, maxis(ylabel(none) xlabel(none)) # scatter plots

regress dv iv1 iv2 ...
regress dv iv1 iv2, beta # show beta statistics in regression result 
regress inc age if x==1


## Graph (graph query, scheme)
graph twoway histogram 
graph bar 
graph pie
graph twoway scatter
graph twoway line
graph twoway dot

scatter var1 var2 
graph twoway scatter var1 var2
graph twoway lfitci var1 var2 || scatter var1 var2 # line fit confidence interval? with scatter plots
graph twoway (scatter var1 var2 || lfit var1 var2), by (var3)
graph matrix var1-var10, maxis(ylabel(none) xlabel(none)) # scatter plots


### Machine Learning with R ###
install.packages(c("class","tm","gmodels","wordcloud","foreign")) 
1807012000-2129, p.41
1807021202-1400, 1700-1900, p.102
1807050904-0940, p.111

# Learning process: data, abstraction, generalization #

# Steps to apply ML: 
collecting data, exploring and preparing the data, training a model on the data, evaluating model performance, improving model performance

# basic data: 
factors (nominal values), list (mixed), dataframe (a list of lists), 
matrix (a list of numeric variables), array (a matrix of multi-dimensions)
save (x,y,z, file="mydata.RData"); load("mydata.RData") # save dataset
save.image() # save the session
read.csv("data.csv", stringAsFactors=F, header=T) 
inspect(messages[1:3]) # view the first three messages
lower_case = tm_map(message, tolower)
nonumber = tm_map (message, removeNumbers)
nostopword = tm_map (message, removeWords, stopwords())
nopunct = tm_map (message, removePunctuation)
nospace = tm_map (message, stripWhitespace)

# Classification using nearest neighbors
scale() # z-score standardized applied to a dataframe
data_z <- as.data.frame(scale(data[-1]))

# Classification using naive Bayes

# word cloud
raw_train = message [1:5000]
raw_test = message [5001:6000]
wordcloud (raw_train, min.frequ = 40, random.order = FALSE)
spam = subset (raw_train, type == "spam")
ham = subset (raw_train, type == "ham")
wordcloud (spam$text, max.words = 40, scale = c (3, 0.5))
wordcloud (ham$text, max.words = 40, scale = c (3, 0.5))

### 高级计量经济学及STATA应用 ###
180719      p.227

## Monte Carlo
bootstrapping: resampling with replacement

### R数据分析：方法与案例详解 ###
1807092228-2355 p.54
1807102000-2204 p.81
1807151148-1336 p.308
# data type
array: a mix of vector and matrix
list: amix of factors and vectors

# logics
ifelse (condition, statement1, statement2)

# functions and solve
f=function(x) log(x)-x^2
curve(f,xlim=c(0,2))
optimize(f,c(0.1,10), tol=o.ooo1, maximum=T)

# randomized values
set.seed(1);runif (n, min=0, max=1) # n randomized values with min and max set up
RNGkind (kind="Wich")
sample(c("H","T"),10.rep=T) # coin flipping for 10 times with putback options
dnorm - density function; pnorm - accumulated function; qnorm - percentile function

# data loading
scan() # numbers with space, characters with comma
x <- scan (file="dat.txt")
library(foreign); read.spss("dataname.sav")

# data management
Data <- data.frame(gnp,cons,pop); transform(data,pgnp=gnp/pop, psave=(gnp-cons)/pop); pgnp <- with (data,gnp/pop)
Data2 <- data.frame (manu=Cars93$manufacturer, price = cars93$price)
Dates <- as.Date (strDates, “%d/%m/%Y”)
Dataframe$age[dataframe$age == 99] <- NA
sleep (!complete.cases(sleep),)	# extract incomplete observations
sum (!complete.cases(sleep)); mean (complete.cases(sleep))
newsleep <- na.omit (sleep)
newdata = merge (dataframeA, dataframeB, by=”ID”)
yx[,”y”]		# show y observations

### Google Cloud Calculation 龚为纲 #######################################
* Big query；
mapreduce
google books ngram viewer

select ngram, sum (cell_volume_count)cnt FROM [sociology-995:lectureforxian.google_books] where ngram contains "China"
select cell.value, sum(cell,volume_count) cat FROM [] 
group by ngram
order by cat desc

select split(bookname,".") split.books from[]

* Machine Learning: categorization and regression

TensorFlow
Machine Learning Engine
Github
ImageNet

training: input(image) - neural network - output(label)
predicting: new input - existing label

playgraound.tensorflow.org

* Linguistic Analysis - Cloud Vision API
unstructured paragraph -> structured statement
levels: entity,sentiment,syntax,categories(topics)

* Emotion Analysis - GDELT
wordnet


################## Advanced Econometrics and STATA Application #######################################
## OLS
* Conception
    Panel data = cross sectional data + time series data

* OLS Assumptions 
    * small sample size
    1. linearity: distribution 
    2. no strict multicolinearity: 
    3. stric exogeneity: E(ei|x1...xn)=0
        Mean indepdence: E(y|x) = E(y) != f(y|x)
    4. spherical disturbance: 
        consistent variance: var(ei) = var(ej) = sigma^2
        no self-correlation: cov(ei,ej) = 0
    * Big sample size (n > 30)

    endogenous varialbe: correlated to error term
    instrumental variable adopts partial endogenous variable which uncorrelates with errors
    






######################
结构方程模型及其应用

计量经济分析（第六版英文）

拓扑心理学

mit python

贝叶斯 

数据之巅

江恩华尔街

人类简史

数学统计学简史

### Readings ############################################################################
To record：
1.	study hours
2.	time length
3.	translational efficiency
4.	

### 王天夫+李博柏著+STATA+实用教程 ########################################################
1805142200 (10 hours)  p.68