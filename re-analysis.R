#===============================================================================
# 2020-12-01 -- twitter
# peer-review JAMA paper
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#=============================================================================== 

library(tidyverse)
library(magrittr)


# meta --------------------------------------------------------------------

# the data used in meta analysis
meta_estimates <- 
    tibble::tribble(
                 ~country,                          ~study,   ~RR,   ~lb,   ~ub,
                    "USA",           "Lleras-Muney (2005)",  0.42,  0.08,  0.77,
                    "USA",               "Mazumder (2008)",  0.65,     0,   1.3,
                  "Europ",        "Gathmann et al. (2015)",  0.94,  0.91,  0.98,
                 "Norway",         "Grytten et al. (2020)",  0.91,  0.84,  0.98,
                 "Sweden",           "Meghir et.al (2018)",  0.99,  0.88,  1.09,
                     "UK",        "Clark and Royer (2013)",     1,  0.94,  1.07,
            "Netherlands", "van Kippersluis et al. (2009)",  0.97,  0.97,  0.98,
             "USA (corr)",               "Mazumder (2008)",  0.89, 0.743, 1.037,
              "UK (corr)",        "Clark and Royer (2013)", 1.022,  1.01, 1.034
            ) %>% 
    mutate(
        se = (ub - lb)/(2*1.96),
        var = se^2,
        label = paste(study, country)
    )


library(metafor)

# eSup weights -- impossible to back-trach how exactly CVZ produced these
w_cvz <- c(27.9, 17.97, 19.19, 13.7, 0.13, 0.01, 21.09)

# effect sizes with the raw RRs
es_raw <- escalc(
    yi = RR, sei = se, measure = "SMD", 
    data = meta_estimates %>% slice(c(1:7)),
    slab = label
)

# with weight from eSup
es_raw %>% rma(weights = w_cvz, method = "DL") 
# method "DL" is the default in Stata where CVZ did their analysis
es_raw %>% rma(weights = w_cvz, method = "DL") %>% forest()

# store the wrong estimate (exactly the one used by CVZ)
rr_wrong <- es_raw %>% rma(weights = w_cvz, method = "DL") %>% coef()

# with proper inverse-variance weights
es_raw %>% rma(method = "DL")
es_raw %>% rma(method = "DL") %>% forest()

# effect sizes with the corrected RRs
es_corr <- escalc(
    yi = RR, sei = se, measure = "SMD",
    data = meta_estimates %>% slice(c(3:5, 7:9)),
    slab = label
)

# with corrected coefficients
es_corr %>% rma(method = "DL")
es_corr %>% rma(method = "DL") %>% forest()

# store the corrected estimate
rr_correct <- es_corr %>% rma(method = "DL")  %>% coef()


# life tables checks ------------------------------------------------------

# Life tables for the US 2017 males and females come from CDC
# Arias, E., & Xu, J. (2019, June). United States Life Tables, 2017. National Vital Statistics Reports: From the Centers for Disease Control and Prevention, National Center for Health Statistics, National Vital Statistics System; Natl Vital Stat Rep. https://pubmed.ncbi.nlm.nih.gov/32501200/

# the data
mx_males <- c(0.00632226950903415, 0.000422775747410972, 0.000287476653142719, 0.000224970263755197, 0.000158177275653063, 0.000155552688018643, 0.000138465733112914, 0.000124372010134123, 0.000110416722392824, 9.82084948956533e-05, 9.39812229932824e-05, 0.000107867065008478, 0.000151568533141826, 0.000231921948144827, 0.000341448252029625, 0.000461434570781203, 0.000584331813857119, 0.000717836240630526, 0.000858966618314768, 0.0010019885267079, 0.00114772939606543, 0.00128676974364546, 0.00140381757769145, 0.00149110416883173, 0.00155502669817919, 0.00161048790561027, 0.00166504519960817, 0.00171476919746506, 0.00176309994872098, 0.00181194700611167, 0.00186048725598298, 0.00190927888138717, 0.00196111856526915, 0.00201615959271786, 0.00207377499413846, 0.00214130526542121, 0.00221378165624976, 0.00227940990571385, 0.00233565584685844, 0.00239269519978819, 0.00246568227704371, 0.00256881925900718, 0.00270463146630933, 0.00287381681603144, 0.00307075767002442, 0.00328550045902885, 0.00352630965837379, 0.00381116915863466, 0.00415458810097002, 0.00455724695186968, 0.00499071898601084, 0.00545577354609457, 0.00598294081976762, 0.00657004014006209, 0.0071957168442714, 0.00783374619173973, 0.00848047049548716, 0.00915792663839142, 0.00988672069580934, 0.0106760087608035, 0.0115360687560272, 0.0124381705350083, 0.013348846378539, 0.0142404148630368, 0.0151325029172268, 0.0160697955290481, 0.0171726261328814, 0.0183556610831256, 0.0196748279017037, 0.0212130556076285, 0.0227028140631317, 0.0249380349156175, 0.026927277466436, 0.0294683793136262, 0.032044669645934, 0.0352551640578939, 0.0388900943301892, 0.0431614817870752, 0.0476296997788337, 0.0527566431768533, 0.0584420247883665, 0.0645311973949119, 0.0719504938398219, 0.0807557584150202, 0.0900338183380276, 0.100234200341519, 0.111696358159708, 0.125993830236757, 0.141829218862204, 0.159292143031088, 0.178458080357054, 0.199383161064111, 0.222098461174078, 0.246604597710115, 0.272866222234942, 0.300807864308802, 0.330311013586645, 0.361212894425063, 0.393307925299997, 0.426351308209103, 2)

mx_females <-  c(0.00524012265089744, 0.000339120346902692, 0.000207559730911797, 0.000159227886920614, 0.00013886369586057, 0.000125580086479857, 0.00011296452342201, 0.000103627847578364, 9.66962070873392e-05, 9.23830409589621e-05, 9.20227896354737e-05, 9.79006698792667e-05, 0.000112700631045994, 0.000137946463570702, 0.000171677727571921, 0.000210010552220568, 0.000250241011337613, 0.000292592960306932, 0.000335718649635885, 0.000379106572761375, 0.00042454065267701, 0.000470686099303624, 0.000513230686633576, 0.000550136379263216, 0.000582849797204633, 0.000613647435144302, 0.000645897492221801, 0.000681963143356301, 0.00072475629946073, 0.000774065667393091, 0.000828802707927321, 0.000885124654154298, 0.000939948115455695, 0.000989725706659925, 0.00103608252462623, 0.00108723799514259, 0.00114485308808382, 0.00120364229799183, 0.00126487158421242, 0.00133329280031741, 0.00141524656268869, 0.00151415197577155, 0.00162765013146068, 0.00175186664290626, 0.00188512105642345, 0.00202682333981319, 0.00218499506245125, 0.00236872839900314, 0.00258718866584483, 0.00284029771747776, 0.00310982953681651, 0.00339626600538743, 0.0037183571597659, 0.00407445717768938, 0.00445114564192507, 0.00484100858179032, 0.00523467111947442, 0.00562908909456032, 0.00602950738303717, 0.0064498262247523, 0.00690419316248465, 0.00739846710511353, 0.00793474532467044, 0.00851704506480643, 0.00915256280842694, 0.00984116792193356, 0.0106238644233542, 0.011502264044528, 0.0125520306882868, 0.0137529996532802, 0.0149927845584978, 0.0166669784424688, 0.0183775943444499, 0.0202132268465863, 0.0221456074984001, 0.0246213899474191, 0.0272652783023258, 0.0303389731704115, 0.03398044466396, 0.0377647763610001, 0.0423563956451932, 0.0472398213311441, 0.053051707497627, 0.0603552108597202, 0.0678099066625772, 0.0756092034305041, 0.0845435649625891, 0.0960159429837782, 0.108859986998836, 0.123188032818712, 0.139107536064602, 0.156716638196786, 0.176098790007306, 0.197317202505911, 0.220408286428717, 0.24537557432178, 0.272183630486781, 0.300753278894621, 0.330958055782045, 0.362623251869918, 2)


# trapez approx of life expectancy from a logmx schedule over ages 0..99
estimate_life_expectancy_at_birth <- function(logmx) {
    mx = exp(logmx)
    px = exp(-mx)
    lx = c(1,cumprod(px))
    return( sum(head(lx,-1) + tail(lx,-1)) / 2)
}

# let's check if our life expectancy estimates for US males and females match those by CDC

e0_m <- mx_males %>% log %>% estimate_life_expectancy_at_birth 
e0_m # 76.1

e0_f <- mx_females %>% log %>% estimate_life_expectancy_at_birth 
e0_f # 81.1

# nice! move on

# next only for males 
# life expectancy with wrong RR applied 
e0_m_wrong <- mx_males %>% multiply_by(rr_wrong) %>%  log %>% estimate_life_expectancy_at_birth 
e0_m_wrong

# life expectancy with correct RR applied 
e0_m_correct <- mx_males %>% multiply_by(rr_correct) %>%  log %>% estimate_life_expectancy_at_birth 
e0_m_correct

# How much decrease in life expectancy change the correction brings
(e0_m_correct - e0_m) / (e0_m_wrong - e0_m)


# same for females
# life expectancy with wrong RR applied 
e0_f_wrong <- mx_females %>% multiply_by(rr_wrong) %>%  log %>% estimate_life_expectancy_at_birth 
e0_f_wrong

# life expectancy with correct RR applied 
e0_f_correct <- mx_females %>% multiply_by(rr_correct) %>%  log %>% estimate_life_expectancy_at_birth 
e0_f_correct

# How much decrease in life expectancy change the correction brings
(e0_f_correct - e0_f) / (e0_f_wrong - e0_f)

# the corrected amount of YLL
5.53 * .117
