# Churn Modelling with Linear Models

Churn modelling based on excercise in Applied Predictive Modelling by Kuhn and Johnson

### Session Info:

R version 3.2.2 (2015-08-14)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 15.04

locale:
 [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C               LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8
 [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8    LC_PAPER=en_GB.UTF-8       LC_NAME=C
 [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C

attached base packages:
[1] parallel  grid      stats     graphics  grDevices utils     datasets  methods   base

other attached packages:
 [1] sparseLDA_0.1-6                 mda_0.4-7                       class_7.3-14
 [4] elasticnet_1.1                  lars_1.2                        glmnet_2.0-2
 [7] Matrix_1.2-2                    pls_2.4-3                       MASS_7.3-40
[10] doMC_1.3.3                      iterators_1.0.7                 foreach_1.4.2
[13] ggbiplot_0.55                   scales_0.3.0                    plyr_1.8.3
[16] corrplot_0.73                   subselect_0.12-5                e1071_1.6-4
[19] C50_0.1.0-24                    Hmisc_3.16-0                    Formula_1.2-1
[22] survival_2.38-3                 AppliedPredictiveModeling_1.1-6 caret_6.0-52
[25] ggplot2_1.0.1                   lattice_0.20-33                 FUNctions_1.0
[28] dplyr_0.4.1

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.1         gtools_3.4.2        assertthat_0.1      digest_0.6.8        BradleyTerry2_1.0-6
 [6] acepack_1.3-3.3     stats4_3.2.2        lazyeval_0.1.10     minqa_1.2.4         SparseM_1.6
[11] car_2.0-25          nloptr_1.0.4        combinat_0.0-8      rpart_4.1-10        partykit_1.0-1
[16] proto_0.3-10        labeling_0.3        splines_3.2.2       lme4_1.1-8          stringr_1.0.0
[21] foreign_0.8-66      munsell_0.4.2       compiler_3.2.2      mgcv_1.8-7          nnet_7.3-9
[26] gridExtra_0.9.1     CORElearn_1.47.1    codetools_0.2-14    brglm_0.5-9         nlme_3.1-122
[31] gtable_0.1.2        DBI_0.3.1           magrittr_1.5        pROC_1.8            stringi_0.5-5
[36] reshape2_1.4.1      latticeExtra_0.6-26 klaR_0.6-12         RColorBrewer_1.1-2  tools_3.2.2
[41] pbkrtest_0.4-2      colorspace_1.2-6    cluster_2.0.3       quantreg_5.11
