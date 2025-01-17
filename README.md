### Personalised & Optimised Therapy (POT) algorithm using five cognitive and behavioural skills for subthreshold depression: Precision medicine based on the largest randomised controlled trial of smartphone CBT for depression (n=4469) in the general population (RESiLIENT trial)

Toshi A. Furukawa†, Hisashi Noma† (equally contributed), Aran Tajika, Rie Toyomoto, Masatsugu Sakata, Yan Luo, Masaru Horikoshi, Tatsuo Akechi, Norito Kawakami, Takeo Nakayama, Naoki Kondo, Shingo Fukuma,
James M. S. Wason, Ronald C. Kessler, Wolfgang Lutz, and Pim Cuijpers

---

This repository contains the R codes used for the statistical analyses of the article "Personalised & Optimised Therapy (POT) algorithm using five cognitive and behavioural skills for subthreshold depression: Precision medicine based on the largest randomised controlled trial of smartphone CBT for depression (n=4469) in the general population (RESiLIENT trial)" (Furukawa et al., 2025+). The protocol paper and statistical analysis plan document are published as Furukawa et al. (2023) and Noma et al. (2024).

The "Codes" directory of this repository contains:  
-- `Table_2.r`: R code to generate the results in Tables 2;  
-- `Table_3a_01.r`: R code to generate the results in Table 3a (Model 1);  
-- `Table_3a_02.r`: R code to generate the results in Table 3a (Model 2);  
-- `Table_3a_03.r`: R code to generate the results in Table 3a (Model 3);  
-- `Table_3b_01.r`: R code to generate the results in Table 3b (Model 1);  
-- `Table_3b_02.r`: R code to generate the results in Table 3b (Model 2);  
-- `Table_3b_03.r`: R code to generate the results in Table 3b (Model 3);  
-- `Table_4.r`: R code to generate the results in Table 4;  
-- `e-Table_1a.r`: R code to generate the results in e-Table 1 (RIDGE) of Supplementary Data;  
-- `e-Table_1b.r`: R code to generate the results in e-Table 1 (LASSO) of Supplementary Data;  
-- `e-Table_1c.r`: R code to generate the results in e-Table 1 (Elastic-net) of Supplementary Data;  
-- `e-Table_1d.r`: R code to generate the results in e-Table 1 (SVM) of Supplementary Data;  
-- `e-Table_1e.r`: R code to generate the results in e-Table 1 (causal forest) of Supplementary Data;  
-- `e-Table_2a.r`: R code to generate the results in e-Table 2a of Supplementary Data;  
-- `e-Table_2b.r`: R code to generate the results in e-Table 2b of Supplementary Data;  
-- `e-Table_3a.r`: R code to generate the results in e-Table 3a of Supplementary Data;  
-- `e-Table_3b.r`: R code to generate the results in e-Table 3b of Supplementary Data;  
-- `e-Table_3c.r`: R code to generate the results in e-Table 3c of Supplementary Data;  
-- `e-Table_4a.r`: R code to generate the results in e-Table 4a of Supplementary Data;  
-- `e-Table_4b.r`: R code to generate the results in e-Table 4b of Supplementary Data;  
-- `e-Table_4c.r`: R code to generate the results in e-Table 4c of Supplementary Data;  
-- `e-Table_5.r`: R code to generate the results in e-Table 5 of Supplementary Data.

We used the R packages glmnet ver. 4.1-8 (Friedman et al., 2023) for RIDGE, LASSO, and elastic-net, e1071 ver. 1.7-16 (Meyer et al., 2024) for support vector machine, grf ver. 2.4.0 (Tibshirani et al., 2024) for causal forest, and mice ver. 3.17.0 (van Buuren et al., 2024) for the multiple imputation analyses. Also, for the MMRM (mixed-effects models for repeated measurements) analyses, we used the R packages mmrm ver. 0.3.11 (Sabanes Bove et al., 2024) and emmeans ver. 1.9.0 (Lenth et al., 2023).

---

**References**:  
Furukawa, T. A., Noma, H., Tajika, A., et al. (2025+). Personalised & Optimised Therapy (POT) algorithm using five cognitive and behavioural skills for subthreshold depression: Precision medicine based on the largest randomised controlled trial of smartphone CBT for depression (n=4469) in the general population (RESiLIENT trial). Forthcoming.  
Furukawa, T. A., Tajika, A., Sakata, M., et al. (2023). Four 2x2 factorial trials of smartphone CBT to reduce subthreshold depression and to prevent new depressive episodes among adults in the community-RESiLIENT trial (Resilience Enhancement with Smartphone in LIving ENvironmenTs): a master protocol. *BMJ Open* **13**, e067850. (https://doi.org/10.1136/bmjopen-2022-067850)  
Friedman, J., Hastie, T., Tibshirani, R., et al. (2023). glmnet: Lasso and Elastic-Net Regularized Generalized Linear Models. R package version 4.1-8. (https://doi.org/10.32614/CRAN.package.glmnet)  
Lenth, R. V., Banfai, B., Bolker, B., et al. (2023). emmeans: Estimated Marginal Means, aka Least-Squares Means. R package version 1.9.0. (https://doi.org/10.32614/CRAN.package.emmeans)  
Meyer, D., Dimitriadou, E., Hornik, K., et al. (2024). e1071: Misc Functions of the Department of Statistics, Probability Theory Group (Formerly: E1071), TU Wien. R package version 1.7-16. (https://doi.org/10.32614/CRAN.package.e1071)  
Noma, H., Furukawa, T. A., Tajika, A., et al. (2024). RESiLIENT (Resilience Enhancement with Smartphone in LIving ENvironmenTs) Trial: The Statistical Analysis Plan. medRxiv 2024.03.16.24304261. (https://doi.org/10.1101/2024.03.16.24304261)  
Sabanes Bove, D., Li, L., Dedic, J., et al. (2024). mmrm: Mixed Models for Repeated Measures. R package version 0.3.11. (https://doi.org/10.32614/CRAN.package.mmrm)  
Tibshirani, J., Athey, S., Friedberg, R., et al. (2024). grf: Generalized Random Forests. R package version 2.4.0. (https://doi.org/10.32614/CRAN.package.grf)  
van Buuren, S., Groothuis-Oudshoorn, K., Vink, G., et al. (2024). mice: Multivariate Imputation by Chained Equations. R package version 3.17.0. (https://doi.org/10.32614/CRAN.package.mice) 
