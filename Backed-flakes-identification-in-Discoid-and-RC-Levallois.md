# The identification of core edge flakes and pseudo-Levallois points in Levallois and Discoid knapping strategies

Guillermo Bustos-Pérez<sup>1</sup>  
Francesca Romagnoli<sup>1</sup>

<sup>1</sup>Universidad Autónoma de Madrid. Departamento de Prehistoria
y Arqueología, Campus de Cantoblanco, 28049 Madrid, Spain

## Abstract

Backed flakes (core edge flakes and pseudo-Levallois points) represent a
special product of Middle Paleolithic flaking strategies. This especial
character results from their role as a technological objective and as
the management of core convexity aiming to retain the geometric
properties of the core in a knapping strategy. On Middle Paleolithic
contexts these backed implements are commonly the result of Levallois
and Discoid knapping strategies. Both backed implements are common to
both knapping methods and in many occasions they share common geometric
and morphological features which hinders their adscription to one of the
methods. This study examines the identification of two knapping
strategies (Discoid and Levallois recurrent centripetal) on an
experimental assemblage of backed products (including all stages of
reduction) based on their morphological features. 3D Geometric
Morphometrics are employed to quantify morphological variability among
the experimental sample. Dimensionality reduction though Principal
Component Analysis is combined with eleven Machine Learning models for
the identification of knapping methods. The best model (SVM with
polynomial kernel) presented a general accuracy of 0.76 and an Area
Under the Curve of 0.8. This indicates that Geometric Morphometrics,
Principal Component Analysis and Machine Learning models succeed in
capturing the morphological differences of backed products according to
the knapping method.

**Key words**: lithic analysis; Levallois; Discoid; Geometric
Morphometrics; Machine Learning; Deep Learning

## 1. Introduction

The Middle Paleolithic in Western Europe is characterized by the
diversification and increase of knapping methods resulting in
flake-dominated assemblages ([Delagnes and Meignen,
2006](#ref-hovers_diversity_2006); [Kuhn, 2013](#ref-kuhn_roots_2013)).
Two of the most common flake production methods are the Discoid and the
Levallois recurrent centripetal.  
Following Boëda ([1995](#ref-dibble_levallois:_1995),
[1994](#ref-boeda_concept_1994), [1993](#ref-boeda_debitage_1993)),
there are six technological defining criteria defining the Discoid
method:

1.  The volume of the core is conceived as two oblique asymmetric convex
    surfaces delimited by an intersection plane;  
2.  These two surfaces are not hierarchical being possible to alternate
    the roles of percussion and exploitation surfaces;  
3.  The peripheral convexity of the debitage surface is managed to
    control lateral and distal extractions thus allowing for a degree of
    predetermination;  
4.  Surfaces of the striking planes are oriented in a way that the core
    edge is perpendicular to the predetermined products;  
5.  The planes of extraction of the products are secant;  
6.  The technique employed is the direct percussion with hard hammer.

Also, according to Boëda ([1994](#ref-boeda_concept_1994),
[1993](#ref-boeda_debitage_1993)) six characteristics define the
Levallois knapping strategy from a technological point of view:

1.  The volume of the core is conceived in two convex asymmetric
    surfaces;  
2.  These two surfaces are hierarchical and are not interchangeable.
    They maintain their role of striking and debitage (or exploitation)
    surface respectively along the whole reduction process;  
3.  The distal and lateral convexities of the debitage surface are
    maintained to obtain predetermined flakes;  
4.  The plane of fracture of the predetermined products is parallel to
    the intersection between both surfaces;  
5.  The striking platform is perpendicular to the overhang (the core
    edge, at the intersection between the two core surfaces);  
6.  The technique employed during the knapping process is the direct
    percussion with hard hammer.

Depending on the organization of the debitage surface Levallois cores
are usually classified into preferential method (were a single
predetermined Levallois flake is obtained from the debitage surface) or
recurrent methods (were several predetermined flakes are produced from
the debitage surface) with removals being either unidirectional,
bidirectional or centripetal ([Boëda,
1995](#ref-dibble_levallois:_1995); [Delagnes,
1995](#ref-dibble_variability_1995); [Delagnes and Meignen,
2006](#ref-hovers_diversity_2006)). Both knapping methods share the
production of backed products which usually includes two wide
categories: core edge flakes (*eclat débordant*) and pseudo-Levallois
points.  
Core edge flakes / *eclat débordant* ([Beyries and Boëda,
1983](#ref-beyries_etude_1983); [Boëda, 1993](#ref-boeda_debitage_1993);
[Boëda et al., 1990](#ref-boeda_identification_1990)) are technical
backed knives which have a cutting edge opposite and parallel to a blunt
margin (which usually haves an angle close to the 90º). This blunt
margin commonly results from the removal of one of the laterals of the
core and can be plain, keep the scars from previous removals or be
cortical. Core edge flakes are also divided into two categories:
“classical core edge flakes” and “core edge flakes with a limited back.”
“Classical core edge flakes”

### 1.1 Loading the data and packages

``` r
list.of.packages <- c("tidyverse", "caret",  "ranger")

lapply(list.of.packages, library, character.only = TRUE)
```

    ## [[1]]
    ##  [1] "forcats"   "stringr"   "dplyr"     "purrr"     "readr"     "tidyr"    
    ##  [7] "tibble"    "ggplot2"   "tidyverse" "stats"     "graphics"  "grDevices"
    ## [13] "utils"     "datasets"  "methods"   "base"     
    ## 
    ## [[2]]
    ##  [1] "caret"     "lattice"   "forcats"   "stringr"   "dplyr"     "purrr"    
    ##  [7] "readr"     "tidyr"     "tibble"    "ggplot2"   "tidyverse" "stats"    
    ## [13] "graphics"  "grDevices" "utils"     "datasets"  "methods"   "base"     
    ## 
    ## [[3]]
    ##  [1] "ranger"    "caret"     "lattice"   "forcats"   "stringr"   "dplyr"    
    ##  [7] "purrr"     "readr"     "tidyr"     "tibble"    "ggplot2"   "tidyverse"
    ## [13] "stats"     "graphics"  "grDevices" "utils"     "datasets"  "methods"  
    ## [19] "base"

``` r
rm(list.of.packages)
```

``` r
# Loading landmarks coordinates
load("Data/Flakes LM rotated.RData")

# Loading manual attributes
Att <- read.csv("Data/Attributes data.csv")
```

## 2. Methods

### 2.1 Experimental assemblage

The experimental assemblage of the present study is the result of nine
discrete knapping sequences. Seven cores were knapped on Bergerac flint
([Fernandes et al., 2012](#ref-fernandes_silex_2012)) and two cores were
knapped on Miocene flint from South of Madrid ([Bustillo et al.,
2012](#ref-bustillo_caracterizacion_2012); [Bustillo and Pérez-Jiménez,
2005](#ref-bustillo_caracteristicas_2005)). Five cores were knapped
following the Discoid *“sensu stricto”* which highly corresponds to
Boëda’s original technological definition of the knapping system
([Boëda, 1995](#ref-dibble_levallois:_1995),
[1994](#ref-boeda_concept_1994), [1993](#ref-boeda_debitage_1993)) and
five experimental cores were knapped following the Levallois recurrent
centripetal system ([Boëda, 1995](#ref-dibble_levallois:_1995),
[1994](#ref-boeda_concept_1994), [1993](#ref-boeda_debitage_1993);
[Lenoir and Turq, 1995](#ref-dibble_recurrent_1995)). A total of 139
unretouched backed flakes (independent of the type of termination) were
obtained (70 belonging to Discoid reduction sequences and 69 belonging
to Levallois reduction sequences) from the experimental knapping
sequences. In the case of Levallois recurrent centripetal cores backed
products from both surface (debitage and striking) are included.

``` r
Att %>% group_by(Strategy) %>% 
  count(CORTEX) %>% 
  mutate(Percentage = round(n/sum(n)*100, 2)) %>%
  ggplot(aes(CORTEX, Percentage, fill = Strategy)) +
  geom_col(position = "dodge") +
  ggsci::scale_fill_aaas() +
  xlab(NULL) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            vjust= -0.2, size = 2.5,
            position = position_dodge(.9)) +
  geom_text(aes(label = paste("n =", n)), 
            vjust = "top", size = 2.5,
            position = position_dodge(.9)) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(color = "black", size = 8))
```

![](Backed-flakes-identification-in-Discoid-and-RC-Levallois_files/figure-markdown_github/Cortex%20per%20Method-1.png)

## 3 Results

### 3.1 PCA and model performance

PCA results show that the 25 first principal components account for 95%
of the variance of the dataset with PC1 accounting for 21.39% of
variance and PC25 accounting for 0.36% of variance. This is an important
reduction from the original number of variables (1524) and substantially
lower than the sample (139).

``` r
# Procrustes alignment
proc <- ProcGPA(Flakes_LM, 
                CSinit = TRUE, 
                silent = FALSE)

# Extract coordinates
Proc.Rot <- proc$rotated
LM.DF <- data.frame(matrix(Proc.Rot, nrow = length(filenames), byrow = TRUE))
```

``` r
# PCA on coordinates
pca <- prcomp(LM.DF, scale. = TRUE)
summary(pca)$importance[1:3, 1:25]
```

    ##                             PC1      PC2      PC3      PC4      PC5      PC6
    ## Standard deviation     18.05288 16.62783 12.83087 10.83128 10.42072 8.299316
    ## Proportion of Variance  0.21385  0.18142  0.10803  0.07698  0.07125 0.045200
    ## Cumulative Proportion   0.21385  0.39527  0.50330  0.58028  0.65153 0.696730
    ##                            PC7      PC8      PC9     PC10     PC11     PC12
    ## Standard deviation     7.73039 7.439897 6.710911 6.173021 5.368746 4.773021
    ## Proportion of Variance 0.03921 0.036320 0.029550 0.025000 0.018910 0.014950
    ## Cumulative Proportion  0.73594 0.772260 0.801810 0.826810 0.845730 0.860670
    ##                            PC13    PC14     PC15     PC16    PC17    PC18
    ## Standard deviation     4.562145 4.44228 3.803028 3.750857 3.54599 3.23142
    ## Proportion of Variance 0.013660 0.01295 0.009490 0.009230 0.00825 0.00685
    ## Cumulative Proportion  0.874330 0.88728 0.896770 0.906000 0.91425 0.92110
    ##                            PC19    PC20     PC21     PC22     PC23    PC24
    ## Standard deviation     2.956198 2.70170 2.649925 2.516422 2.466077 2.38239
    ## Proportion of Variance 0.005730 0.00479 0.004610 0.004160 0.003990 0.00372
    ## Cumulative Proportion  0.926840 0.93163 0.936240 0.940390 0.944380 0.94811
    ##                            PC25
    ## Standard deviation     2.327281
    ## Proportion of Variance 0.003550
    ## Cumulative Proportion  0.951660

performance metrics for each of the models. On general all models
performed with accuracy values higher than 0.7 with the exception of
KNN, Naïve Bayes and decision tree with C5.0 algorithm. When considering
the two measures of overall model performance (F1 and Accuracy)
Supported Vector Machine with polynomial kernel presents the highest
performance values (F1 = 0.75 and Accuracy = 0.757). Additionally, SVM
with polynomial kernel also provides the highest values of precision.

The following code stores the PCA values along with each black ID.
Knapping method is documented for each of the experimental cores and can
be added using a `case_when()` function.

``` r
# Store PCA values in a dataframe and add ID's
PCA_Coord <- as.data.frame(pca$x)
PCA_Coord$ID <- filenames
PCA_Coord$Core <- str_sub(PCA_Coord$ID, end = 2)

# Set the core to which they belong and strategy
PCA_Coord <- PCA_Coord %>% mutate(
  Strategy = case_when(Core == "B2" | Core == "B3" |
              Core == "B4" | Core == "B5" |Core == "B6" ~ "Discoid",
            Core == "B7" | Core == "B8" |Core == "B9" | Core == "Le" ~ "Levallois" ))

# Set strategy as factor or varImp will not work
PCA_Coord$Strategy <- factor(PCA_Coord$Strategy)
```

``` r
# Set formula
frmla <- as.formula(
  paste("Strategy", paste(colnames(PCA_Coord[,1:25]), collapse = " + "), sep = " ~ "))

# Set cross validation
trControl <- trainControl(method  = "repeatedcv",
                          verboseIter = TRUE,
                          number  = 10,
                          repeats = 50,
                          savePredictions = "final",
                          classProbs = TRUE)
```

The following code trains the selected models using the established
k-fold cross validation.

``` r
# LDA model 
set.seed(123)
fit.LDA <- caret::train(frmla, 
                         PCA_Coord, 
                         method = "lda",
                         preProc = c("center", "scale"), 
                         trControl = trControl)

# KNN model
set.seed(123)
KNN.model <- caret::train(
  frmla,
  PCA_Coord,
  method = "knn",
  preProc = c("center", "scale"), 
  trControl = trControl,
  tuneGrid = expand.grid(k = seq(1, 15, 1))
)

# Logistic regression model
set.seed(123)
logmod <- caret::train(frmla, 
                       PCA_Coord, 
                       method = "glm",
                       family = "binomial",
                       preProc = c("center", "scale"),
                       trControl = trControl)

# SVM linear 
set.seed(123)
SVM_Linear <- train(frmla, 
                    PCA_Coord, 
                    method = "svmLinear",
                    preProcess = c("center","scale"),
                    trControl = trControl,
                    tuneGrid = expand.grid(C = seq(0.01, 3, length = 20)),
                    metric = "Accuracy",
                    importance = 'impurity')

# SVM Radial 
set.seed(123)
SVM_Radial <- train(frmla, 
                    PCA_Coord, 
                    method = "svmRadial",
                    preProcess = c("center","scale"),
                    trControl = trControl,
                    tuneGrid = 
                      expand.grid(C = seq(0.01, 3, length = 20),
                                  sigma = seq(0.0001, 1, length = 20)),
                    metric = "Accuracy",
                    importance = 'impurity')

# SVM Poly 
set.seed(123)
SVM_Poly <- train(frmla, 
                  PCA_Coord, 
                  method = "svmPoly",
                  preProcess = c("center","scale"),
                  trControl = trControl,
                  metric = "Accuracy",
                  tuneGrid = 
                    expand.grid(C = seq(0.01, 3, length = 15),
                                scale = seq(0.001, 1, length = 15),
                                degree = as.integer(seq(1, 3, 1))),
                  importance = 'impurity')

# Random Forest 
best_tune <- data.frame(
  mtry = numeric(0),
  Num_Trees = numeric(0),
  Split_Rule = character(0),
  Precision = numeric(0),
  Node.Size = numeric(0))

my_seq <- seq(350, 700, 25)
set.seed(123)
for (x in my_seq){
  RF_Model <- train(frmla, 
                    PCA_Coord,
                    method = "ranger",
                    trControl = trControl,
                    tuneGrid =
                      expand.grid(.mtry = seq(1, 10, 1),
                                  .min.node.size = seq(1, 6, 1),
                                  .splitrule = c("gini", "extratrees")),
                    metric = "Accuracy",
                    importance = 'impurity')

  Bst_R <- data.frame(
    mtry = RF_Model$bestTune[[1]],
    Num_Trees = x,
    Split_Rule = RF_Model$bestTune[[2]],
    Precision = max(RF_Model$results[[4]]),
    Node.Size = RF_Model$bestTune[[3]]
  )
  
  best_tune <- rbind(best_tune, Bst_R)
  
  Bst_R <- c()
}

# Best tune 
# mtry = 7; 550 trees split_Rule = extratrees; min.nod.size = 6
set.seed(123)
RF_Model <- train(
  frmla,
  PCA_Coord,
  method = "ranger",
  trControl = trControl,
  tuneGrid = expand.grid(
    .mtry = 7,
    .min.node.size = 6,
    .splitrule = "extratrees"),
  num.trees = 550,
  metric = "Accuracy",
  importance = 'impurity')

# Boosted tree 
set.seed(123)
Boost_Tree <- train(frmla, 
                  PCA_Coord,
                  method = "gbm",
                  trControl = trControl,
                  metric = "Accuracy",
                  tuneGrid = 
                    expand.grid(
                      n.trees = seq(from = 300, to = 700, by = 50),
                      interaction.depth = seq(from = 1, to = 10, length.out = 5),
                      shrinkage = 0.1,
                      n.minobsinnode = as.integer(seq(1, 10, length = 5))))

# Multilayer ANN
set.seed(123)
mlp_Mod = train(frmla, 
                PCA_Coord, 
                method = "mlpML", 
                preProc =  c('center', 'scale'),
                trControl = trControl,
                tuneGrid = 
                  expand.grid(
                    layer1 = c(1:8),
                    layer2 = c(0:8),
                    layer3 = c(0:8)))

# Naive Bayes
set.seed(123)
NaiB_Model <- train(frmla, 
                    PCA_Coord,
                    method = "nb",
                    preProcess = c("scale","center"),
                    trControl = trControl,
                    metric = "Accuracy",
                    lineout = FALSE)

confusionMatrix(NaiB_Model)

# C5.0 Tree 
grid <- expand.grid(
  winnow = c(TRUE), 
  trials = seq(10, 40, by = 5), 
  model = "tree" )

set.seed(123)
C50_Mod <- train(frmla, 
                 PCA_Coord,
                 method = "C5.0",
                 trControl = trControl,
                 metric = "Accuracy",
                 importance = 'impurity')
```

``` r
data.frame(
  Model = c("LDA", "KNN", "Log. Reg.", "SVML", "SVMP", "SVMR",
            "C5.0", "Rand. Forest", "Boost Tree", "Baïve Bayes",
            "ANN") %>% 
    cbind(
  data.frame(
 rbind(
   confusionMatrix(fit.LDA$pred$pred, fit.LDA$pred$obs)[[4]],
   confusionMatrix(KNN.model$pred$pred, KNN.model$pred$obs)[[4]],
   confusionMatrix(logmod$pred$pred, logmod$pred$obs)[[4]],
   confusionMatrix(SVM_Linear$pred$pred, SVM_Linear$pred$obs)[[4]],
   confusionMatrix(SVM_Poly$pred$pred, SVM_Poly$pred$obs)[[4]],
   confusionMatrix(SVM_Radial$pred$pred, SVM_Radial$pred$obs)[[4]],
   confusionMatrix(C50_Mod$pred$pred, C50_Mod$pred$obs)[[4]],
   confusionMatrix(RF_Model$pred$pred, RF_Model$pred$obs)[[4]],
   confusionMatrix(Boost_Tree$pred$pred, Boost_Tree$pred$obs)[[4]],
   confusionMatrix(NaiB_Model$pred$pred, NaiB_Model$pred$obs)[[4]],
   confusionMatrix(mlp_Mod$pred$pred, mlp_Mod$pred$obs)[[4]]))
  ))
```

    ##         Model.. Model.Sensitivity Model.Specificity Model.Pos.Pred.Value
    ## 1           LDA         0.6817143         0.7666667            0.7477280
    ## 2           KNN         0.3328571         0.8881159            0.7511283
    ## 3     Log. Reg.         0.6991429         0.7339130            0.7271917
    ## 4          SVML         0.6840000         0.7976812            0.7742561
    ## 5          SVMP         0.7234286         0.7904348            0.7778802
    ## 6          SVMR         0.7325714         0.7159420            0.7234763
    ## 7          C5.0         0.6602857         0.6571014            0.6614196
    ## 8  Rand. Forest         0.7071429         0.7417391            0.7352941
    ## 9    Boost Tree         0.7254286         0.7388406            0.7380814
    ## 10  Baïve Bayes         0.6702857         0.7246377            0.7117718
    ## 11          ANN         0.6945714         0.7176812            0.7139501
    ##    Model.Neg.Pred.Value Model.Precision Model.Recall  Model.F1 Model.Prevalence
    ## 1             0.7036446       0.7477280    0.6817143 0.7131968        0.5035971
    ## 2             0.5675125       0.7511283    0.3328571 0.4612948        0.5035971
    ## 3             0.7062762       0.7271917    0.6991429 0.7128915        0.5035971
    ## 4             0.7133230       0.7742561    0.6840000 0.7263350        0.5035971
    ## 5             0.7380244       0.7778802    0.7234286 0.7496669        0.5035971
    ## 6             0.7251908       0.7234763    0.7325714 0.7279955        0.5035971
    ## 7             0.6559606       0.6614196    0.6602857 0.6608522        0.5035971
    ## 8             0.7140067       0.7352941    0.7071429 0.7209438        0.5035971
    ## 9             0.7262108       0.7380814    0.7254286 0.7317003        0.5035971
    ## 10            0.6841817       0.7117718    0.6702857 0.6904061        0.5035971
    ## 11            0.6984485       0.7139501    0.6945714 0.7041274        0.5035971
    ##    Model.Detection.Rate Model.Detection.Prevalence Model.Balanced.Accuracy
    ## 1             0.3433094                  0.4591367               0.7241905
    ## 2             0.1676259                  0.2231655               0.6104865
    ## 3             0.3520863                  0.4841727               0.7165280
    ## 4             0.3444604                  0.4448921               0.7408406
    ## 5             0.3643165                  0.4683453               0.7569317
    ## 6             0.3689209                  0.5099281               0.7242567
    ## 7             0.3325180                  0.5027338               0.6586936
    ## 8             0.3561151                  0.4843165               0.7244410
    ## 9             0.3653237                  0.4949640               0.7321346
    ## 10            0.3375540                  0.4742446               0.6974617
    ## 11            0.3497842                  0.4899281               0.7061263

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-beyries_etude_1983" class="csl-entry">

Beyries, S., Boëda, E., 1983. Étude technoloogique et traces
d’utilisation des éclats débordants de Corbehem (Pas-de-Calais).
Bulletin de la Société préhistorique française 80, 275–279.
https://doi.org/<https://doi.org/10.3406/bspf.1983.5455>

</div>

<div id="ref-dibble_levallois:_1995" class="csl-entry">

Boëda, E., 1995. Levallois: A Volumetric Construction, Methods, A
Technique, in: Dibble, H.L., Bar-Yosef, O. (Eds.), The Definition and
Interpretation of Levallois Technology, Monographs in World Archaeology.
Prehistory Press, Madison, Wisconsin, pp. 41–68.

</div>

<div id="ref-boeda_concept_1994" class="csl-entry">

Boëda, E., 1994. Le concept Levallois: Variabilité des méthodes, CNRS
éditions. CNRS.

</div>

<div id="ref-boeda_debitage_1993" class="csl-entry">

Boëda, E., 1993. Le débitage discoïde et le débitage Levallois récurrent
centripède. Bulletin de la Société Préhistorique Française 90, 392–404.
<https://doi.org/10.3406/bspf.1993.9669>

</div>

<div id="ref-boeda_identification_1990" class="csl-entry">

Boëda, E., Geneste, J.-M., Meignen, L., 1990. Identification de chaînes
opératoires lithiques du Paléolithique ancien et moyen. Paléo 2, 43–80.

</div>

<div id="ref-bustillo_caracteristicas_2005" class="csl-entry">

Bustillo, M.A., Pérez-Jiménez, J.L., 2005. Características diferenciales
y génesis de los niveles silíceos explotados en el yacimiento
arqueológico de Casa Montero (Vicálvaro, Madrid). Geogaceta 38, 243–246.

</div>

<div id="ref-bustillo_caracterizacion_2012" class="csl-entry">

Bustillo, M.Á., Pérez-Jiménez, J.L., Bustillo, M., 2012. Caracterización
geoquímica de rocas sedimentarias formadas por silicificación como
fuentes de suministro de utensilios líticos (Mioceno, cuenca de Madrid).
Revista Mexicana de Ciencias Geológicas 29, 233–247.

</div>

<div id="ref-dibble_variability_1995" class="csl-entry">

Delagnes, A., 1995. Variability within Uniformity: Three Levels of
Variability within the Levallois System, in: Dibble, H.L., Bar-Yosef, O.
(Eds.), The Definition and Interpretation of Levallois Technology,
Monographs in World Archaeology. Prehistory Press, Madison, Wisconsin,
pp. 201–211.

</div>

<div id="ref-hovers_diversity_2006" class="csl-entry">

Delagnes, A., Meignen, L., 2006. Diversity of Lithic Production Systems
During the Middle Paleolithic in France. Are There Any Chronological
Trends?, in: Hovers, E., Kuhn, S.L., Jochim, M. (Eds.), Transitions
Before the Transition Evolution and Stability in the Middle Paleolithic
and Middle Stone Age. Springer, pp. 85–107.

</div>

<div id="ref-fernandes_silex_2012" class="csl-entry">

Fernandes, P., Morala, A., Schmidt, P., Séronie-Vivien, M.-R., Turq, A.,
2012. Le silex du Bergeracois: État de la question. Quaternaire
Continental d’Aquitaine, excursion AFEQ, ASF 2012 2012, 22–33.

</div>

<div id="ref-kuhn_roots_2013" class="csl-entry">

Kuhn, S.L., 2013. Roots of the Middle Paleolithic in Eurasia. Current
Anthropology 54, S255–S268. <https://doi.org/10.1086/673529>

</div>

<div id="ref-dibble_recurrent_1995" class="csl-entry">

Lenoir, M., Turq, A., 1995. Recurrent Centripetal Debitage (Levallois
and Discoidal): Continuity or Discontinuity?, in: Dibble, H.L.,
Bar-Yosef, O. (Eds.), The Definition and Interpretation of Levallois
Technology, Monographs in World Archaeology. Prehistory Press, Madison,
Wisconsin, pp. 249–256.

</div>

</div>
