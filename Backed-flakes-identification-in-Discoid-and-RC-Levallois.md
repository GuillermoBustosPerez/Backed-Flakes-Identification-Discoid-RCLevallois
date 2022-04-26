# The identification of core edge flakes and pseudo-Levallois points in Levallois and Discoid knapping strategies

Guillermo Bustos-Pérez<sup>1</sup>  
Brad Gravina<sup>2, 3</sup>  
Michel Brenet<sup>3, 4</sup>  
Francesca Romagnoli<sup>1</sup>

<sup>1</sup>Universidad Autónoma de Madrid. Departamento de Prehistoria
y Arqueología, Campus de Cantoblanco, 28049 Madrid, Spain  
<sup>2</sup>Musée national de Préhistoire, MCC, 1 rue du Musée, 24260
Les Eyzies de Tayac, France  
<sup>3</sup>UMR-5199 PACEA, Université de Bordeaux, Bâtiment B8, Allée
Geoffroy Saint Hilaire, CS 50023, 33615 PESSAC CEDEX, France  
<sup>4</sup>INRAP Grand Sud-Ouest, Centre mixte de recherches
archéologiques, Domaine de Campagne, 242460 Campagne, France

Corresponding authors:  
G.B.P. <guillermo.willbustos@mail.com>  
F.R. <f.romagnoli2@gmail.com>

## Abstract

Backed flakes (core edge flakes and pseudo-Levallois points) represent
special products of Middle Paleolithic centripetal flaking strategies.
Their peculiarities are due to their roles as both a technological
objective and in the management of core convexities to retain its
geometric properties during reduction. In Middle Paleolithic contexts,
these backed implements are commonly produced within Levallois and
discoidal reduction sequences. Often, Levallois and discoidal backed
implements show common geometric and morphological features that
complicate their attribution to one of these methods. This study
examines the identification of experimentally produced discoidal and
recurrent centripetal Levallois backed products (including all stages of
reduction) based on their morphological features. 3D geometric
morphometrics are employed to quantify morphological variability among
the experimental sample. Dimensionality reduction though principal
component analysis is combined with 11 machine learning models for the
identification of knapping methods. A supported vector machine with
polynomial kernel has been identified as the best model (with a general
accuracy of 0.76 and an area under the curve \[AUC\] of 0.8). This
indicates that combining geometric morphometrics, principal component
analysis, and machine learning models succeeds in capturing the
morphological differences of backed products according to the knapping
method.

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
“Classical core edge flakes” ([Beyries and Boëda,
1983](#ref-beyries_etude_1983); [Boëda, 1993](#ref-boeda_debitage_1993);
[Boëda et al., 1990](#ref-boeda_identification_1990)), which are
sometimes referred to as “core edge flakes with a non-limited
back”/*“éclat débordant à dos non limité”* ([Duran,
2005](#ref-duran_lindustrie_2005); [Duran and Soler,
2006](#ref-duran_variabilite_2006)), have a morphological axis more or
less similar to the axis of percussion. “Core edge flakes with a limited
back”/*“éclat débordant à dos limité”* have a deviated axis of symmetry
regarding the axis of percussion ([Meignen,
1996](#ref-meignen_persistance_1996); [Meignen,
1993](#ref-meignen_les_1993); [Pasty et al.,
2004](#ref-pasty_etude_2004)). Usually, because of this deviation, the
back is not parallel and does not span the entire length of the sharp
edge or the percussion axis (Slimak, 2003).  
Pseudo-Levallois points ([Boëda, 1993](#ref-boeda_debitage_1993); [Boëda
et al., 1990](#ref-boeda_identification_1990); [Bordes,
1961](#ref-bordes_typologie_1961), [1953](#ref-bordes_notules_1953);
[Slimak, 2003](#ref-peresani_les_2003)) are backed products where the
edge opposite to the back has a triangular morphology. This triangular
morphology is usually the result of the convergence of two or more
scars. As with core edge flakes, the back usually results from the
removal of one of the lateral edges of the core and can be plain, retain
the scars from previous removals, or more rarely be cortical. Both
pseudo-Levallois points and core edge flakes with a limited back share
the deviation of symmetry from the axis of percussion but are clearly
differentiable due to their morphology. The present study includes the
three categories defined above as backed products.  
Depending on the knapping method, different roles in Levallois recurrent
centripetal and discoidal debitage are attributed to core edge flakes
and pseudo-Levallois points. [Boëda et
al.](#ref-boeda_identification_1990)
([1990](#ref-boeda_identification_1990)) focus on the role of core edge
flakes and cortically backed flakes for maintaining the lateral
convexities throughout Levallois recurrent centripetal reduction.
Similarly, pseudo-Levallois points contribute to maintaining the lateral
and distal convexities between different series of removals ([Boëda et
al., 1990](#ref-boeda_identification_1990)).  
Focusing on the variability of discoidal, debitage
[Slimak](#ref-peresani_les_2003) ([2003](#ref-peresani_les_2003)) points
out that pseudo-Levallois points are short products that induce a
limited lowering of the core overhang (the intersection between the
striking and debitage surfaces). In contrast, core edge flakes can
result from several distinct production objectives. Expanding on the
roles of pseudo-Levallois points and core edge flakes within discoidal
debitage, [Locht](#ref-peresani_industrie_2003)
([2003](#ref-peresani_industrie_2003)) demonstrated the systematic
production of both products at the site of Beauvais. This indicates that
at Beauvais, core edge flakes and pseudo-Levallois points were the main
predetermining/predetermined products ([Locht,
2003](#ref-peresani_industrie_2003)).  
An additional added value of core edge flakes and pseudo-Levallois
points is their frequent transport by Paleolithic groups. [Turq et
al.](#ref-turq_fragmented_2013) ([2013](#ref-turq_fragmented_2013))
described the widespread import and export of lithic artifacts during
the Middle Paleolithic. Examples of the transport of pseudo-Levallois
points from discoidal production sequences can be observed at
Combemenue, La Mouline, Les Fieux ([Brenet,
2013](#ref-brenet_variabilite_2013), [2012](#ref-brenet_silex_2012);
[Brenet and Cretin, 2008](#ref-brenet_gisement_2008); [Folgado and
Brenet, 2010](#ref-folgado_economie_2010); [Turq et al.,
2013](#ref-turq_fragmented_2013)), and the open-air site of Bout des
Vergnes ([Courbin et al., 2020](#ref-courbin_spatial_2020)), while the
transport of core edge flakes (into and out of the site) is also clearly
observed at la Grotte Vaufrey ([Geneste, 1988](#ref-rigaud_les_1988))
and at Site N of Maastricht-Belvédère ([Roebroeks et al.,
1992](#ref-roebroeks_veil_1992)). Transported backed pieces have also
been clearly identified at Abric Romaní in Spain within both Levallois
and discoidal production methods ([Martín-Viveros et al.,
2020](#ref-martin-viveros_use-wear_2020); [Romagnoli et al.,
2016](#ref-romagnoli_testing_2016)).

![Backed products from the experimental sample: core edge flakes (1–2)
and pseudo-Levallois points (3–4) from the Discoid knapping method. Core
edge flakes (5–6) and pseudo-Levallois points (7–8) from the Levallois
recurrent centripetal method.](Article%20Figures/01%20Materials.png)

A problem exists in the attribution of backed pieces to either discoidal
or recurrent centripetal Levallois reduction.
[Mourre](#ref-peresani_discoiou_2003)
([2003](#ref-peresani_discoiou_2003)) indicates that a key aspect for
the identification of Levallois core edge flakes is the direction of the
debitage axis, which is parallel to the intersection plane of the two
core surfaces while the fracture plane is secant.
[Slimak](#ref-slimak_variabilite_1998)
([1998](#ref-slimak_variabilite_1998)) indicates that core edge flakes
from the discoidal method might have fracture planes parallel to the
intersection between the debitage and striking surfaces although not as
parallel as in Levallois debitage. [Delpiano et
al.](#ref-delpiano_techno-functional_2021)
([2021](#ref-delpiano_techno-functional_2021)) indicate that Levallois
artifacts tend to be more elongated with thinner and sub-parallel edges,
whereas discoidal backed products show higher variation in the minimum
and maximum thickness of the back.  
This raises the question as to the extent to which Discoid and Levallois
recurrent centripetal core edge flakes and pseudo-Levallois points can
be differentiated based on their morphological features. This issue is
relevant in lithic studies because it affects the technological analysis
of a stone tool assemblage and the evolutionary interpretation of
knapping concepts over time. In this paper, we address this issue
through experimental archaeology and a multi-level statistical approach.
We reproduced classic bifacial discoidal and recurrent centripetal
Levallois reduction sequences to obtain a collection of backed products.
3D scanning of lithic artifacts and geometric morphometrics was employed
to quantify the morphological variability of the experimental sample and
the cores were refit. On the set of coordinates, dimensionality
reduction through principal component analysis (PCA) was carried out,
and 11 machine learning models were tested to obtain classification
accuracy and variable importance.

## 2 Methods

### 2.1 Experimental assemblage

The analyzed experimental assemblage derives from the replication of
nine discrete knapping sequences. Seven cores were knapped in Bergerac
chert ([Fernandes et al., 2012](#ref-fernandes_silex_2012)), and two
cores were knapped in Miocene chert from South of Madrid ([Bustillo et
al., 2012](#ref-bustillo_caracterizacion_2012); [Bustillo and
Pérez-Jiménez, 2005](#ref-bustillo_caracteristicas_2005)).

### 2.3 Loading the data and packages

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
# Cortex per method in backed flakes
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
proc <- Morpho::ProcGPA(Flakes_LM, 
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

SVM with polynomial kernel is closely followed by SVM with linear kernel
which presents the second highest value of accuracy (0.741), the fourth
highest value of F1 (0.726) and the second highest value of precision
(0.774). Outside SVM with different kernels, the Boosted trees also
presents high values of accuracy (0.732), F1 (0.732) and precision
(0.738). KNN presented the lowest values on the general performance
metrics with an accuracy value of 0.61 and a very low F1 score (0.461).
KNN does seem to present high values of precision (0.751) and the
highest value of specificity (0.888) although these are clearly the
result of a sensitivity (0.333) lower than the no-information ratio
(0.504).

``` r
# Data frame of models performance
knitr::kable(data.frame(
  Model = c("LDA", "KNN", "Log. Reg.", "SVML", "SVMP", "SVMR",
            "C5.0", "Rand. Forest", "Boost Tree", "Baïve Bayes",
            "ANN") %>% 
    cbind(
  data.frame(
 rbind(
   round(confusionMatrix(fit.LDA$pred$pred, fit.LDA$pred$obs)[[4]][c(1,2,5,7,11)],3),
   round(confusionMatrix(KNN.model$pred$pred, KNN.model$pred$obs)[[4]][c(1,2,5,7,11)],3),
   round(confusionMatrix(logmod$pred$pred, logmod$pred$obs)[[4]][c(1,2,5,7,11)],3),
   round(confusionMatrix(SVM_Linear$pred$pred, SVM_Linear$pred$obs)[[4]][c(1,2,5,7,11)],3),
   round(confusionMatrix(SVM_Poly$pred$pred, SVM_Poly$pred$obs)[[4]][c(1,2,5,7,11)],3),
   round(confusionMatrix(SVM_Radial$pred$pred, SVM_Radial$pred$obs)[[4]][c(1,2,5,7,11)],3),
   round(confusionMatrix(C50_Mod$pred$pred, C50_Mod$pred$obs)[[4]][c(1,2,5,7,11)],3),
   round(confusionMatrix(RF_Model$pred$pred, RF_Model$pred$obs)[[4]][c(1,2,5,7,11)],3),
   round(confusionMatrix(Boost_Tree$pred$pred, Boost_Tree$pred$obs)[[4]][c(1,2,5,7,11)],3),
   round(confusionMatrix(NaiB_Model$pred$pred, NaiB_Model$pred$obs)[[4]][c(1,2,5,7,11)],3),
   round(confusionMatrix(mlp_Mod$pred$pred, mlp_Mod$pred$obs)[[4]][c(1,2,5,7,11)],3)))
  )))
```

| Model..      | Model.Sensitivity | Model.Specificity | Model.Precision | Model.F1 | Model.Balanced.Accuracy |
|:-------------|------------------:|------------------:|----------------:|---------:|------------------------:|
| LDA          |             0.682 |             0.767 |           0.748 |    0.713 |                   0.724 |
| KNN          |             0.333 |             0.888 |           0.751 |    0.461 |                   0.610 |
| Log. Reg.    |             0.699 |             0.734 |           0.727 |    0.713 |                   0.717 |
| SVML         |             0.684 |             0.798 |           0.774 |    0.726 |                   0.741 |
| SVMP         |             0.723 |             0.790 |           0.778 |    0.750 |                   0.757 |
| SVMR         |             0.733 |             0.716 |           0.723 |    0.728 |                   0.724 |
| C5.0         |             0.660 |             0.657 |           0.661 |    0.661 |                   0.659 |
| Rand. Forest |             0.707 |             0.742 |           0.735 |    0.721 |                   0.724 |
| Boost Tree   |             0.725 |             0.739 |           0.738 |    0.732 |                   0.732 |
| Baïve Bayes  |             0.670 |             0.725 |           0.712 |    0.690 |                   0.697 |
| ANN          |             0.695 |             0.718 |           0.714 |    0.704 |                   0.706 |

Evaluation of models through ROC curve and AUC shows that most models
present acceptable/fair (0.8 – 0.7) values. Again KNN presents the
lowest AUC value (0.67) being a poor value. SVM with polynomial kernel
presents the highest AUC (0.799) value being very close to be an
excellent/good model (0.9 to 0.8). Optimal probability threshold values
from the SVM with polynomial kernel are 0.501 for Discoid and 0.491 for
Levallois. General performance metrics (F1 and accuracy) and AUC values
indicate that SVM with polynomial kernel is the best model. Evaluation
of SVM with polynomial kernel confusion matrix shows a very good
distribution along the diagonal axis with the correct identification of
Levallois products being slightly higher than the correct identification
of Discoid products. Directionality of confusions shows that for the SVM
with polynomial it is more common to confuse Discoid backed products as
Levallois instead of confusing Levallois backed products as Discoid.

``` r
# LDA
temp <- pROC::roc(fit.LDA$pred$obs, fit.LDA$pred$Levallois)
Roc_Curve <- tibble(temp$specificities, temp$sensitivities)
Roc_Curve$Model <- "LDA"

# KNN
temp <- pROC::roc(KNN.model$pred$obs, KNN.model$pred$Levallois)
temp <- cbind(tibble(temp$specificities, temp$sensitivities),
              Model = "KNN")
Roc_Curve <- rbind(Roc_Curve, temp)

# Log
temp <- pROC::roc(logmod$pred$obs, logmod$pred$Levallois)
temp <- cbind(tibble(temp$specificities, temp$sensitivities),
              Model = "Log. Reg.")
Roc_Curve <- rbind(Roc_Curve, temp)

# SVML
temp <- pROC::roc(SVM_Linear$pred$obs, SVM_Linear$pred$Levallois)
temp <- cbind(tibble(temp$specificities, temp$sensitivities),
              Model = "SVML")
Roc_Curve <- rbind(Roc_Curve, temp)

# SVMP
temp <- pROC::roc(SVM_Poly$pred$obs, SVM_Poly$pred$Levallois)
temp <- cbind(tibble(temp$specificities, temp$sensitivities),
              Model = "SVMP")
Roc_Curve <- rbind(Roc_Curve, temp)

# SVMR
temp <- pROC::roc(SVM_Radial$pred$obs, SVM_Radial$pred$Levallois)
temp <- cbind(tibble(temp$specificities, temp$sensitivities),
              Model = "SVMR")
Roc_Curve <- rbind(Roc_Curve, temp)

# C5.0
temp <- pROC::roc(C50_Mod$pred$obs, C50_Mod$pred$Levallois)
temp <- cbind(tibble(temp$specificities, temp$sensitivities),
              Model = "C5.0")
Roc_Curve <- rbind(Roc_Curve, temp)

# rf
temp <- pROC::roc(RF_Model$pred$obs, RF_Model$pred$Levallois)
temp <- cbind(tibble(temp$specificities, temp$sensitivities),
              Model = "Rand. Forest")
Roc_Curve <- rbind(Roc_Curve, temp)

# Boosted tree
temp <- pROC::roc(Boost_Tree$pred$obs, Boost_Tree$pred$Levallois)
temp <- cbind(tibble(temp$specificities, temp$sensitivities),
              Model = "Boost Tree")
Roc_Curve <- rbind(Roc_Curve, temp)

# Boosted tree
temp <- pROC::roc(NaiB_Model$pred$obs, NaiB_Model$pred$Levallois)
temp <- cbind(tibble(temp$specificities, temp$sensitivities),
              Model = "Naïve Bayes")
Roc_Curve <- rbind(Roc_Curve, temp)

# Boosted tree
temp <- pROC::roc(mlp_Mod$pred$obs, mlp_Mod$pred$Levallois)
temp <- cbind(tibble(temp$specificities, temp$sensitivities),
              Model = "ANN")
Roc_Curve <- rbind(Roc_Curve, temp)
rm(temp)


aucs <- c(
  paste0("LDA (", round(pROC::auc(fit.LDA$pred$obs, fit.LDA$pred$Levallois),2) ,")"),
  paste0("KNN (", round(pROC::auc(KNN.model$pred$obs, KNN.model$pred$Levallois),2) ,")"),
  paste0("Logistic (", round(pROC::auc(logmod$pred$obs, logmod$pred$Levallois),2) ,")"),
  paste0("SVM Linear (", round(pROC::auc(SVM_Linear$pred$obs, SVM_Linear$pred$Levallois),2) ,")"),
  paste0("SVM Poly (", round(pROC::auc(SVM_Poly$pred$obs, SVM_Poly$pred$Levallois),2) ,")"),
  paste0("SVM Radial (", round(pROC::auc(SVM_Radial$pred$obs, SVM_Radial$pred$Levallois),2) ,")"),
  paste0("C5.0 (", round(pROC::auc(C50_Mod$pred$obs, C50_Mod$pred$Levallois),2) ,")"),
  paste0("Rand. Forest (", round(pROC::auc(RF_Model$pred$obs, RF_Model$pred$Levallois),2) ,")"),
  paste0("Boosted. Tree (", round(pROC::auc(Boost_Tree$pred$obs, Boost_Tree$pred$Levallois),2) ,")"),
  paste0("Naïve Bayes (", round(pROC::auc(NaiB_Model$pred$obs, NaiB_Model$pred$Levallois),2) ,")"),
  paste0("ANN (", round(pROC::auc(mlp_Mod$pred$obs, mlp_Mod$pred$Levallois),2) ,")"))
  
Roc_Curve %>% 
  ggplot(aes(`temp$specificities`, `temp$sensitivities`,
             color = Model), alpha = 0.7) +
  geom_line(size = 1.01) +
  scale_x_continuous(trans = "reverse") +
  coord_fixed() +
  theme_light() +
  xlab("Specificities") +
  ylab("Sensitivities") +
  geom_abline(intercept = 1, slope = 1)  +
  scale_color_brewer(palette = "Paired",
                     breaks = c("LDA", "KNN", "Log. Reg.",
                                "SVML", "SVMP", "SVMR", "C5.0",
                                "Rand. Forest", "Boost Tree",
                                "Naïve Bayes", "ANN"),
                     labels = aucs) +
  labs(colour = "Model (AUC)") +
  theme(
    axis.title = element_text(size = 11, color = "black", face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    legend.title = element_text(face = "bold"))
```

![](Backed-flakes-identification-in-Discoid-and-RC-Levallois_files/figure-markdown_github/Roc%20and%20AUC-1.png)

``` r
# Confusion matrix 
SVM_Poly.Confx <- confusionMatrix(SVM_Poly)$table

SVM_Poly.Confx <- reshape2::melt(SVM_Poly.Confx)

SVM_Poly.Confx$Prediction <- factor(SVM_Poly.Confx$Prediction, 
                                    levels = c(
                                      "Discoid", "Levallois"))
SVM_Poly.Confx$Reference <- factor(SVM_Poly.Confx$Reference, 
                                   levels = c(
                                     "Levallois", "Discoid"))

SVM_Poly.Confx %>% 
  ggplot(aes(Reference, Prediction, fill = value)) + 
  geom_tile(alpha = 0.75) +
  geom_text(aes(label = round(value, 2)), size = 3) +
  scale_fill_gradient(low = "white", high = "blue")  +
  scale_x_discrete(position = "top") +
  theme_bw() +
  coord_fixed() +
  theme(legend.position = "none",
        axis.title = element_text(size = 8, color = "black", face = "bold"),
        axis.text = element_text(size = 7.5, color = "black"),
        title = element_text(size = 8, color = "black", face = "bold"))
```

![](Backed-flakes-identification-in-Discoid-and-RC-Levallois_files/figure-markdown_github/Confusion%20matrix%20of%20SVMP-1.png)

### 3.2 PC Importance

``` r
# Data frame of PC importance
tibble(
  PC = rownames(varImp(SVM_Poly, sale = TRUE)$importance),
  Importance = varImp(SVM_Poly, sale = TRUE)$importance[, 1]) %>% 
  
  top_n(15, Importance) %>% 
# and plot  
  ggplot(aes(Importance, reorder(PC, Importance), fill = Importance)) +
  geom_bar(stat= "identity", position = "dodge") +
  geom_text(aes(label = round(Importance, 2)), 
            position = position_stack(vjust = 0.5), size = 2) +
  scale_fill_gradient(low = "red", high = "blue") +
  guides(fill = "none") +
  coord_flip() +
  ylab(NULL) +
  theme_light() +
  theme(
    axis.text.y = element_text(color = "black", size = 7),
    axis.text.x = element_text(color = "black", size = 7),
    axis.title.x = element_text(color = "black", size = 9),
    axis.title.y = element_text(color = "black", size = 9))
```

![](Backed-flakes-identification-in-Discoid-and-RC-Levallois_files/figure-markdown_github/PC%20Importance-1.png)

``` r
#PC biplots
ggpubr::ggarrange(
(PCA_Coord %>% ggplot(aes(PC3, PC1, fill = Strategy)) +
  geom_vline(xintercept = 0, alpha = 0.7, linetype = "dashed") +
  geom_hline(yintercept = 0, alpha = 0.7, linetype = "dashed") +
  stat_ellipse(geom = "polygon", alpha = 0.2, aes(fill = Strategy)) +
  geom_point(aes(color = Strategy)) +
  xlab(paste0("PC3 (", round((summary(pca)$importance[2,3])*100, 2), "%)")) +
  ylab(paste0("PC1 (", round((summary(pca)$importance[2,1])*100, 2), "%)")) +
  ggsci::scale_fill_aaas() +
  ggsci::scale_color_aaas() +
  theme_light() +
  theme(
    axis.text.y = element_text(color = "black", size = 7),
    axis.text.x = element_text(color = "black", size = 7),
    axis.title.x = element_text(color = "black", size = 9),
    axis.title.y = element_text(color = "black", size = 9),
    legend.position = "bottom")),


(PCA_Coord %>% ggplot(aes(PC3, PC8, fill = Strategy)) +
  geom_vline(xintercept = 0, alpha = 0.7, linetype = "dashed") +
  geom_hline(yintercept = 0, alpha = 0.7, linetype = "dashed") +
  stat_ellipse(geom = "polygon", alpha = 0.2, aes(fill = Strategy)) +
  geom_point(aes(color = Strategy)) +
  xlab(paste0("PC3 (", round((summary(pca)$importance[2,3])*100, 2), "%)")) +
  ylab(paste0("PC8 (", round((summary(pca)$importance[2,8])*100, 2), "%)")) +
  ggsci::scale_fill_aaas() +
  ggsci::scale_color_aaas() +
  theme_light() +
  theme(
    axis.text.y = element_text(color = "black", size = 7),
    axis.text.x = element_text(color = "black", size = 7),
    axis.title.x = element_text(color = "black", size = 9),
    axis.title.y = element_text(color = "black", size = 9),
    legend.position = "bottom")),
ncol = 2,
common.legend = TRUE,
legend = "bottom",
align = "h")
```

![](Backed-flakes-identification-in-Discoid-and-RC-Levallois_files/figure-markdown_github/First%20pairs%20of%20PC%20biplots-1.png)

``` r
ggpubr::ggarrange(
(PCA_Coord %>% ggplot(aes(PC1, PC8, fill = Strategy)) +
  geom_vline(xintercept = 0, alpha = 0.7, linetype = "dashed") +
  geom_hline(yintercept = 0, alpha = 0.7, linetype = "dashed") +
  stat_ellipse(geom = "polygon", alpha = 0.2, aes(fill = Strategy)) +
  geom_point(aes(color = Strategy)) +
  xlab(paste0("PC1 (", round((summary(pca)$importance[2,1])*100, 2), "%)")) +
  ylab(paste0("PC8 (", round((summary(pca)$importance[2,8])*100, 2), "%)")) +
  ggsci::scale_fill_aaas() +
  ggsci::scale_color_aaas() +
  theme_light() +
  theme(
    axis.text.y = element_text(color = "black", size = 7),
    axis.text.x = element_text(color = "black", size = 7),
    axis.title.x = element_text(color = "black", size = 9),
    axis.title.y = element_text(color = "black", size = 9),
    legend.position = "bottom")),

(PCA_Coord %>% ggplot(aes(PC1, PC6, fill = Strategy)) +
  geom_vline(xintercept = 0, alpha = 0.7, linetype = "dashed") +
  geom_hline(yintercept = 0, alpha = 0.7, linetype = "dashed") +
  stat_ellipse(geom = "polygon", alpha = 0.2, aes(fill = Strategy)) +
  geom_point(aes(color = Strategy)) +
  xlab(paste0("PC1 (", round((summary(pca)$importance[2,1])*100, 2), "%)")) +
  ylab(paste0("PC6 (", round((summary(pca)$importance[2,6])*100, 2), "%)")) +
  ggsci::scale_fill_aaas() +
  ggsci::scale_color_aaas() +
  theme_light() +
  theme(
    axis.text.y = element_text(color = "black", size = 7),
    axis.text.x = element_text(color = "black", size = 7),
    axis.title.x = element_text(color = "black", size = 9),
    axis.title.y = element_text(color = "black", size = 9),
    legend.position = "bottom")),
ncol = 2,
common.legend = TRUE,
legend = "bottom",
align = "h")
```

![](Backed-flakes-identification-in-Discoid-and-RC-Levallois_files/figure-markdown_github/Second%20pairs%20of%20PC%20biplots-1.png)

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-beyries_etude_1983" class="csl-entry">

Beyries, S., Boëda, E., 1983. Étude technoloogique et traces
d’utilisation des éclats débordants de corbehem (pas-de-calais).
Bulletin de la Société préhistorique française 80, 275–279.
https://doi.org/<https://doi.org/10.3406/bspf.1983.5455>

</div>

<div id="ref-dibble_levallois:_1995" class="csl-entry">

Boëda, E., 1995. Levallois: A volumetric construction, methods, a
technique, in: Dibble, H.L., Bar-Yosef, O. (Eds.), The Definition and
Interpretation of Levallois Technology, Monographs in World Archaeology.
Prehistory Press, Madison, Wisconsin, pp. 41–68.

</div>

<div id="ref-boeda_concept_1994" class="csl-entry">

Boëda, E., 1994. Le concept levallois: Variabilité des méthodes, CNRS
éditions. CNRS.

</div>

<div id="ref-boeda_debitage_1993" class="csl-entry">

Boëda, E., 1993. Le débitage discoïde et le débitage levallois récurrent
centripède. Bulletin de la Société Préhistorique Française 90, 392–404.
<https://doi.org/10.3406/bspf.1993.9669>

</div>

<div id="ref-boeda_identification_1990" class="csl-entry">

Boëda, E., Geneste, J.-M., Meignen, L., 1990. Identification de chaînes
opératoires lithiques du paléolithique ancien et moyen. Paléo 2, 43–80.

</div>

<div id="ref-bordes_typologie_1961" class="csl-entry">

Bordes, F., 1961. Typologie du paléolithique ancien et moyen,
Publications de l’institut de préhistoire de l’université de bordeaux.
CNRS Editions, Bordeaux.

</div>

<div id="ref-bordes_notules_1953" class="csl-entry">

Bordes, F., 1953. Notules de typologie paléolithique II : Pointes
Levalloisiennes et pointes pseudo-levalloisiennes. bspf 50, 311–313.
<https://doi.org/10.3406/bspf.1953.3057>

</div>

<div id="ref-brenet_variabilite_2013" class="csl-entry">

Brenet, M., 2013. Variabilité et signification des productions lithiques
au paléolithique moyen ancien. L’exemple de trois gisements de plein-air
du bergeracois (dordogne, france), BAR international series.
Archaeopress, Oxford.

</div>

<div id="ref-brenet_silex_2012" class="csl-entry">

Brenet, M., 2012. Silex et roches métamorphiques au paléolithique moyen
récent: Combemenue (corrèze) et chemin d’herbe (lot-et-garonne), in:
Marchand, G., Querré, G. (Eds.), Roches Et Sociétés de La Préhistoire
Entre Massifs Cristallins Et Bassins Sédimentaires : Le Nord-Ouest de La
France Dans Son Contexte Européen. Presses universitaires de Rennes, pp.
379–393.

</div>

<div id="ref-brenet_gisement_2008" class="csl-entry">

Brenet, M., Cretin, C., 2008. Le gisement paléolithique moyen et
supérieur de combemenue (brignac-la-plaine, corrèze). Du microvestige au
territoire, réflections sur les perspectives d’une approche
multiscalaire, in: Aubry, T., Almeida, F., Araújo, A.C., Tiffagom, M.
(Eds.), Proceedings of the XV World Congress UISPP (Lisbon, 4-9
September 2006). Space and Time: Which Diachronies, Which Synchronies,
Which Scales? / Typology Vs Technology, Sessions C64 and C65, BAR
International Series. Archaeopress, Oxford, pp. 35–44.

</div>

<div id="ref-bustillo_caracteristicas_2005" class="csl-entry">

Bustillo, M.A., Pérez-Jiménez, J.L., 2005. Características diferenciales
y génesis de los niveles silíceos explotados en el yacimiento
arqueológico de casa montero (vicálvaro, madrid). Geogaceta 38, 243–246.

</div>

<div id="ref-bustillo_caracterizacion_2012" class="csl-entry">

Bustillo, M.Á., Pérez-Jiménez, J.L., Bustillo, M., 2012. Caracterización
geoquímica de rocas sedimentarias formadas por silicificación como
fuentes de suministro de utensilios líticos (mioceno, cuenca de madrid).
Revista Mexicana de Ciencias Geológicas 29, 233–247.

</div>

<div id="ref-courbin_spatial_2020" class="csl-entry">

Courbin, P., Brenet, M., Michel, A., Gravina, B., 2020. Spatial analysis
of the late middle palaeolithic open-air site of bout-des-vergnes
(bergerac, dordogne) based on lithic technology and refitting. Journal
of Archaeological Science: Reports 32, 102373.
https://doi.org/<https://doi.org/10.1016/j.jasrep.2020.102373>

</div>

<div id="ref-dibble_variability_1995" class="csl-entry">

Delagnes, A., 1995. Variability within uniformity: Three levels of
variability within the levallois system, in: Dibble, H.L., Bar-Yosef, O.
(Eds.), The Definition and Interpretation of Levallois Technology,
Monographs in World Archaeology. Prehistory Press, Madison, Wisconsin,
pp. 201–211.

</div>

<div id="ref-hovers_diversity_2006" class="csl-entry">

Delagnes, A., Meignen, L., 2006. Diversity of lithic production systems
during the middle paleolithic in france. Are there any chronological
trends?, in: Hovers, E., Kuhn, S.L. (Eds.), Transitions Before the
Transition Evolution and Stability in the Middle Paleolithic and Middle
Stone Age. Springer, pp. 85–107.

</div>

<div id="ref-delpiano_techno-functional_2021" class="csl-entry">

Delpiano, D., Gennai, J., Peresani, M., 2021. Techno-functional
implication on the production of discoid and levallois backed
implements. Lithic Technology 46, 171–191.
<https://doi.org/10.1080/01977261.2021.1886487>

</div>

<div id="ref-duran_lindustrie_2005" class="csl-entry">

Duran, J.-P., 2005. L’industrie moustérienne des Ànecs (Rodès,
Pyrénées-orientales, France). PYRENAE 36, 11–39.

</div>

<div id="ref-duran_variabilite_2006" class="csl-entry">

Duran, J.-P., Soler, N., 2006. Variabilité des modalités de débitage et
des productions lithiques dans les industries moustériennes de la grotte
de l’arbreda, secteur alpha (serinyà, espagne). Bulletin de la Société
Préhistorique Française 103, 241–262.

</div>

<div id="ref-fernandes_silex_2012" class="csl-entry">

Fernandes, P., Morala, A., Schmidt, P., Séronie-Vivien, M.-R., Turq, A.,
2012. Le silex du bergeracois: État de la question. Quaternaire
Continental d’Aquitaine, excursion AFEQ, ASF 2012 2012, 22–33.

</div>

<div id="ref-folgado_economie_2010" class="csl-entry">

Folgado, M., Brenet, M., 2010. Economie de débitage et organisation de
l’espace technique sur le site du paléolithique moyen de plein-air de la
mouline (dordogne, france), in: Conard, N., Delagnes, A. (Eds.),
Settlement Dynamics of the Middle Paleolithic and Middle Stone Age.
Kerns Verlag - (Tübingen Publications in Prehistory), Tübingen, pp.
427–454.

</div>

<div id="ref-rigaud_les_1988" class="csl-entry">

Geneste, J.-M., 1988. Les industries de la grotte vaufrey: Technologie
du debitage, economie et circulation de la matiere premiere lithique,
in: Rigaud, J.-P. (Ed.), La Grotte Vaufrey à Cenac Et Saint-Julien
(Dordogne) : Paléoenvironnements, Chronologie Et Activités Humaines,
Mémoires de La Société Préhistorique Française (Revue). Société
préhistorique française, Paris, pp. 441–517.

</div>

<div id="ref-kuhn_roots_2013" class="csl-entry">

Kuhn, S.L., 2013. Roots of the middle paleolithic in eurasia. Current
Anthropology 54, S255–S268. <https://doi.org/10.1086/673529>

</div>

<div id="ref-dibble_recurrent_1995" class="csl-entry">

Lenoir, M., Turq, A., 1995. Recurrent centripetal debitage (levallois
and discoidal): Continuity or discontinuity?, in: Dibble, H.L.,
Bar-Yosef, O. (Eds.), The Definition and Interpretation of Levallois
Technology, Monographs in World Archaeology. Prehistory Press, Madison,
Wisconsin, pp. 249–256.

</div>

<div id="ref-peresani_industrie_2003" class="csl-entry">

Locht, J.-L., 2003. L’industrie lithique du gisement de beauvais (oise,
france): Objectifs et variabilité du débitage discoïde, in: Peresani, M.
(Ed.), Discoid Lithic Technology: Advances and Implications, BAR
International Series. Archaeopress, Oxford, pp. 193–209.

</div>

<div id="ref-martin-viveros_use-wear_2020" class="csl-entry">

Martín-Viveros, J.I., Ollé, A., Chacón, M.G., Romagnoli, F., Gómez de
Soler, B., Vaquero, M., Saladié, P., Vallverdú, J., Carbonell, E., 2020.
Use-wear analysis of a specific mobile toolkit from the middle
paleolithic site of abric romaní (barcelona, spain): A case study from
level m. Archaeol Anthropol Sci 12, 16.
<https://doi.org/10.1007/s12520-019-00951-z>

</div>

<div id="ref-meignen_persistance_1996" class="csl-entry">

Meignen, L., 1996. Persistance des traditions techniques dans l’abri des
canalettes (nant-aveyron). Quaternaria Nova 6, 449–64.

</div>

<div id="ref-meignen_les_1993" class="csl-entry">

Meignen, L., 1993. Les industries lithiques de l’abri des Canalettes:
cuche 2, in: Meignen, L. (Ed.), L’abri des Canalettes. Un habitat
moustérien sur les grands Causses (Nant-Aveyron), Monographie du CRA.
CNRS Ed., Paris, pp. 238–328.

</div>

<div id="ref-peresani_discoiou_2003" class="csl-entry">

Mourre, V., 2003. Discoïde ou pas discoïde? Réflexions sur la pertinence
des critères techniques définissant le débitage discoïde, in: Peresani,
M. (Ed.), Discoid Lithic Technology. Advances and Implications, BAR
International Series. Archaeopress, Oxford, pp. 1–17.

</div>

<div id="ref-pasty_etude_2004" class="csl-entry">

Pasty, J.-F., Liegard, S., Alix, P., 2004. Étude de l’industrie lithique
du site paléolithique moyen des Fendeux (Coulanges, Allier). bspf 101,
5–25. <https://doi.org/10.3406/bspf.2004.12945>

</div>

<div id="ref-roebroeks_veil_1992" class="csl-entry">

Roebroeks, W., Loecker, D.D., Hennekens, P., Leperen, M.V., 1992. "A
veil of stones”: On the interpretation of an early middle palaeolithic
low density scatter at maastricht-belvédère (the netherlands). Analecta
Praehistorica Leidensia 25 The end of our third decade: Papers written
on the occasion of the 30th anniversary of the Institutte of prehistory,
volume I 25, 1–16.

</div>

<div id="ref-romagnoli_testing_2016" class="csl-entry">

Romagnoli, F., Bargalló, A., Chacón, M.G., Gómez de Soler, B., Vaquero,
M., 2016. Testing a hypothesis about the importance of the quality of
raw material on technological changes at abric romaní (capellades,
spain): Some considerations using a high-resolution techno-economic
perspective. JLS 3, 635–659. <https://doi.org/10.2218/jls.v3i2.1443>

</div>

<div id="ref-peresani_les_2003" class="csl-entry">

Slimak, L., 2003. Les debitages discoïdes mousteriens: Evaluation d’un
concept technologique, in: Peresani, M. (Ed.), Discoid Lithic
Technology. Advances and Implications, BAR International Series.
Archaeopress, Oxford, pp. 33–65.

</div>

<div id="ref-slimak_variabilite_1998" class="csl-entry">

Slimak, L., 1998. La variabilité des débitages discoïdes au
paléolithique moyen: Diversité des méthodes et unité d’un concept.
L’exemple des gisements de la baume néron (soyons, ardèche) et du champ
grand (saint-maurice-sur-loire, loire). Préhistoire anthropologie
méditerranéennes 7, 75–88.

</div>

<div id="ref-turq_fragmented_2013" class="csl-entry">

Turq, A., Roebroeks, W., Bourguignon, L., Faivre, G.-P., 2013. The
fragmented character of middle palaeolithic stone tool technology.
Journal of Human Evolution 65, 641–655.
<https://doi.org/10.1016/j.jhevol.2013.07.014>

</div>

</div>
