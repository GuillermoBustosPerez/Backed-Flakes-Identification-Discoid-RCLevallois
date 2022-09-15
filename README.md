## Note for Reviewers

The present repository contains all data and code of employed for the
development of the present article.  
Please note that this is a private repository intended for reviewers
only. If the article is accepted for publication the present repository
will be made public and published in Zenodo (<https://zenodo.org/>)
under a DOI.

Other examples of published data:

-   Bustos-Pérez, G., Baena, J., 2021. Predicting Flake Mass: A View
    from Machine Learning. Lithic Technology 46, 130–142.
    <https://doi.org/10.1080/01977261.2021.1881267>  
-   Link to Zenodo repository:
    <https://zenodo.org/record/4603149#.YmZzKdpBwdU>

## Article information

**Journal**:

-   Journal of Archaeological Science: Reports

**Authors**:

-   Guillermo Bustos-Pérez<sup>1, 2, 3</sup>  
-   Brad Gravina<sup>4, 5</sup>  
-   Michel Brenet<sup>5, 6</sup>  
-   Francesca Romagnoli<sup>1</sup>

<sup>1</sup>Universidad Autónoma de Madrid. Departamento de Prehistoria
y Arqueología, Campus de Cantoblanco, 28049 Madrid, Spain  
<sup>2</sup>Institut Català de Paleoecologia Humana i Evolució Social
(IPHES), Zona Educacional 4, Campus Sescelades URV (Edifici W3), 43007
Tarragona, Spain  
<sup>3</sup>Àrea de Prehistoria, Universitat Rovira i Virgili (URV),
Avinguda de Catalunya 35, 43002 Tarragona, Spain  
<sup>4</sup>Musée national de Préhistoire, MCC, 1 rue du Musée, 24260
Les Eyzies de Tayac, France  
<sup>5</sup>UMR-5199 PACEA, Université de Bordeaux, Bâtiment B8, Allée
Geoffroy Saint Hilaire, CS 50023, 33615 PESSAC CEDEX, France  
<sup>6</sup>INRAP Grand Sud-Ouest, Centre mixte de recherches
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
these backed implements are commonly produced during Levallois and
discoidal reduction sequences. Backed products from Levallois and
discoidal reduction sequences often show common geometric and
morphological features that complicate their attribution to one of these
methods. This study examines the identification of experimentally
produced discoidal and recurrent centripetal Levallois backed products
(including all stages of reduction) based on their morphological
features. 3D geometric morphometrics are employed to quantify
morphological variability among the experimental sample. Dimensionality
reduction though principal component analysis is combined with 11
machine learning models for the identification of knapping methods. A
supported vector machine with polynomial kernel has been identified as
the best model (with a general accuracy of 0.76 and an area under the
curve \[AUC\] of 0.8). This indicates that combining geometric
morphometrics, principal component analysis, and machine learning models
succeeds in capturing the morphological differences of backed products
according to the knapping method.

**Key words**: lithic analysis; Levallois; Discoid; Geometric
Morphometrics; Machine Learning; Deep Learning

## Structure of the repository

-   [Data](Data) folder:

    -   **GM csvs**: folder containing csvs. Each csv represents 3D
        coordinates of each of the artifacts.  
    -   **Attributes.csv**: attributes manually recorded of each of the
        artifacts  
    -   **Flakes LM rotated.RData**: coordinates of landmarks after
        rotation  
    -   **Flakes_LM.RData**: coordinates of landmarks prior to rotation.
        This file is generated after loading all csvs from the folder
        “GM csvs”.  
    -   **Worksapce all models.RData**: contains all Machine and Deep
        Learning models

-   **“Meshes” folder**: contains all .ply meshes employed in the
    study  

-   **“Templates” folder**:

    -   **Code for inspecting Meshes and Landmarks.R**  
    -   **Flake Generic.vbrx**: a ViewBox template for positioning
        landmarks in flakes.  
    -   **Flake template.ply**: generic flake employed to generate the
        ViewBox template.

The final draft of the article can be accessed in the following formats:

-   **Backed flakes identification in Discoid and RC Levallois.Rmd**  

-   **Backed flakes identification in Discoid and RC Levallois.html**  

-   **Backed flakes identification in Discoid and RC Levallois.md**

-   **Backed flakes identification in Discoid and RC Levallois.pdf**

-   **License**  

-   **README.md**  

-   **References.bib**  

-   **journal-of-archaeological-science.csl**
