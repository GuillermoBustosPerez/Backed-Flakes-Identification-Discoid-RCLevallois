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

## Introduction

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
2006](#ref-hovers_diversity_2006)).

## References

<div id="refs" class="references csl-bib-body hanging-indent">

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

<div id="ref-kuhn_roots_2013" class="csl-entry">

Kuhn, S.L., 2013. Roots of the Middle Paleolithic in Eurasia. Current
Anthropology 54, S255–S268. <https://doi.org/10.1086/673529>

</div>

</div>
