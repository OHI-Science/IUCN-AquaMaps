### Methods: Comparison of paired maps
 
Although relatively small in number, overlap species present a unique opportunity
to evaluate the two datasets overall.  For the species included in both datasets,
we examine how well the maps align in both spatial distribution and overall area.
Based upon the definitions of extent of occurrence and area of occupancy, we expect
that for a given species, the AquaMaps predicted distribution will fall within, and
describe a smaller range than, the IUCN predicted distribution.  Where these
expectations seem to fail, we explore methodological issues that can introduce errors.

We identified "paired map" species using genus and species binomials as a matching key.
We used the [taxize package](https://ropensci.org/tutorials/taxize_tutorial.html)  
in [R statistical software](https://www.r-project.org/)
to standardize species names and synonyms; for species with separate subpopulation maps
in IUCN, we combined all subpopulations to create a single global population.   For each
of these paired map species, we determined species presence within each spatial cell for
each dataset using the same criteria outlined above.

Overlaying paired distribution maps for each species, we defined and calculated
_distribution alignment_ $\alpha_{dist}$ and _area ratio_ $\alpha_{area}$:

$$\alpha_{dist} = \frac{A_{small \cap large}}{A_{large}} * 100\%$$

$$\alpha_{area} = \frac{A_{small}}{A_{large}} * 100\%$$

For each paired map species, $A_{small}$ and $A_{large}$ indicate the smaller and larger
range representation (regardless of which dataset) in km^2^.  $A_{small \cap large}$ 
represents the amount of overlapping area between the two datasets.  Distribution alignment
uses overlapping predictions of presence as a measure of concurrence between the two 
datasets.  Area ratio compares range size, used by IUCN as a criterion to help define
extinction risk; it also provides an indicator for frequency of errors of commission
(false indication of presence) or omission (false indication of absence).