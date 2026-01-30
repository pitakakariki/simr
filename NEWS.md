### simr 1.0.9

 - capture boundary (singular) fit messages

### simr 1.0.8

 - maintenance update to comply with CRAN rules

### simr 1.0.7

 - compatibility update for R 4.3.0 (Martyn Plummer)
 
 - bugfix for `compare` with complicated models (Tobias R. Rebholz)

### simr 1.0.6

 - reduce dependence on orphaned plyr.
 
 - fixed given/family name order in DESCRIPTION

### simr 1.0.5

 - fixed a unit test that was causing problems for lme4 downstream checks.

 - minor improvements
     - include `nrow` in `summary.powerCurve`.

### simr 1.0.4

 - compatibility updates for `lmerTest` version 3.0-0.

 - bugfixes
     - contrast attributes are no longer dropped by `extend`.
     - more bugfixes and unit tests for binomial responses.
     - warnings for non-uniform weights, which aren't supported yet.

### simr 1.0.3

 - update maintainer email address.

### simr 1.0.2

 - `print`, `summary`, and `confint` methods for easier access to results.
 - `log(y)` type responses now work.
 - added unit tests.

### simr 1.0.1

 - added citation info.
 - added vignette building instructions.
 - bugfixes
     - binomial responses.
     - lm/glm simulation.
     - subsetted data arguments.

### simr 1.0.0

 - Initial CRAN release.
