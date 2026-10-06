# Publication readiness, 6 October 2026 (sources checked 5 October)

We recommend maintaining and testing the corrected R/Shiny authoring path before
expanding metaUI. The distinctive contribution remains editable apps that authors
create and own, with R modelling visible in the generated source. The follow-up timings establish improvements in p-curve calculation and repeated
selection response (see performance-optimisation.md). New-selection fits remain
bounded on the measured fixtures; there is no measured case for a runtime rewrite.

JOSS is a conditional later candidate. Its [current submission requirements](https://joss.readthedocs.io/en/latest/submitting.html)
require more than six months of public history with active development across that
period, demonstrated research use (at minimum by developers), a meaningful research
contribution, and maintainable, feature-complete software. The [review criteria](https://joss.readthedocs.io/en/latest/review_criteria.html)
also assess evidence of impact and require AI-use disclosure. Neither document
requires an invented three-month revival period or a particular GitHub star count.
The repository's age alone does not establish the required iterative development.

There is documented **developer research use**: Wallrich et al. (2024),
[The Relationship Between Team Diversity and Team Performance](https://doi.org/10.1007/s10869-024-09977-0),
Journal of Business and Psychology, states that the authors used metaUI to create
an exploratory webapp. The [institutional accepted manuscript](https://eprints.bbk.ac.uk/id/eprint/53930/1/diversity_meta_preprint_updated.pdf)
contains that statement. This supports concrete own use; it does not establish
independent adoption, nor that the default metaUI models reproduce every estimate
in that paper. GitHub stars and forks are not usage counts. We found no verified
independent research use in this bounded search; that is not evidence of absence.

Relevant alternatives include [PsychOpen CAMA](https://leibniz-psychology.org/en/practices-and-tools-of-open-science/psychopen-cama),
a maintained platform for depositing and exploring cumulative meta-analyses, and
[Allbritton et al. (2024)](https://pmc.ncbi.nlm.nih.gov/articles/PMC11276543/),
Breathing Life Into Meta-Analytic Methods, a general tool for living meta-analysis.
These already support online exploration and updates. A publication would need to
explain when ownership of editable generated R apps is useful, rather than claiming
that interactive meta-analysis itself is new.

The next practical gate is an independently created useful app, with its scientific
mapping, estimator assumptions, exclusions, and edit-preservation behaviour checked
against the user's analysis, plus documented real research use. No recruitment or
contact occurred in this thread. The bundled-data audit now verifies the retained Aksayli, Coles and Dannheim
licences; the unresolved Barroso CSV has been removed with an explicit migration
notice. Release/oldrel-1 CI and a fresh hosted Linux empty-library installation
have passed. A broader cross-platform release review and independent author reuse
remain useful next gates; scale-specific panels and limitations are documented. No publication draft, submission, DOI, public upload, or
release is included. Agent assistance can be considered next over the tested config
path, with authors choosing scientific mappings and models. If independent reuse
reveals little demand, maintenance is a proportionate outcome.
