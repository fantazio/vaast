# Vaast

Vaast stands for Version-Agnostic AST.

## Why

OCaml's Typedtree (and Parsetree) breaks in between minor OCaml version updates.
Developers of tools relying on this representation need to either accept to
drop support of older versions when updating to the latest, or adapt their
codebase with version selection mechanisms (e.g. via cppo or version-control
branches).
The first solution is not satisfying because users may work with different
OCaml versions, and not every project moves on to the latest version at
the same pace (or even at all).
Regarding the second solution, both relying on cppo and version-control
branches hinder overall maintenace and are very error-prone, with increasing
amount of versions and breakages. In addition, with every tools reimplementing
the same solution, the maintenance cost is multiplied.

Vaast's goal is to provide a uniform representation of OCaml's AST across
all OCaml versions. In addition, the representation should be sufficiently
robust to reduce version-related breakages to relevant ones (e.g. replacing a
constructor), while silencing usually-noisy ones (e.g. an additional field in
a constructor).
This way, supporting the latest OCaml version in Typedtree-dependent tools
should require less effort (when any), and their code would be identical
across versions, improving code readability and accessibility, and reducing
maintenance cost.
