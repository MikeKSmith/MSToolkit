---
layout: page
title: MSToolkit
description: An R package for simulating trial designs and 
evaluating operating characteristics.
---

![MSToolkit logo](assets/index.600px-Mstoolkit_logo_04.jpg)

The MSToolkit R package has been written to help users simulate clinical
trials, evaluate designs, analysis methodology and quantify operating
characteristics through the application of dose and study level decision
criteria.

-   Can simulate parallel group, crossover and longitudinal designs
-   Flexibility in defining data generation processes using parametric
    models
-   Data generation model parameters can vary across simulation
    replicates; parameters can vary between subjects
-   The data generation function specifies the linear predictor for the
    mean response given inputs (dose, time, covariates); inverse link
    functions allow generation of continuous, binary and count data.
    User-written inverse link functions allow data generation from a
    variety of other distributions.
-   Model parameters and covariates for each subject / replicate can be
    generated from multivariate normal distributions or sampled from
    external data files e.g. existing data files.

This wiki guides users through the use of the package. Help files for
individual functions are provided with the package.

MSToolkit development page is hosted on GitHub at 
(https://github.com/MikeKSmith/MSToolkit).

- [Status](pages/status.html)
- [Installation](pages/installation.html)
- [Simulation overview](pages/simulation_overview.html)
- [Usage](pages/usage.html)
- [generateData](pages/generatedata.html)
- [Data generation examples](pages/data_generation_examples.html)
- [analyzeData](pages/analyzeData.html)
- [Demo code: Simple Emax fit example](pages/simple_emax_fit_example.html)
- [Support](pages/support.html)
- [References](pages/references.html)

MSToolkit was developed jointly by Mango Solutions and Pfizer Inc.
Funding for the project was provided by Pfizer. MSToolkit is covered by
the GNU Public License (GPL). (c) 2010.

This program is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along
with this program. If not, see http://www.gnu.org/licenses.

**Disclaimer of Warranty:**

THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM “AS IS” WITHOUT
WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF
THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME
THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.

Limitation of Liability:

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR
CONVEYS THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,
INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES
ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT
NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES
SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE
WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN
ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
