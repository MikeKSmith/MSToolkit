# MSToolkit 3.3.1

`MSToolkit` 3.3.1 is in development on [GitHub](https://github.com/MikeKSmith/MSToolkit). The 3.3.1 version refactors some of the tests and directory structure.

# MSToolkit 3.0.0

`MSToolkit` 3.0.0 is in development on [GitHub](https://github.com/MikeKSmith/MSToolkit). The 3.0.0 version removes the NONMEM $PRED parsing and related code. The rationale behind this is to try to revert `MSToolkit` to a more dedicated trial design simulation, analysis and operating characteristic evaluation tool. Other simulation tools are available to simulate from NONMEM models - for example: [Perl speaks NONMEM or PsN](https://github.com/UUPharmacometrics/PsN).

Other R-based simulation engines exist for simulating from systems of differential equations. See for example [PKPDsim](https://github.com/ronkeizer/PKPDsim), [RxODE](https://github.com/hallowkm/RxODE) or [mrgsolve](https://github.com/metrumresearchgroup/mrgsolve).

DDMoRe products allow the user to simulate from MDL / PharmML models using tools such as PsN, simulx in [mlxR](https://github.com/MarcLavielle/mlxR).

With `MSToolkit` 3.0.0 I would like to focus on trial design, leaving generation of response outcomes to other tools, such as those listed above, unless the response generating equations are easy to define analytically. Ultimately it is my intention to be able to link `MSToolkit` with other tools to generate a pipeline of trial design, analysis and evaluation of operating characteristics.

# MSToolkit 2.0.0

`MSToolkit` 2.0.0 was released to the public via [R-Forge](https://r-forge.r-project.org/projects/mstoolkit/) on 29th June 2009.

It was available via CRAN until March 2013 when it was orphaned. At that time `MSToolkit` required some additional changes around handling of parallel processing (moving from Rlsf to snow to parallel) and I didn’t have the time to address this and other maintenance issues. I hope that with `MSToolkit` being on GitHub the community may assist in fixing minor issues by forking, changing code and making a pull request.

# MSToolkit 1.0.0

`MSToolkit` 1.0.0 was designed by [Mike K Smith](https://www.linkedin.com/in/mikeksmith/) of Pfizer and was developed by [Mango Solutions](https://www.mango-solutions.com/) for Pfizer. `MSToolkit` 1.0.0 was an internal release to Pfizer.
