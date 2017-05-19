<span class="s1">[**Installation**](installation.html)</span>

**From the R command prompt:**

**For the** [<span
class="s1">**CRAN**</span>](http://cran.r-project.org/) **production
version:**

-   Type<span class="s2"> install.packages("MSToolkit") </span>from the
    R prompt. Choose a [<span
    class="s1">CRAN</span>](http://cran.r-project.org/) mirror site from
    which to download the package.

\

**For the** [<span
class="s1">**R-Forge**</span>](http://r-forge.r-project.org/projects/mstoolkit/)
**development (daily build) version:**

<span class="s3">Type
</span>install.packages("packagename",repos="[<span
class="s1">http://R-Forge.R-project.org</span>](http://R-Forge.R-project.org)")<span
class="s3"> from the R prompt.<span
class="Apple-converted-space"> </span></span>

\

**Manually**:

Download MSToolkit from [<span
class="s1">R-Forge</span>](http://r-forge.r-project.org/projects/mstoolkit/).
Save the zip file to the desktop.

-   Launch R on your local system

<!-- -->

-   On the menu bar select “Packages”
-   Select “Install package(s) from local zip files”
-   Navigate to the folder where you saved the R package zip file above
-   Select the zip file and install (click open)

The MSToolkit package is now installed on your local instance of R. Full
instructions for use, and HELP files are included in the package. These
are accessible using R’s “Help” menu.

**R settings**

If the package fails to load every time you start R you may need to
change your RProfile and set it to load up the MSToolkit package every
time you invoke R. To do this add the following line to your RProfile
(or RProfile.Site) file in the \\etc directory of your R program (e.g.
In Windows this may be **C:\\program files\\R\\R-2.9.1\\etc\\**. In Mac
OSX this will be
**\\library\\Frameworks\\R.framework\\Versions\\2.9\\**). This file MAY
NOT EXIST in this directory and you may need to create a file with the
following lines of code:

.First \<- function(){

<span class="Apple-converted-space">  </span>require ("MSToolkit")}
<span class="Apple-converted-space"> </span>

\

If you have a **.First** function already set up, then simply add the
line<span class="s2"> require("MSToolkit")</span>.

**SAS and LSF GRID settings**

Before using SAS as an analytical engine or using an LSF GRID
environment, it is important to set up preferences for the location of R
and the SAS executable (if required). A file ECTD.ini is provided in the
top directory of the library which should contain the following
rows:<span class="Apple-converted-space"> </span>

\# Instructions to set up environmental variables

\# This option allows the setting of the R execution binary on a Unix
grid

\# Sys.setenv("RLSF\_UNIX"=".")

\# This option allows the setting of the R execution binary on a Linux
grid

\# Sys.setenv("RLSF\_LINUX"=".") <span
class="Apple-converted-space"> </span>

\# This option allows the setting of the SAS execution path on a Unix
machine

\# Sys.setenv("SASPATH\_UNIX"=".")

\# This option allows the setting of the SAS execution path on a Windows
machine

\# Sys.setenv("SASPATH\_WIN"="C:\\\\Program Files\\\\SAS
Institute\\\\SAS\\\\V8\\\\sas.exe")

\

