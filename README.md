## Spatially-explicit estimation of building stock dynamics and thermal consumption in California

##### Hanna M. Breunig, Tyler Huntington, Ling Jin, Alastair Robinson, Corinne D. Scown 

<br>

#### Overview
This study takes a spatially-explicit approach to quantifying current and future
energy use of all non-residential buildings within the state of California.

<br>

#### Guide to The Repository

The folder in which this document is the root directory of the file system 
containing all of the R source code and several supporting data files used in 
our analyses. The file system is structured to behave as a 
self-containe ecosystem. That is all code scripts within the subdirectories 
should run and retrieve the data they need from other subdirectories using 
relative filepaths as long as the directory structure within `root` is 
maintained.

Here we provide an brief orientation to the repository and detail the contents
of the subdirectories:

<br>
##### input_shapefiles:
This subdirectory is currently empty except for a readme.txt file that 
reiterates what is noted here about it's purpose for existence. Shapefiles 
placed in this directory are loaded as input data to the scripts in the 
`root/R_code` directory. As the Shapefiles we used in our analysis are 
proprietary, we are unable to share them with you here; however, if you obtain
your own copies, you may easily process them using our code by placing them 
in this directory. Note that in addition to the `.shp` file, you must also 
inlcude the `.dbf`, `.shx`/`.sbx`, and `.prj` files associated with each
Shapefile.

<br>
##### R_code:

This subdirectory contains all the R source code for simulating building stock
turnover, calculating summary statistics and generating shapefiles. 

**`main.R`** - A script containing the main control flow of our analysis. 
Sourceshelper functions from `functions.R` and input data from `root/supp_data/` 
and `root/input_shapefiles`. Outputs building stock shapefiles to 
`root/output_shapefiles` and associated summary tables to `root/output_data`.

`functions.R` - Contains all the supporting functions required by other scripts
in the directory.

`btl_io.R` - An I/0 script that calculates median county and state level 
building area to lot area (BtoL) ratios for all California countiesf for which
shapefiles exist in the `root/input_shapefiles/` directory

<br>
#### documentation

This subdirectory contains supporting written documents for analysis. The `methods.txt` file which details the analytical methodology of the project and the `Tyler_log.gdoc` is a running log where I am keeping notes of my daily progress.
