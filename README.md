# KeilSim
An R package with functions supporting Statistical Computing Final Project Analysis

## Installation Instructions
### Installing KeilSim

Install directly from this GitHub repo as follows:
```
# Install devtools, needed for install_github() function
install.packages("devtools")
library(devtools)

# use install_github() to install the package and load using library()
install_github('StableMarkets/KeilSim', force=TRUE)
library(KeilSim)

?keilsim # pull up the help file
```

### Installing C++ Compiler
```keilsim()``` back-ends to Stan, which in turn back-ends to C++. So you'll need to install a C++ 
compiler to use this function. 

For Windows, follow the instructions in the "toolchain" sections:  
https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Windows#toolchain

For Mac/Linux, follow the instructions in the "toolchain" sections:  
https://github.com/stan-dev/rstan/wiki/Installing-RStan-on-Mac-or-Linux#toolchain

## Vingette

A vingette is available 
