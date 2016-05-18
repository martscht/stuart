# README #

This repository contains the current alpha-build of the STUART package for R. It is intended for the creation of short-forms of questionnaires via an Ant-Colony-Optimization approach. For more details on the package please see the help files contained in the package itself.

## Installation ##

The easiest way to install the current version of STUART is via the use of the `install_bitbucket()`-function included in the devtools package. To install the *stable* version use:

        library(devtools)
        install_bitbucket('martscht/stuart/stuart',auth_user='...',password='...')
        library(stuart)

This will install the version currently committed to the master branch. If you want to be a bit more edgy, you can install the development build by setting `ref='develop'`, like so:

        library(devtools)
        install_bitbucket('martscht/stuart/stuart',ref='develop',auth_user='...',password='...')
        library(stuart)

After installation the easiest way to get an overview of STUARTs functions and capabilities is to use `?stuart` to open the package help-file.
