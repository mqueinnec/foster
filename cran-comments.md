## Resubmission (0.1.1)
In this version I have:

* Fixed dependency to the spatstat package (>= 2.0.0) which has been divided into a family of packages. The spatstat (>=2.0.0) and spatstat.geom packages are listed in the DESCRIPTION. The namespace spatstat::crossdit has been updated to spatstat.geom::crossdit in the getSample function
  
## Test environments
* local Windows 10 x64 (build 17134), R 4.0.2
* Ubuntu 16.04.6 LTS (on travis-ci), R 4.0.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

