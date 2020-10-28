## Resubmission
This is a resubmission. In this version I have:

* Changed all T and F to TRUE and FALSE 
* Removed unexecutable code in man/trainNN.Rd 
* Unwrapped examples of functions from `\dontrun{}`.Small toys 
examples were created and are all executable in < 5 sec, except for the calcIndices function example (see following bullet point)
* The example of the calcIndices function is executable in < 5 sec locally but > 5 sec during R CMD CHECK (both locally and on win-builder). The size of the dataset used in this example was reduced as much as possible. Therefore, I used `\donttest{}` for that example.
  
## Test environments
* local Windows 10 x64 (build 17134), R 4.0.2
* Ubuntu 16.04.6 LTS (on travis-ci), R 4.0.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
