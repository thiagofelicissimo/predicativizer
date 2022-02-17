- Needs Universo version [here](https://github.com/thiagofelicissimo/Dedukti/tree/universo-for-predicativizer)
- Run `bash translate.sh` to translate the files in `ctslib-fresh` to `final`. The result is in the Agda PTS and where some definitions are universe polymorphic.
- The files in `final-result-all-workin` are the results of a successful run, in which all files were translated
- If a definition is unecessarily too universe polymorphic, we can add constraints in some of its level variables. See the function `extra_cstr` in the file `Dedukti/universo/common/unif.ml`


