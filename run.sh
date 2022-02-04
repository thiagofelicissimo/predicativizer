name=$1
file=$(printf "%s.dk" $name)
constraints=$(printf "%s_cstr.dk" $name)

rm program
ocamlc -o program str.cma unif.ml

dune exec universo -- --elab-only -o temp_univ \
     --theory theory/cts.dk --config config/universo_cfg.dk \
     -I theory/ -I final/ -l \
     $(printf "ctslib/%s " $file)

dune exec universo -- --check-only -o temp_univ \
     --theory theory/cts.dk --config config/universo_cfg.dk \
     -I theory/ -I final/ -l \
     $(printf "ctslib/%s " $file)

cd temp_univ
to_dependencies=$(../program $name)
cp out.dk ../final/$file
cd ../final
dkcheck --errors-in-snf -e -I ../theory $file
cd ../ctslib
sed -i "s/$name.$name/$to_dependencies/g" *.dk
