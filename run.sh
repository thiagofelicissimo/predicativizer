name=$1
file=$(printf "%s.dk" $name)
constraints=$(printf "%s_cstr.dk" $name)

dune exec universo -- --elab-only -o temp_univ \
     --theory theory/cts.dk --config config/universo_cfg.dk \
     -I theory/ -I final/ -l \
     $(printf "ctslib/%s " $file)

dune exec universo -- --check-only -o temp_univ \
     --theory theory/cts.dk --config config/universo_cfg.dk \
     -I theory/ -I final/ -l \
     $(printf "ctslib/%s " $file)

cp temp_univ/$file final/$file
cd final
echo "Checking"
dkcheck --errors-in-snf -e -I ../theory $file
cd ..
to_dependencies=$(cat todep)
cd ctslib

# some occurences of $name.$name occur in strings such as
# blabla_$name.$name_bla
# look for instance at ltn_to_ltO
# therefore, we add the @ to protect them from the sed
sed -i "s/_$name\.$name/@_$name_@_$name/g" *.dk
sed -i "s/$name\.$name/$to_dependencies/g" *.dk
sed -i "s/@_$name_@_$name/_$name\.$name/g" *.dk
