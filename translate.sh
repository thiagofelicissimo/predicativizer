mkdir final
rm -rf ctslib
cp -r ctslib-fresh ctslib
cd theory
dkcheck -e cts.dk
cd ..
cd ctslib
files=$(dkdep -si *.dk)
cd ..
for i in $files
do
    bash run.sh $(basename $i .dk)
done

   
     
