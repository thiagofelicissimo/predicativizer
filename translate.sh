rm -rf ctslib
cp -r ctslib-fresh ctslib
cd ctslib
files=$(dkdep -si *.dk)
cd ..
for i in $files
do
    bash run.sh $(basename $i .dk)
done

   
     
