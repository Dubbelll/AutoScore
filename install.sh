#!/bin/sh
#Variables
CI_PIPELINE_ID=$1
PATH_BUILDS="/home/musilitar/builds/carcrashmystery"
PATH_PUBLISHED="/home/musilitar/published/carcrashmystery"
PATH_WEBROOT="/var/www/carcrashmystery"
#Script
echo "Changing directory to builds..."
cd $PATH_BUILDS
echo "Removing builds if there are more than 5, oldest first..."
ls -A1t | tail -n +6 | xargs -r rm
echo "Deleting current active build .zip..."
rm $PATH_PUBLISHED/build-*.zip
echo "Copying .zip..."
cp $PATH_BUILDS/build-$CI_PIPELINE_ID.zip $PATH_PUBLISHED/
echo "Unzipping..."
sudo unzip -q -o $PATH_BUILDS/build-$CI_PIPELINE_ID.zip -d $PATH_WEBROOT
echo "Done installing!"
