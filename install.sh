#!/bin/sh
#Variables
CI_PIPELINE_ID=$1
#Script
echo "Changing directory to builds..."
cd /home/musilitar/builds/carcrashmystery
echo "Removing builds if there are more than 5, oldest first..."
ls -A1t | tail -n +6 | xargs -r rm
echo "Deleting current active build .zip..."
rm /home/musilitar/published/carcrashmystery/build-*.zip
echo "Copying .zip..."
cp /home/musilitar/builds/carcrashmystery/build-$CI_PIPELINE_ID.zip /home/musilitar/published/carcrashmystery/
echo "Unzipping..."
sudo unzip -q -o /home/musilitar/published/carcrashmystery/build-$CI_PIPELINE_ID.zip -d /var/www/carcrashmystery
echo "Done installing!"
