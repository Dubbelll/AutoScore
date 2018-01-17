#!/bin/sh
#Variables
CI_PIPELINE_ID=$1
#Script
echo "Changing directory to builds..."
cd /home/private/builds
echo "Removing builds if there are more than 5, oldest first..."
ls -A1t | tail -n +6 | xargs -r rm
echo "Deleting current active build .zip..."
rm /home/public/build-*.zip
echo "Copying .zip..."
cp /home/private/builds/build-$CI_PIPELINE_ID.zip /home/public/
echo "Unzipping..."
unzip -q -o /home/public/build-$CI_PIPELINE_ID.zip -d /home/public
echo "Done installing!"