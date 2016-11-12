#!/usr/bin/env bash

echo "checking if I'm standing in project's root"
cat navigation-api/version.sbt
if [ $? -eq 0 ]; then
    echo $'\nI am, checking if repository has changes'
    if git diff-index --quiet HEAD --; then
        echo "executing build... "
        sbt clean test navigation-api/assembly
        echo "done. starting deploy!"
        scp -i ~/.ssh/easy-travel-amazon.pem navigation-api/target/scala-2.11/footpath-routing-api.jar ubuntu@ec2-54-187-65-126.us-west-2.compute.amazonaws.com:~/app/
        ssh -i ~/.ssh/easy-travel-amazon.pem ubuntu@ec2-54-187-65-126.us-west-2.compute.amazonaws.com "~/app/scripts/shutdown && ~/app/scripts/startup"
        echo "all done. merry christmas! Maiameeeeeeeeeeeeeeeeee!"
    else
        echo "project has uncommited changes! commit them to continue, I'm guessing you don't want to deploy unversioned changes... or do you? D:"
    fi
else
    echo $'\nplease, cd to project\'s root directory to execute this script. thank you very much for your patience.'
fi
