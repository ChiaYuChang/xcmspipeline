#!/bin/bash

tar zcvf pipeline.tar.gz pipeline
docker build --tag toolsxcmspipeline:1.22 .

