#!/bin/sh
. ./cuda_ver.sh
cd ../app

widget=gtk2
cpu=amd64

outdir=~/Public
mkdir -p $outdir
name=$outdir/cudatext-linux-$widget-$cpu-$cuda_ver.tar

rm $outdir/*-$widget-*.xz
tar --exclude=*.pyc -cf $name cudatext readme data settings_default py/*.py py/cuda_addonman py/cuda_make_plugin py/cuda_insert_time py/cudax_lib py/requests py/cuda_comments

cd ../setup/debfiles
tar -rf $name cudatext-256.png

xz -z $name
