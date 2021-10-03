#!/usr/bin/env bash

dd if=result/iso/$(ls result/iso/) of=$1 status='progress'
