#!/bin/bash

npm install

export PATH=$(pwd)/node_modules/.bin:$PATH

exec bash
