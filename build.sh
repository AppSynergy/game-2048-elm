#!/bin/bash

elm make src/Elm2048.elm --output build/Elm2048.js
echo -n "Copying html files ... "
cp assets/index.html build/
echo "Done"
