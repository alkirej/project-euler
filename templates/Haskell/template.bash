#!/bin/bash

clear
hlint .
hlint ~/git/project-euler/0000-library/Haskell

cd ~/git/project-euler/<PROBLEM DIR>
cabal run euler-problem-n<PROBLEM NUMBER> $@

