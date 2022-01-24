#!/bin/bash

set -e

bin/setup

sudo -E bin/mitamae local $@ lib/recipe.rb
