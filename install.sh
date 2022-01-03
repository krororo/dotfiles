#!/bin/bash

set -e

bin/setup

bin/mitamae local $@ lib/recipe.rb
