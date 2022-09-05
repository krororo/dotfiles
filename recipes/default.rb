include_recipe '../lib/recipe_helper'

node.reverse_merge!(user: ENV['SUDO_USER'] || ENV['USER'])

include_recipe 'base'
include_recipe node[:platform]
