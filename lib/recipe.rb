include_recipe 'recipe_helper'

node.reverse_merge!(user: ENV['USER'])

directory "#{ENV['HOME']}/bin" do
  owner node[:user]
end

directory "#{ENV['HOME']}/.config/systemd/user" do
  owner node[:user]
end

dotfile '.docker'
dotfile '.peco'
dotfile '.vimrc'

xdg_config 'gem/gemrc'
xdg_config 'git/config'
xdg_config 'git/commit_template'

include_recipe '../cookbooks/zsh'
