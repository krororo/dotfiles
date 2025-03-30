[
  "#{ENV['HOME']}/bin",
  "#{ENV['HOME']}/.config/bookmarks",
  "#{ENV['HOME']}/.config/systemd/user",
].each do |dir|
  directory dir do
    user node[:user]
  end
end

include_recipe "../../cookbooks/nix"

include_recipe '../../cookbooks/docker'
include_recipe "../../cookbooks/firefox"
include_recipe '../../cookbooks/xremap'

package "fcitx5-mozc"
package 'fonts-noto-color-emoji'

dotfile ".Xresources"
