[
  "#{ENV['HOME']}/bin",
  "#{ENV['HOME']}/.config/bookmarks",
  "#{ENV['HOME']}/.config/systemd/user",
].each do |dir|
  directory dir do
    owner node[:user]
  end
end

xdg_config 'gtk-3.0/gtk.css'

include_recipe "../../cookbooks/nix"

include_recipe '../../cookbooks/docker'
include_recipe '../../cookbooks/xremap'

package "fcitx5-mozc"
package 'fonts-noto-color-emoji'
