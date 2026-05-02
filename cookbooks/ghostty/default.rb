xdg_config "ghostty/config.ghostty"

if node[:platform] == "darwin"
  cask "ghostty"
  os_config = "darwin-config"
else
  execute "Install ghostty" do
    url = "https://raw.githubusercontent.com/mkasberg/ghostty-ubuntu/HEAD/install.sh"
    command %(/bin/bash -c "$(curl -fsSL #{url})")
    not_if "test -f /usr/bin/ghostty"
  end
  os_config = "linux-config"
end

link File.join(ENV["HOME"], ".config/ghostty/os-config") do
  to File.expand_path(File.join("../../../config/ghostty/#{os_config}"), __FILE__)
  user node[:user]
  force true
end
