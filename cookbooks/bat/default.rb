xdg_config "bat/config"

package "bat"

if node[:platform] != "darwin"
  directory "#{ENV['HOME']}/.local/bin" do
    user node[:user]
  end

  link "#{ENV['HOME']}/.local/bin/bat" do
    to '/usr/bin/batcat'
    user node[:user]
    force true
  end
end
