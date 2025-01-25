# frozen_string_literal: true

if node[:platform] == 'darwin'
  execute "Install Homebrew" do
    url = "https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh"
    command %(NONINTERACTIVE=1 bash -c "$(curl -fsSL #{url})")
    user node[:user]
    not_if "test -d /opt/homebrew"
  end
end
