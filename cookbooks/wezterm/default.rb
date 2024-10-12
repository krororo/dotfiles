# frozen_string_literal: true

if node[:platform] == "darwin"
  cask "wezterm"
else
  keyring_path = "/etc/apt/keyrings/wezterm-keyring.asc"

  execute "Download keyring" do
    keyring_url = "https://apt.fury.io/wez/gpg.key"
    command "curl -fsSL #{keyring_url} | sudo tee #{keyring_path}"
    not_if "test -f #{keyring_path}"
  end

  execute "sudo apt update" do
    subscribes :run, "file[/etc/apt/sources.list.d/wezterm.list]", :immediately
    action :nothing
  end

  file "/etc/apt/sources.list.d/wezterm.list" do
    arch = run_command("dpkg --print-architecture").stdout.chomp
    content "deb [arch=#{arch} signed-by=#{keyring_path}] " \
            "https://apt.fury.io/wez/ * *\n"
    mode "644"
  end

  package "wezterm"
end

xdg_config "wezterm/wezterm.lua"
