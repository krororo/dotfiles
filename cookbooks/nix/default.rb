# frozen_string_literal: true

execute "Install nix" do
  url = "https://install.determinate.systems/nix"
  command "curl --proto '=https' --tlsv1.2 -sSf -L #{url} | sh -s -- install --no-confirm"
  user node[:user]
  not_if "test -f /nix/nix-installer"
end
