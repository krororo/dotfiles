keyring_path = "/etc/apt/keyrings/packages.mozilla.org.asc"

execute "Download keyring" do
  command "curl -fsSL https://packages.mozilla.org/apt/repo-signing-key.gpg | sudo tee #{keyring_path} 1> /dev/null"
  not_if "test -f #{keyring_path}"
end

execute "sudo apt update" do
  subscribes :run, "file[/etc/apt/preferences.d/mozilla]", :immediately
  action :nothing
end

file "/etc/apt/sources.list.d/mozilla.list" do
  content "deb [signed-by=#{keyring_path}] " \
          "https://packages.mozilla.org/apt mozilla main\n"
  mode "644"
end

file "/etc/apt/preferences.d/mozilla" do
  content <<~CONTENT
    Package: *
    Pin: origin packages.mozilla.org
    Pin-Priority: 1000
  CONTENT
  mode "644"
end

package "firefox-l10n-ja"
