xdg_config 'xremap/config.yml'

github_release 'xremap' do
  repository 'k0kubun/xremap'
  v = '0.3.0'
  version "v#{v}"
  filename 'xremap-linux-x86_64-x11.zip'
  bin_path = "#{ENV['HOME']}/bin/xremap"
  not_if "test -f #{bin_path} && [ $(#{bin_path} --version | egrep -o '([0-9]+.){2}[0-9]+') = #{v} ]"
end

execute "gpasswd -a #{node[:user]} input" do
  not_if "groups #{node[:user]} | grep -q input"
end

execute "echo 'KERNEL==\"uinput\", GROUP=\"input\"' > /etc/udev/rules.d/input-xremap.rules" do
  not_if 'test -f /etc/udev/rules.d/input-xremap.rules'
end

template "#{ENV['HOME']}/.config/systemd/user/xremap.service" do
  source 'templates/xremap.service.erb'
  mode '644'
  owner node[:user]
  notifies :run, 'execute[systemctl daemon-reload]', :immediately
end

execute "systemctl daemon-reload" do
  action :nothing
  command "sudo -E -u #{node[:user]} systemctl --user daemon-reload"
  notifies :run, 'execute[systemctl enable xremap.service]', :immediately
end

execute "systemctl enable xremap.service" do
  action :nothing
  command "sudo -E -u #{node[:user]} systemctl --user enable xremap.service"
end
