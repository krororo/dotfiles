dotfile '.docker'

%w[ca-certificates curl lsb-release].each do |pkg|
  package pkg
end

keyring_path = '/etc/apt/keyrings/docker-archive-keyring.asc'

execute 'Download keyring' do
  keyring_url = 'https://download.docker.com/linux/ubuntu/gpg'
  command "curl -fsSL #{keyring_url} -o #{keyring_path}"
  not_if "test -f #{keyring_path}"
end

execute "sudo apt update" do
  subscribes :run, "file[/etc/apt/sources.list.d/docker.list]", :immediately
  action :nothing
end

file '/etc/apt/sources.list.d/docker.list' do
  arch = run_command('dpkg --print-architecture').stdout.chomp
  lsb_release = run_command('lsb_release -cs').stdout.chomp
  content <<~CONTENT
    deb [arch=#{arch} signed-by=#{keyring_path}] https://download.docker.com/linux/ubuntu #{lsb_release} stable
  CONTENT
  mode '644'
end

%w[
  docker-ce
  docker-ce-cli
  docker-compose-plugin
  containerd.io
].each do |pkg|
  package pkg
end

execute "gpasswd -a #{node[:user]} docker" do
  not_if { run_command("id #{node[:user]}").stdout.include?('docker') }
end
