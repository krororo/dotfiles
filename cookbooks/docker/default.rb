dotfile '.docker'

%w[ca-certificates curl gnupg lsb-release].each do |pkg|
  package pkg
end

keyring_path = '/usr/share/keyrings/docker-archive-keyring.gpg'

execute 'Download keyring' do
  keyring_url = 'https://download.docker.com/linux/ubuntu/gpg'
  command "curl -fsSL #{keyring_url} | sudo gpg --dearmor -o #{keyring_path}"
  not_if "test -f #{keyring_path}"
end

file '/etc/apt/sources.list.d/docker.list' do
  arch = run_command('dpkg --print-architecture').stdout.chomp
  lsb_release = run_command('lsb_release -cs').stdout.chomp
  content <<~CONTENT
    deb [arch=#{arch} signed-by=#{keyring_path}] https://download.docker.com/linux/ubuntu #{lsb_release} stable
  CONTENT
  mode '644'
end

%w[docker-ce docker-ce-cli containerd.io].each do |pkg|
  package pkg
end

cli_plugins_dir = "#{ENV['HOME']}/.docker/cli-plugins"
directory cli_plugins_dir do
  owner node[:user]
end

execute 'Download docker-compose' do
  v = 'v2.4.1'
  url = "https://github.com/docker/compose/releases/download/#{v}/docker-compose-linux-x86_64"

  user node[:user]
  command "curl -sL -o #{cli_plugins_dir}/docker-compose #{url}"
  only_if %(command -v docker && [ $(docker compose version | egrep -o "v([0-9]+\\\\.){2}[0-9]+") != #{v} ])
end
