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

execute "gpasswd -a #{node[:user]} docker" do
  not_if { run_command("id #{node[:user]}").stdout.include?('docker') }
end

cli_plugins_dir = "#{ENV['HOME']}/.docker/cli-plugins"
directory cli_plugins_dir do
  owner node[:user]
end

execute 'Download docker-compose' do
  v = 'v2.29.1'
  url = "https://github.com/docker/compose/releases/download/#{v}/docker-compose-linux-x86_64"

  user node[:user]
  command "curl -sL -o #{cli_plugins_dir}/docker-compose #{url} && chmod +x #{cli_plugins_dir}/docker-compose"
  only_if do
    next true unless File.exist?("#{cli_plugins_dir}/docker-compose")

    m = run_command('docker compose version').stdout.match(/v(\d+\.){2}\d+/)
    m && m[0] != v
  end
end
