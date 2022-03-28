dotfile '.docker'

cli_plugins_dir = "#{ENV['HOME']}/.docker/cli-plugins"
directory cli_plugins_dir do
  owner node[:user]
end

execute 'Download docker-compose' do
  v = 'v2.3.4'
  url = "https://github.com/docker/compose/releases/download/#{v}/docker-compose-linux-x86_64"

  user node[:user]
  command "curl -sL -o #{cli_plugins_dir}/docker-compose #{url}"
  only_if %(command -v docker && [ $(docker compose version | egrep -o "v([0-9]+\\\\.){2}[0-9]+") != #{v} ])
end
