dotfile ".docker"

package "colima"
package "docker"
package "docker-compose"
package "docker-credential-helper"

execute "add cliPluginsExtraDirs to ~/.docker/config.json" do
  config_path = "#{ENV['HOME']}/.docker/config.json"
  add_value = '{ "cliPluginsExtraDirs": ["/opt/homebrew/lib/docker/cli-plugins"] }'
  tmp_path = "/tmp/docker-config.json"
  command <<~COMMAND
    cat #{config_path} | jq '. + #{add_value}' > #{tmp_path} && mv #{tmp_path} #{config_path}
  COMMAND
  not_if "grep -q cliPluginsExtraDirs #{ENV['HOME']}/.docker/config.json"
end

# TODO: インストール直後はファイルが存在しないのでなんとかしたい
local_ruby_block "edit colima config" do
  config_path = "#{ENV['HOME']}/.config/colima/default/colima.yaml"
  block do
    provision_config = <<~PROVISION.chomp
      provision:
        - mode: system
          script: |
            sleep 5
            cp #{ENV['HOME']}/.local/share/warp/cloudflare.crt /usr/local/share/ca-certificates/cloudflare.crt
            update-ca-certificates
            systemctl restart docker
    PROVISION
    docker_config = <<~DOCKER.chomp
      docker:
        registry-mirrors:
          - https://mirror.gcr.io
    DOCKER

    content = File.read(config_path)
    content.gsub!(/^provision:.*$/, provision_config)
    content.gsub!(/^docker:.*$/, docker_config)

    File.open(config_path, "w") { |f| f.write(content) }
  end
  not_if "test ! -f #{config_path} || grep -q cloudflare.crt #{config_path}"
end

plist_path = "#{ENV['HOME']}/.config/colima/colima.plist"
template plist_path do
  source "templates/colima.plist.erb"
  mode "644"
  owner node[:user]
end

execute "brew services start colima --file=#{plist_path}" do
  not_if "brew services list | grep colima | grep started"
end
