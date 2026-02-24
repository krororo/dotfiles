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

execute "edit colima config" do
  provision_config = <<~PROVISION.gsub("\n", "\\n").chomp
    provision:
      - mode: system
        script: |
          sleep 5
          cp #{ENV['HOME']}/.local/share/warp/cloudflare.crt /usr/local/share/ca-certificates/cloudflare.crt
          update-ca-certificates
          systemctl restart docker
  PROVISION
  docker_config = <<~DOCKER.gsub("\n", "\\n").chomp
    docker:
      registry-mirrors:
        - https://mirror.gcr.io
  DOCKER
  command <<~COMMAND
    sed -i .bak \
      -e 's/^provision:/,#{provision_config}/' \
      -e 's/^docker:/#{docker_config}/' \
      #{ENV['HOME']}/.config/colima/default/colima.yaml
  COMMAND
  not_if "grep -q cloudflare.crt #{ENV['HOME']}/.config/colima/default/colima.yaml"
end

local_ruby_block "edit plist" do
  plist_path = run_command("brew ls colima | grep plist").stdout.chomp
  block do
    content = File.read(plist_path)
    content.gsub!(
      %r{(<key>EnvironmentVariables</key>\s*<dict>)}m,
      "\\1\n\t\t<key>XDG_CONFIG_HOME</key>\n\t\t<string>#{ENV['HOME']}/.config</string>"
    )
    File.open(plist_path, "w") { |f| f.write(content) }
  end
  not_if "grep -q XDG_CONFIG_HOME #{plist_path}"
end

execute "brew services start colima" do
  not_if "brew services list | grep colima | grep started"
end
