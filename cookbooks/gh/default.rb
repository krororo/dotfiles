if node[:platform] != 'darwin'
  execute 'Download keyring' do
    keyring_url = 'https://cli.github.com/packages/githubcli-archive-keyring.gpg'
    keyring_path = '/usr/share/keyrings/githubcli-archive-keyring.gpg'
    command "curl -fsSL #{keyring_url} | sudo dd of=#{keyring_path}"
    not_if "test -f #{keyring_path}"
  end

  execute 'sudo apt update' do
    subscribes :run, 'execute[Add apt source]', :immediately
    action :nothing
  end

  execute 'Add apt source' do
    command 'echo "deb [arch=$(dpkg --print-architecture) ' \
            'signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] ' \
            'https://cli.github.com/packages stable main" ' \
            '| sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null'
    not_if "test -f /etc/apt/sources.list.d/github-cli.list"
  end
end

package 'gh'
