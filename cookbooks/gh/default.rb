if node[:platform] != 'darwin'
  keyring_path = '/etc/apt/keyrings/githubcli-archive-keyring.gpg'

  execute 'Download keyring' do
    keyring_url = 'https://cli.github.com/packages/githubcli-archive-keyring.gpg'
    command "curl -fsSL #{keyring_url} | sudo dd of=#{keyring_path} " \
            "&& sudo chmod go+r #{keyring_path}"
    not_if "test -f #{keyring_path}"
  end

  execute 'sudo apt update' do
    subscribes :run, 'file[/etc/apt/sources.list.d/github-cli.list]', :immediately
    action :nothing
  end

  file '/etc/apt/sources.list.d/github-cli.list' do
    arch = run_command('dpkg --print-architecture').stdout.chomp
    content "deb [arch=#{arch} signed-by=#{keyring_path}] " \
            "https://cli.github.com/packages stable main\n"
    mode '644'
  end
end

package 'gh'
