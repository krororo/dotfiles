if node[:platform] != 'darwin'
  keyring_path = '/etc/apt/keyrings/mise-archive-keyring.gpg'

  execute 'Download keyring' do
    command "curl -fsSL https://mise.jdx.dev/gpg-key.pub | gpg --dearmor | sudo tee #{keyring_path} 1> /dev/null"
    not_if "test -f #{keyring_path}"
  end

  execute 'sudo apt update' do
    subscribes :run, 'execute[Add apt source]', :immediately
    action :nothing
  end

  execute 'Add apt source' do
    command 'echo "deb [arch=$(dpkg --print-architecture) ' \
            "signed-by=#{keyring_path}] " \
            'https://mise.jdx.dev/deb stable main" ' \
            '| sudo tee /etc/apt/sources.list.d/mise.list > /dev/null'
    not_if "test -f /etc/apt/sources.list.d/mise.list"
  end
end

package 'mise'

xdg_config 'mise/config.toml'

# for ruby-build
if node[:platform] == 'darwin'
  %w[openssl@3 readline libyaml gmp rust]
else
  %w[
    autoconf
    patch
    build-essential
    rustc
    libssl-dev
    libyaml-dev
    libreadline-dev
    zlib1g-dev
    libgmp-dev
    libncurses-dev
    libffi-dev
    libgdbm6
    libgdbm-dev
    libdb-dev
    uuid-dev
  ]
end.each do |pkg|
  package pkg
end

if node[:platform] == 'darwin'
  # TODO
else
  execute 'mise completion zsh > /usr/share/zsh/site-functions/_mise' do
    not_if 'test -f /usr/share/zsh/site-functions/_mise'
  end
end
