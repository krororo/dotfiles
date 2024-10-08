if node[:platform] != 'darwin'
  keyring_path = '/etc/apt/keyrings/mise-archive-keyring.asc'

  execute 'Download keyring' do
    command "curl -fsSL https://mise.jdx.dev/gpg-key.pub | sudo tee #{keyring_path} 1> /dev/null"
    not_if "test -f #{keyring_path}"
  end

  execute 'sudo apt update' do
    subscribes :run, 'file[/etc/apt/sources.list.d/mise.list]', :immediately
    action :nothing
  end

  file '/etc/apt/sources.list.d/mise.list' do
    arch = run_command('dpkg --print-architecture').stdout.chomp
    content "deb [arch=#{arch} signed-by=#{keyring_path}] " \
            "https://mise.jdx.dev/deb stable main\n"
    mode "644"
  end
end

package 'mise'

link "#{ENV['HOME']}/.config/mise" do
  to File.expand_path("../../../config/mise", __FILE__)
  user node[:user]
  force true
end

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
    libgdbm6t64
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
