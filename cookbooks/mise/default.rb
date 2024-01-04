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
