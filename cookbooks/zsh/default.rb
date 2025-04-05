dotfile '.zsh'
dotfile '.zshrc'

package 'zsh-syntax-highlighting'

git 'zsh-autosuggestions' do
  repository 'https://github.com/zsh-users/zsh-autosuggestions'
  destination "#{ENV['HOME']}/.zsh/zsh-autosuggestions"
  user node[:user]
end

if node[:platform] != "darwin"
  execute "chsh -s /usr/bin/zsh #{node[:user]}" do
    not_if "grep -qP '#{node[:user]}.+/usr/bin/zsh' /etc/passwd"
  end
end
