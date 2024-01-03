dotfile '.zsh'
dotfile '.zshrc'

package 'zsh-syntax-highlighting'

if node[:platform] == 'darwin'
  package 'zsh-autosuggestions'
else
  git 'zsh-autosuggestions' do
    repository 'https://github.com/zsh-users/zsh-autosuggestions'
    destination "#{ENV['HOME']}/.zsh/zsh-autosuggestions"
    user node[:user]
    action :nothing
  end
end
