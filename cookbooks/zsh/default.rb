dotfile '.zsh'
dotfile '.zshrc'

package 'zsh-syntax-highlighting'

git 'zsh-autosuggestions' do
  repository 'https://github.com/zsh-users/zsh-autosuggestions'
  destination "#{ENV['HOME']}/.zsh/zsh-autosuggestions"
  user node[:user]
end
