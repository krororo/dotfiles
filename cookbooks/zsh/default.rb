dotfile '.zsh'
dotfile '.zshrc'

git 'zsh-autosuggestions' do
  repository 'https://github.com/zsh-users/zsh-autosuggestions'
  destination File.expand_path('../../../config/.zsh/zsh-autosuggestions', __FILE__)
  user node[:user]
end
