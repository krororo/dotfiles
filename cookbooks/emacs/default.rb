if node[:platform] == 'darwin'
  brew_tap "d12frosted/emacs-plus"
  cask "emacs-plus-app"
else
  package 'emacs'
  package "emacs-mozc"

  # for compile vterm
  package "cmake"
  package "libtool-bin"
end

link "#{ENV['HOME']}/.config/emacs" do
  to File.expand_path("../../../config/emacs", __FILE__)
  user node[:user]
  force true
end

directory "#{ENV['HOME']}/.local/share/emacs" do
  user node[:user]
end
