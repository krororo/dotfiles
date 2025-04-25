if node[:platform] == 'darwin'
  brew_tap "d12frosted/emacs-plus"
  execute "brew install emacs-plus --with-xwidgets --with-imagemagick --with-modern-pen-icon" do
    # Optimized `brew list` command
    # ref. https://github.com/mizzy/specinfra/blob/v2.92.0/lib/specinfra/command/darwin/base/package.rb#L46
    not_if "ls -1 $(brew --prefix)/Cellar/ | grep ^emacs-plus"
  end

  link "/Applications/Emacs.app" do
    to "/opt/homebrew/opt/emacs-plus/Emacs.app"
    force true
  end
else
  package 'emacs'
  package "emacs-mozc"
end

link "#{ENV['HOME']}/.config/emacs" do
  to File.expand_path("../../../config/emacs", __FILE__)
  user node[:user]
  force true
end

directory "#{ENV['HOME']}/.local/share/emacs" do
  user node[:user]
end
