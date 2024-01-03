if node[:platform] == 'darwin'
  execute "Tap repository: emacsmacport" do
    name = 'railwaycat/emacsmacport'
    command "brew tap #{name}"
    not_if "brew tap | grep '^#{name}$'"
  end
  package 'emacs-mac'
else
  package 'emacs'
  package 'emacs-mozc-bin'
end

link "#{ENV['HOME']}/.config/emacs" do
  to File.expand_path("../../../config/emacs", __FILE__)
  user node[:user]
  force true
end
