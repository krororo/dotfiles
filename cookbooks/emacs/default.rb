package 'emacs'
package 'emacs-mozc-bin'

link "#{ENV['HOME']}/.config/emacs" do
  to File.expand_path("../../../config/emacs", __FILE__)
  user node[:user]
  force true
end
