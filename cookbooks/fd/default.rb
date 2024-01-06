if node[:platform] == 'darwin'
  package 'fd'
else
  package 'fd-find'

  link "#{ENV['HOME']}/.local/bin/fd" do
    to '/usr/bin/fdfind'
    user node[:user]
    force true
  end
end
