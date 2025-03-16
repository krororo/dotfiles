if node[:platform] == 'darwin'
  package 'fd'
else
  package 'fd-find'

  directory "#{ENV['HOME']}/.local/bin" do
    user node[:user]
  end

  link "#{ENV['HOME']}/.local/bin/fd" do
    to '/usr/bin/fdfind'
    user node[:user]
    force true
  end
end
