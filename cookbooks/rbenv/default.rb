%w[
  autoconf
  bison
  build-essential
  libssl-dev
  libyaml-dev
  zlib1g-dev
  libncurses5-dev
  libffi-dev
  libgdbm6
  libgdbm-dev
  libdb-dev
].each do |pkg|
  package pkg
end

execute "git clone https://github.com/rbenv/rbenv.git #{ENV['HOME']}/.rbenv" do
  user node[:user]
  not_if "test -d #{ENV['HOME']}/.rbenv"
end
directory "#{ENV['HOME']}/.rbenv/plugins" do
  owner node[:user]
end
execute "git clone https://github.com/rbenv/ruby-build.git #{ENV['HOME']}/.rbenv/plugins/ruby-build" do
  user node[:user]
  not_if "test -d #{ENV['HOME']}/.rbenv/plugins/ruby-build"
end
