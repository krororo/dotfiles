# frozen_string_literal: true

directory "#{ENV['HOME']}/.fonts" do
  owner node[:user]
end

execute "Install HackGen font" do
  v = "v2.10.0"
  script = File.expand_path("../../../lib/install_hackgen.bash", __FILE__)
  version_file_path = "#{ENV['HOME']}/.fonts/.hackgen-vesion"

  user node[:user]
  command "bash #{script} #{v}"
  not_if "test -f #{version_file_path} && grep -qx #{v} #{version_file_path}"
end
