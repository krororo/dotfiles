define :dotfile, source: nil do
  source = params[:source] || params[:name]
  link File.join(ENV['HOME'], params[:name]) do
    to File.expand_path("../../config/#{source}", __FILE__)
    user node[:user]
    force true
  end
end

define :xdg_config, dir: nil do
  d = params[:dir]
  config_dir = File.join(ENV['HOME'], '.config', d)
  directory config_dir do
    owner node[:user]
  end
  link File.join(config_dir, params[:name]) do
    to File.expand_path("../../config/#{d}/#{params[:name]}", __FILE__)
    user node[:user]
    force true
  end
end
