define :dotfile, source: nil do
  source = params[:source] || params[:name]
  link File.join(ENV['HOME'], params[:name]) do
    to File.expand_path("../../config/#{source}", __FILE__)
    user node[:user]
    force true
  end
end

define :xdg_config do
  d, f = params[:name].split('/', 2)
  config_dir = File.join(ENV['HOME'], '.config', d)
  directory config_dir do
    owner node[:user]
  end
  link File.join(config_dir, f) do
    to File.expand_path("../../config/#{d}/#{f}", __FILE__)
    user node[:user]
    force true
  end
end

define :github_release, repository: nil, version: nil, filename: nil do
  raise 'repository is required' unless params[:repository]
  raise 'version is required' unless params[:version]
  raise 'filename is required' unless (filename = params[:filename])

  url = "https://github.com/#{params[:repository]}/releases/download/#{params[:version]}/#{filename}"

  execute "curl -sL -o /tmp/#{filename} #{url}"
  case filename
  when /\.deb\z/
    execute "dpkg -i /tmp/#{filename}"
  when /\.zip\z/
    execute "unzip -o /tmp/#{filename}" do
      cwd '/tmp'
      user node[:user]
    end
    cmd = params[:name]
    bin_path = "#{ENV['HOME']}/bin/#{cmd}"
    execute "mv /tmp/#{cmd} #{bin_path} && chmod +x #{bin_path}" do
      user node[:user]
    end
  else
    raise "unknown type: #{filename}"
  end
end
