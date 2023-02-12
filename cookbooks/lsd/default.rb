github_release 'lsd' do
  v = '0.23.1'
  repository 'Peltoche/lsd'
  version v
  filename "lsd_#{v}_amd64.deb"
  not_if %(command -v lsd && [ $(lsd --version | egrep -o "([0-9]+.){2}[0-9]+") = #{v} ])
end
