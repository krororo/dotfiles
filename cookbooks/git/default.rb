xdg_config 'git/config'
xdg_config 'git/commit_template'

github_release 'git-delta' do
  v = '0.12.1'
  repository 'dandavison/delta'
  version v
  filename "git-delta_#{v}_amd64.deb"
  not_if %(command -v delta && [ $(delta --version | egrep -o "([0-9]+.){2}[0-9]+") = #{v} ])
end
