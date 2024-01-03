xdg_config 'git/config'
xdg_config 'git/commit_template'
xdg_config 'git/ignore'

if node[:platform] == 'darwin'
  package 'git-delta'
else
  github_release 'git-delta' do
    v = '0.16.5'
    repository 'dandavison/delta'
    version v
    filename "git-delta_#{v}_amd64.deb"
    not_if %(command -v delta && [ $(delta --version | egrep -o "([0-9]+.){2}[0-9]+") = #{v} ])
  end
end
