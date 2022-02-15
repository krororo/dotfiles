xdg_config 'git/config'
xdg_config 'git/commit_template'

github_release 'git-delta' do
  repository 'dandavison/delta'
  version '0.12.0'
  filename 'git-delta_0.12.0_amd64.deb'
  not_if 'command -v delta'
end
