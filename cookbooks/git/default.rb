xdg_config 'git/config'
xdg_config 'git/commit_template'
xdg_config 'git/ignore'
xdg_config "git/worktree-add"
xdg_config "git/worktree-delete"

if node[:platform] == 'darwin'
  package 'git-delta'

  package "k1LoW/tap/git-wt"
else
  github_release 'git-delta' do
    v = '0.18.2'
    repository 'dandavison/delta'
    version v
    filename "git-delta_#{v}_amd64.deb"
    not_if %(command -v delta && [ $(delta --version | egrep -o "([0-9]+.){2}[0-9]+") = #{v} ])
  end

  github_release "git-wt" do
    v = "0.26.2"
    repository "k1LoW/git-wt"
    version "v#{v}"
    filename "git-wt_#{v}-1_amd64.deb"
    not_if %(command -v git-wt && [ $(git-wt --version | egrep -o "([0-9]+.){2}[0-9]+") = #{v} ])
  end
end
