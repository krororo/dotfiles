directory "#{ENV['HOME']}/bin" do
  user node[:user]
end

package 'coreutils'
package "deno"
package 'findutils'
package "ghq"
package "github-mcp-server"
package 'grep'
package 'noborus/tap/ov'
package 'watch'
package "yq"

cask "1password-cli"
cask 'alt-tab'
cask "claude"
cask "claude-code@latest"
cask "copilot-cli"
cask 'deepl'
cask 'devtoys'
cask "font-hackgen"
cask "font-hackgen-nerd"
cask 'font-noto-color-emoji'
cask "karabiner-elements"
cask "macskk"
cask "meetingbar"
cask "notion"
cask 'raycast'

xdg_config "karabiner/karabiner.json"

include_recipe "../../cookbooks/aider"
include_recipe "../../cookbooks/borders"
include_recipe "../../cookbooks/colima"
include_recipe "../../cookbooks/macos_key_bindings"
