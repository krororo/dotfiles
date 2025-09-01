package 'coreutils'
package "deno"
package 'findutils'
package "fzf"
package "ghq"
package "github-mcp-server"
package 'grep'
package 'noborus/tap/ov'
package "uv"
package 'watch'
package "yq"

cask "1password-cli"
cask 'alt-tab'
cask 'deepl'
cask 'devtoys'
cask "font-hackgen"
cask "font-hackgen-nerd"
cask 'font-noto-color-emoji'
# cask 'karabiner-elements' # TBD
cask "meetingbar"
cask "notion"
cask 'raycast'
cask "superwhisper"

xdg_config "karabiner/karabiner.json"

include_recipe "../../cookbooks/aider"
include_recipe "../../cookbooks/borders"
include_recipe "../../cookbooks/colima"
