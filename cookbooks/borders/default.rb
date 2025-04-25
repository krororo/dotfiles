brew_tap "FelixKratz/formulae"
package "borders"

xdg_config "borders/bordersrc"

execute "brew services start borders" do
  not_if "brew services list | grep borders | grep started"
end
