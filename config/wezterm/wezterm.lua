local wezterm = require 'wezterm'
local config = {}

config = wezterm.config_builder()

-- font

config.font = wezterm.font_with_fallback {
  "HackGen Console NF"
}
config.font_size = 16

-- window

config.color_scheme = 'iceberg-dark'
config.window_background_opacity = 0.85
config.macos_window_background_blur = 20

local mux = wezterm.mux
wezterm.on('on-startup', function(cmd)
  local tab, pane, window = mux.spawn_window(cmd or {width=140, height=50})
end)

-- key bindings

local act = wezterm.action

config.keys = {
  {
    key = 'f',
    mods = 'SUPER',
    action = act.SendKey { key = 'f', mods = 'ALT' },
  },
  {
    key = 'b',
    mods = 'SUPER',
    action = act.SendKey { key = 'b', mods = 'ALT' },
  },
  {
    key = 'Backspace',
    mods = 'SUPER',
    action = act.SendKey { key = 'Backspace', mods = 'ALT' },
  },
}

for i = 1, 8 do
  table.insert(config.keys, {
    key = tostring(i),
    mods = 'ALT',
    action = act.ActivateTab(i - 1),
  })
end

-- other

config.audible_bell = "Disabled"

return config
