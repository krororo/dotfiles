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

config.window_frame = {
  inactive_titlebar_bg = "#44475A",
  active_titlebar_bg = "#BD93F9",
  inactive_titlebar_fg = "#44475A",
  active_titlebar_fg = "#F8F8F2",
}

config.colors = {
  tab_bar = {
    active_tab = {
      bg_color = "#8b7500",
      fg_color = "#F8F8F2",
    },
  },
}

config.show_new_tab_button_in_tab_bar = false
-- config.show_close_tab_button_in_tabs = false

-- key bindings

local act = wezterm.action
local darwin = wezterm.target_triple:find('darwin')

config.leader = { key = 'x', mods = 'CTRL', timeout_milliseconds = 1000 }
config.keys = {
  {
    key = '0',
    mods = 'LEADER',
    action = act.PaneSelect,
  },
  {
    key = '2',
    mods = 'LEADER',
    action = act.SplitVertical { domain = 'CurrentPaneDomain' },
  },
  {
    key = '3',
    mods = 'LEADER',
    action = act.SplitHorizontal { domain = 'CurrentPaneDomain' },
  },
}

super2alt_keys = { 'f', 'b', 'd', 'Backspace', '.' }
for _, k in ipairs(super2alt_keys) do
  table.insert(config.keys, {
    key = k,
    mods = 'SUPER',
    action = act.SendKey { key = k, mods = 'ALT' },
  })
end

for i = 1, 8 do
  table.insert(config.keys, {
    key = tostring(i),
    mods = 'ALT',
    action = act.ActivateTab(i - 1),
  })
end

if not (darwin) then
  table.insert(config.keys, {
    key = 't', mods = 'ALT', action = act.SpawnTab 'CurrentPaneDomain',
  })
  table.insert(config.keys, {
    key = 'c', mods = 'ALT', action = act.CopyTo 'ClipboardAndPrimarySelection',
  })
  table.insert(config.keys, {
    key = 'v', mods = 'ALT', action = act.PasteFrom 'Clipboard',
  })
end

-- mouse bindings

config.mouse_bindings = {
  {
    event = { Up = { streak = 1, button = 'Left' } },
    mods = 'NONE',
    action = act.CompleteSelection 'ClipboardAndPrimarySelection'
  },
  {
    event = { Up = { streak = 1, button = 'Left' } },
    mods = 'CTRL',
    action = act.OpenLinkAtMouseCursor,
  },
}

-- other

config.audible_bell = "Disabled"

return config
