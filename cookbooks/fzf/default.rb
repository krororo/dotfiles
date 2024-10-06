# frozen_string_literal: true

if node[:platform] == 'darwin'
  package 'fzf'
else
  linuxbrew 'fzf'
end
