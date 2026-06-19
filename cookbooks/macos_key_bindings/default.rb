# ref. https://texwiki.texjp.org/?Mac#defaultkeybinding-dict
file "#{ENV['HOME']}/Library/KeyBindings/DefaultKeyBinding.dict" do
  content <<~CONTENT
    {
      "¥" = ("insertText:", "\\\\");
      "~\\\\" = ("insertText:", "¥");
    }
  CONTENT
  mode "644"
end
