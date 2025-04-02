/**
 * $ deno run --allow-env --allow-read --allow-write ./config/karabiner/karabiner.ts
 */

import { rule, map, ifApp, writeToProfile } from 'https://deno.land/x/karabinerts@1.30.1/deno.ts'

const targetApps = [
  "^com\\.tinyspeck\\.slackmacgap$",
  "^com\\.google\\.Chrome$",
  "^org\\.mozilla\\.firefox$",
]

writeToProfile(
  'Default profile', // profile name
  // '--dry-run', // プロファイル名を--dry-runにすると標準出力で確認できる
  [
    rule('My Emacs key binding')
      .manipulators([
        map('f', 'control').to('right_arrow')
          .condition(ifApp(targetApps)),
        map('b', 'control').to('left_arrow')
          .condition(ifApp(targetApps)),
        map('p', 'control').to('up_arrow')
          .condition(ifApp(targetApps)),
        map('n', 'control').to('down_arrow')
          .condition(ifApp(targetApps)),
        map('g', 'control').to('escape')
          .condition(ifApp(targetApps)),
        map('h', 'control').to('delete_or_backspace')
          .condition(ifApp(targetApps)),
      ])
  ]
)
