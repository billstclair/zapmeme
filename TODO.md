# TODO

This file tracks bugs and ideas for new features.

## Necessary Features

* Persist everything.
* Save/Restore persisted data as JSON, with optional compression to Base64.
* More built-in fonts.
  Outline fonts: [urbanfonts.com](https://www.urbanfonts.com/fonts/outline-fonts.htm),
  [Google (3 hits)](https://fonts.google.com/?query=outline),
  [Google search](https://www.google.com/search?q=fonts+with+outlines).
  * "You can fake outlines using 4 duplicates of the same text with a different
     color, offset by n pixels in all directions, placed under the main text."
     -- @meowski
    See [site/svg-outline.html](site/svg-outline.html).
    It will be easy to provide an "outlined SVG" mode for captions,
    as long as the user is happy to provide line breaks via "<br>".
* Edit available fonts, in case you don't want all the supplied ones.
* Allow import of Google fonts, and maybe some other repositories.

## Nice Features

* Space above first line of caption text.
  Not useful for bottom positions, but desirable for top and middle.
* Markdown text.
* Resize handles on caption borders.
* Custom caption location, not just the nine current positions.
* Stamps and ornaments, e.g. speech bubbles.
* Custom SVG for user-defined stamps and ornaments.

## Bugs

* Warn the user when they use a URL to specify the background image that it
  won't appear in the downloaded image file.
