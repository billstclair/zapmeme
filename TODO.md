# TODO

This file tracks bugs and ideas for new features.

## Necessary Features

* Save/Restore persisted data as JSON, with optional compression to Base64.
* Word-wrap outline font SVG.
* More built-in fonts.
  Edit available fonts, in case you don't want all the supplied ones.
  Allow import of Google fonts, and maybe some other repositories.
* Trim available fonts to those which are actually available.
  Convert site/js/is-font-available.js to a custom element.
  Probe it half a second after launch, to give conditional fonts
  time to load.

## Nice Features

* Save on return in Memes dialog.
* Default saved meme name to text of first caption, <br> -> " "
  Unless it was saved under another name (not sure how to encode this).
* Space above first line of caption text.
  Not useful for bottom positions, but desirable for top and middle.
* Blank space above and/or below image. Specify color or background image.
  This is also useful with NO meme image.
* Markdown text.
* Resize handles on caption borders.
* Custom caption location, not just the nine current positions.
* Stamps and ornaments, e.g. speech bubbles.
* Custom SVG for user-defined stamps and ornaments.
* Persistence on S3.
* [Keybase integration](https://keybase.io/docs/proof_integration_guide)

## Bugs

* The PNG and JPEG buttons no longer show the image on the page in iPhone.
* The Impact font does not appear in the meme image on iPhone (may be unfixable).
