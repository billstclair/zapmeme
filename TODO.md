# TODO

This file tracks bugs and ideas for new features.

## Necessary Features

* I just got a flash of how to do embedded images, stamps, voice bubbles, whatever you want to call them, for ZAP Meme.
  1. Let a caption optionally be positioned anywhere in the page, not just one of the current nine positions. The stored coordinate will be its center, so that resizing it doesn't cause it to move, just resize.
  2. A caption can either be text, as it is now, or another meme. SVG makes it really easy to just call the rendering code recursively, offset and resized (and rotated or mirrored).
  3. A meme can easily include itself. The rendering code just needs recursive depth and size limits, so that it stops.
  4. And, of course, meme captions need to have an opacity, so might as well let that apply to the textual ones as well.
  5. Need warnings when overwriting a saved meme that it's used by others.<br/>
  ∞. YOW!
* Word-wrap outline font SVG.
* More built-in fonts.
  Edit available fonts, in case you don't want all the supplied ones.
  Allow import of Google fonts, and maybe some other repositories.

## Nice Features

* Persistence on S3.
  * This opens up the idea of general web-hosted memes and images.
  1.  zapmeme.com needs to provide a proxy service, since JavaScript is
      allowed to download only from the server hosting the JS or
      servers that implement CORS.
  2.  In order to keep it from being abused, it will require an OAuth login,
      to Google, Facebook, GitHub, or Gab (Twitter doesn't do proper OAuth).
      Any login will get you some limited bandwidth per time period.
      A paid account (e.g. $1/month, $10/year) will open that up considerably.
  3. Fetches are accompanied by file length and hash, and are stopped by the
     server if longer or no hash match or JSON doesn't parse.
     This implies a "Make Link" function, which paid & S3 accounts will have,
     and which downloads a link, checks the JSON, and creates a proxy link
     with length & hash included.
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

## Bugs

* The PNG and JPEG buttons no longer show the image on the page in iPhone (sometimes).
* The Impact font does not appear in the meme image on iPhone (may be unfixable).
