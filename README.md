# ZAP Meme

This is the meme maker I've always wanted. There are a lot of them out there, but this one works entirely from a single HTML file, which you can download and keep your own copy of, if you like. It has no dependency on a server for anything but that. Everything is stored in your browser's local storage database.

The project is live at https://zapmeme.com

During development, this will be via two files, `index.html`, and `elm.js`. Once it works, I'll package `elm.js` inside of `index.html` for shipping.

The code runs in `elm reactor`.

```
git clone git@github.com:billstclair/zapmeme.git
cd zapmeme
elm reactor
```

Then aim your browser at http://localhost:8000/src/Main.elm

## Technical details

In order to size the text to the background image, we need to know the size of the uploaded background image. This relies on the `HtmlImageElement.naturalWidth` and `HtmlImageElement.naturalHeight` properties, as documented at https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement. They are accessed in Elm via `billstclair/elm-image-properties` (to be done).

I plan to use the ideas in Matti Paksula's [From SVG to Canvas and Back](http://svgopen.org/2010/papers/62-From_SVG_to_Canvas_and_Back/) to enable converting the meme to a PNG file for download.

Samuli Kaipiainen's code based on Paksula's paper is live at http://sampumon.github.io/SVG.toDataURL/butterfly_test.html, and is stored locally in `site/butterfly-test.html` and `site/svg_todataurl.js` (and `site/base64.js` for IE). The NPM package that generates `svg_todataurl.js` is at https://www.npmjs.com/package/svgtodatauri.

You can run the local copy by starting `elm reactor` as documented above, and aiming your browser at http://localhost:8000/site/butterfly-test.html.

I plan to support all the fonts at https://fonts.google.com. You'll just have to name one of them, and it will be added to the `<head>` and CSS automatically. Out of the box, I'm supporting the safe fonts listed at http://web.mit.edu/jmorzins/www/fonts.html with CSS at http://web.mit.edu/jmorzins/www/@/css/fonts.css

Copyright 2019, Bill St. Clair<br/>
Distributed under the MIT License. See [LICENSE](LICENSE).
