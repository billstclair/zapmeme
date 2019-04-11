# State Machines for LocalStorage

This file documents the interaction with persistent `LocalStorage`. Code is in [src/Main.elm](src/Main.elm). Look for "State machines".

## Prefix

This is the prefix of all `LocalStorage` keys. Enables multiple applications run from the same domain.

```
localStoragePrefix = "zapmeme"
```
 
## Keys

The `persistenceKeys` are used as `LocalStorage` keys. Singular key names refer to a single key/value pair. Plural key names get a subkey added to store multiple objects of that type.

```
persistenceKeys =
    { model = "model"
    , meme = "meme"
    , shownimageurl = "shownimageurl"
    , memes = "memes"
    , images = "images"
    , imageurls = "imageurls"
    , thumbnails = "thumbnails"
    }
```

* `model` is a `SavedModel`
* `meme` is the current `Meme`, with only the `hash`, not the `url` in 
   the encoding of its `Image`.
* `shownimageurl` is the `data:` URL of the meme, as a single image.
* `memes.<name>` is the saved `Meme` named `<name>`.
* `images.<hash>` is a flag, marking the given image hash as existing.
   This is used so we don't have to load a whole image `data:` URL
   to determine that an image has already been saved.
* `imageurls.<hash>` is the URL with the given MD5 `<hash>`.
   Nothing is done to handle hash collision. We just assume it won't happen.
   This is a `data:` URL for uploaded images, or a standard `http[s]:` image,
   if the user entered it that way.
* `thumbnails.<hash>` is a `data:` URL for a thumbnail version of the image
   with the given MD5 `<hash>`.

## State Machines

The `PortFunnel.LocalStorage.Sequence` module is used for state machines for operations that take multiple trips to `LocalStorage`. This is experimental at present. If it works well, I'll add it to `billstclair/elm-localstorage`.

The state machines are all driven off of `labels` passed to `PortFunnel.LocalStorage.getLabeled` and `PortFunnel.LocalStorage.listKeysLabeled`.

```
label =
    { -- Simple
      saveImage = "saveImage"
    , getImageFromDialog = "getImageFromDialog"
    , prepareMemeDialog = "prepareMemeDialog"
    -- Complex
    , startup = "startup"
    , prepareImageDialog = "prepareImageDialog"
    , loadData = "loadData"
    }
```

## Simple state machines

`saveImage` loads the `images.<hash>` flag, and, if it doesn't exist, stores the `image.<hash>` URL.

`getImageFromDialog` loads the image associated with a thumbnail displayed in the "Images" dialog.

`prepareMemeDialog` does a `listKeys` for the names of the saved memes.

## Complex state machines

`startup` is used to load the `SavedModel` at startup, and its associated `Meme` & `Image`, and, sometimes, saved image URL.

`prepareImageDialog` lists all the image hashes, probes each one for a saved thumbnail image, creates the thumbnail if it doesn't exist, lists all the memes, then loads each meme and saves the associated image hash, indexing image hash to a list of meme names and meme name to an image hash.

`loadData` lists the the saved meme names, loads each one, lists the saved image names, and loads each one. Or the lists of meme names and image hashes can be supplied, if it comes from the "Images" dialog.
