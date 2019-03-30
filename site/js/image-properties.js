//////////////////////////////////////////////////////////////////////
//
// image-properties.js
// Define the `image-properties` custom element.
// Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////

customElements.define('image-properties', class extends HTMLElement {
  constructor() {
    super();
    this._imageId = null;
    this._triggerImageProperties = null;
  }

  connectedCallback() {
    // Move along. Nothing to see here.
  }

  get imageId() {
    return this._imageId;
  }

  set imageId(value) {
    this._imageId = value;
  }

  get triggerImageProperties() {
    return this._triggerImageProperties;
  }

  set triggerImageProperties(value) {
    // Don't trigger on first set.
    var doit = this._triggerImageProperties !== null;
    this._triggerImageProperties = value;
    if (doit) {
      this._width = -1;
      this._height = -1;

      var that = this;
      function dispatch() {
        that.dispatchEvent(new CustomEvent('imageProperties'));
      }

      var image = this.image
      var tagName = image.tagName;
      if (typeof(tagName) == "string") {
        tagName = tagName.toLowerCase();
        if (tagName == "img") {
          // It's an <img>, the natural width and height are available now.
          this._width = image.naturalWidth;
          this._height = image.naturalHeight;
          window.setTimeout(dispatch, 1);
        } else if (tagName == "image") {
          var href = image.href;
          if (typeof(href) == 'object') {
            href = href.baseVal;
            if (typeof(href) == "string") {
              // It's an SVG <image>, need to ask an <img> to load its URL.
              var img = new Image();
              var that = this;
              img.onload = function() {
                that._width = img.naturalWidth;
                that._height = img.naturalHeight;
                // Probably don't need the additional delay here,
                // but it won't hurt.
                window.setTimeout(dispatch, 1);
              }
              img.src = href;
            }
          }
        }
      }
    }
  }

  get image() {
    if (this._imageId) {
      var element = document.getElementById(this._imageId);
      if (typeof(element) == 'object') {
        return element;
      }
    }
    return null;
  }

  get imageProperties() {
    return { id: this.imageId,
             width: this._width || -1,
             height: this._height || -1
           }
  }
});
