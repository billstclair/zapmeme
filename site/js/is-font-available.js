//////////////////////////////////////////////////////////////////////
//
// is-font-available.js
// Define the `is-font-available` custom element.
// Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
// Custom element code by Bill St. Clair.
// Copyright for isFontAvailable function is below.
//
//////////////////////////////////////////////////////////////////////

(function (document) {

  // <is-font-available> custom element.
  customElements.define('is-font-available', class extends HTMLElement {
    constructor() {
      super();
      this._trigger = null;
    }

    connectedCallback() {
      // Move along. Nothing to see here.
    }

    get trigger() {
      return this._trigger;
    }

    get fonts () {
      return this._fonts;
    }

    set fonts (fonts) {
      this._fonts = fonts;
    }

    get availableFonts() {
      return this._availableFonts || [];
    }

    set trigger(trigger) {
      // Don't trigger on first set
      var doit = this._trigger !== null && this._trigger != trigger;
      this._trigger = trigger;
      if (doit) {
        var fonts = this._fonts;
        var res = {};
        if (typeof(fonts) == 'object') {
          for (var i in fonts) {
            var font = fonts[i];
            res[font] = isFontAvailable(font);
          }
        }
        this._availableFonts = res;
        this.dispatchEvent(new CustomEvent('availableFonts'));
      }
    }
  });

/**
 * Checks if a font is available to be used on a web page.
 *
 * @param {String} fontName The name of the font to check
 * @return {Boolean}
 * @license MIT
 * @copyright Sam Clarke 2013
 * @author Sam Clarke <sam@samclarke.com>
 * @link https://www.samclarke.com/javascript-is-font-available
 */

  var width;
  var body = document.body;

  var container = document.createElement('span');
  container.innerHTML = Array(100).join('wi');
  container.style.cssText = [
    'position:absolute',
    'width:auto',
    'font-size:128px',
    'left:-99999px'
  ].join(' !important;');

  var getWidth = function (fontFamily) {
    container.style.fontFamily = fontFamily;

    body.appendChild(container);
    width = container.clientWidth;
    body.removeChild(container);

    return width;
  };

  // Pre compute the widths of monospace, serif & sans-serif
  // to improve performance.
  var monoWidth  = getWidth('monospace');
  var serifWidth = getWidth('serif');
  var sansWidth  = getWidth('sans-serif');

  function isFontAvailable (font) {
    return monoWidth !== getWidth(font + ',monospace') ||
      sansWidth !== getWidth(font + ',sans-serif') ||
      serifWidth !== getWidth(font + ',serif');
  };

})(document);
