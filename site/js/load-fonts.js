//////////////////////////////////////////////////////////////////////
//
// load-fonts.js
// Conditionally load commercial fonts, only on mobile devices.
// Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
//////////////////////////////////////////////////////////////////////

(function() {
  // I tried this in the  <head>, but impact.css got loaded on browsers with mice.
  // JavaScript to the rescue.
  // <link media='(hover: none)' rel='stylesheet' type='text/css' href='fonts/myfonts/impact.css'/>
  // Modified slightly from https://stackoverflow.com/a/577002
  if ("function" == typeof(window.matchMedia)) {
    var mql = window.matchMedia('(hover: none)');
    if (mql.matches) {
      var cssId = 'impactFont';
      if (!document.getElementById(cssId))
      {
        var head  = document.getElementsByTagName('head')[0];
        var link  = document.createElement('link');
        link.id   = cssId;
        link.rel  = 'stylesheet';
        link.type = 'text/css';
        link.href = 'fonts/myfonts/impact.css';
        link.media = 'all';
        head.appendChild(link);
      }
    }
  }
})();
