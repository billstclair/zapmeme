//////////////////////////////////////////////////////////////////////
//
// svg-to-data-url.js
// Define the `svg-to-data-url` custom element.
// Also define the SVG.toDataURL() function.
// Copyright (c) 2019 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE
//
// SVGElement.toDataUrl code mostly Copyright (c) Samuli Kaipiainen.
//
//////////////////////////////////////////////////////////////////////

/**
   The missing SVG.toDataURL library for your SVG elements.

   Plus an Elm <svg-to-data-url> custom Html element that uses it.
   
   Usage: SVGElement.toDataURL( type, { options } )

   Returns: the data URL, except when using native PNG renderer (needs callback).

   type	MIME type of the exported data.
     Default: image/svg+xml.
     Data URL: image/png, image/jpeg
     File object: File.

   options is a map of options: {
     callback: function(dataURL)
       Callback function which is called when the data URL is ready.
       This is only necessary when using native PNG renderer.
       Default: undefined.
   
   [the rest of the options only apply when type="image/png" or "File"]

     mimeType : String
       If type is "File", then this should be "image/png" or "image/jpeg".
       Default: "image/png"

     fileName: String
       If type is "File", this goes in the name property of the resulting
       File object.

     renderer: "native"|"canvg"
       PNG renderer to use. Native renderer¹ might cause a security exception.
       Default: canvg if available, otherwise native.

     keepNonSafe: true|false
       Export non-safe (image and foreignObject) elements.
       This will set the Canvas origin-clean property to false, if this data is transferred to Canvas.
       Default: false, to keep origin-clean true.
       NOTE: not currently supported and is just ignored.

     keepOutsideViewport: true|false
       Export all drawn content, even if not visible.
       Default: false, export only visible viewport, similar to Canvas toDataURL().
       NOTE: only supported with canvg renderer.
   }

   See original paper¹ for more info on SVG to Canvas exporting.

   ¹ http://svgopen.org/2010/papers/62-From_SVG_to_Canvas_and_Back/#svg_to_canvas
*/

(function() {
  
// <svg-to-data-url> custom element.
customElements.define('svg-to-data-url', class extends HTMLElement {
  constructor() {
    super();
    this._triggerReturnedUrl = null;
    this._triggerReturnedFile = null;
  }

  connectedCallback() {
    // Move along. Nothing to see here.
  }

  get returnedUrl() {
    return this._returnedUrl;
  }

  get returnedFile() {
    return this._returnedFile;
  }

  get returnedUrlParamaters() {
    return this._returnedUrlParameters;
  }

  set returnedUrlParameters(object) {
    this._returnedUrlParameters = object;
  }

  get triggerReturnedUrl() {
    return this._triggerReturnedUrl;
  }

  set triggerReturnedUrl(trigger) {
    // Don't trigger on first set.
    var doit = this._triggerReturnedUrl !== null
        && this._triggerReturnedUrl != trigger
    this._triggerReturnedUrl = trigger;
    if (doit) {
      var parameters = this._returnedUrlParameters;
      if (typeof(parameters) == 'object') {
        var svgId = parameters.svgId;
        var mimeType = parameters.mimeType;

        this._returnedUrl = { svgId : svgId,
                              mimeType: mimeType
                            }
        var that = this;
        function dispatch() {
          that.dispatchEvent(new CustomEvent('returnedUrl'));
        }

        var svg = getSvgElement(svgId);
        var tagName = svg && svg.tagName
        if (typeof(tagName) == "string") {
          if (tagName == "svg") {
            function callback(url) {
              that._returnedUrl.url = url;
              window.setTimeout(dispatch, 1)
            }
            console.log('svg:', svg);
            svg.toDataURL(mimeType, { callback: callback });
          }
        }
      }
    }
  }

  get returnedFileParamaters() {
    return this._returnedFileParameters;
  }

  set returnedFileParameters(object) {
    this._returnedFileParameters = object;
  }

  get triggerReturnedFile() {
    return this._triggerReturnedFile;
  }

  set triggerReturnedFile(trigger) {
    // Don't trigger on first set.
    var doit = this._triggerReturnedFile !== null
            && this._triggerReturnedFile != trigger
    this._triggerReturnedFile = trigger;
    if (doit) {
      var parameters = this._returnedFileParameters;
      if (typeof(parameters) == 'object') {
        var svgId = parameters.svgId;
        var fileName = parameters.fileName
        var mimeType = parameters.mimeType;

        var a = document.createElement('a');
        this._returnedFile = { svgId: svgId,
                               fileName: fileName,
                               mimeType: mimeType,
                               canDownload: a.download == ''
                             }
        var that = this;
        function dispatch() {
          that.dispatchEvent(new CustomEvent('returnedFile'));
        }

        var svg = getSvgElement(svgId);
        var tagName = svg && svg.tagName
        if (typeof(tagName) == "string") {
          if (tagName == "svg") {
            function callback(file) {
              that._returnedFile.file = file;
              window.setTimeout(dispatch, 1)
            }
            svg.toDataURL('File', { callback: callback,
                                    fileName: fileName,
                                    mimeType: mimeType
                                  });
          }
        }
      }
    }
  }
})

function getSvgElement(svgId) {
  var element = document.getElementById(svgId);
  if (typeof(element) == 'object') {
    return element;
  }
}

})();  // execute function surrounding all the code above


// SVG.toDataUrl definition
SVGElement.prototype.toDataURL = function(type, options) {
  var _svg = this;
  
  function debug(s) {
    //console.log("SVG.toDataURL:", s);
  }

  function exportSVG() {
    var svg_xml = XMLSerialize(_svg);
    var svg_dataurl = base64dataURLencode(svg_xml);
    debug(type + " length: " + svg_dataurl.length);

    // NOTE double data carrier
    if (options.callback) options.callback(svg_dataurl);
    return svg_dataurl;
  }

  function XMLSerialize(svg) {

    // quick-n-serialize an SVG dom, needed for IE9 where there's no XMLSerializer nor SVG.xml
    // s: SVG dom, which is the <svg> elemennt
    function XMLSerializerForIE(s) {
      var out = "";
      
      out += "<" + s.nodeName;
      for (var n = 0; n < s.attributes.length; n++) {
	out += " " + s.attributes[n].name + "=" + "'" + s.attributes[n].value + "'";
      }
      
      if (s.hasChildNodes()) {
	out += ">\n";

	for (var n = 0; n < s.childNodes.length; n++) {
	  out += XMLSerializerForIE(s.childNodes[n]);
	}

	out += "</" + s.nodeName + ">" + "\n";

      } else out += " />\n";

      return out;
    }

    
    if (window.XMLSerializer) {
      debug("using standard XMLSerializer.serializeToString")
      return (new XMLSerializer()).serializeToString(svg);
    } else {
      debug("using custom XMLSerializerForIE")
      return XMLSerializerForIE(svg);
    }
    
  }

  function base64dataURLencode(s) {
    var b64 = "data:image/svg+xml;base64,";

    // https://developer.mozilla.org/en/DOM/window.btoa
    if (window.btoa) {
      debug("using window.btoa for base64 encoding");
      b64 += btoa(s);
    } else {
      debug("using custom base64 encoder");
      b64 += Base64.encode(s);
    }
    
    return b64;
  }

  // Harel Levy
  // https://stackoverflow.com/a/43358515/1386989
  function dataURLtoFile(dataurl, filename) {
    var arr = dataurl.split(','), mime = arr[0].match(/:(.*?);/)[1],
        bstr = atob(arr[1]), n = bstr.length, u8arr = new Uint8Array(n);
    while(n--){
      u8arr[n] = bstr.charCodeAt(n);
    }
    return new File([u8arr], filename, {type:mime});
  }

  function exportPNG(type, mimeType, fileName) {
    if (!mimeType) {
      mimeType = 'image/png';
    }

    var canvas = document.createElement("canvas");
    var ctx = canvas.getContext('2d');

    // TODO: if (options.keepOutsideViewport), do some translation magic?

    var svg_img = new Image();
    var svg_xml = XMLSerialize(_svg);
    svg_img.src = base64dataURLencode(svg_xml);

    svg_img.onload = function() {
      debug("exported image size: " + [svg_img.width, svg_img.height])
      canvas.width = svg_img.width;
      canvas.height = svg_img.height;
      ctx.drawImage(svg_img, 0, 0);

      // SECURITY_ERR WILL HAPPEN NOW
      var png_dataurl = canvas.toDataURL(mimeType);
      debug(type + " length: " + png_dataurl.length);

      if (type == 'File') {
        png_dataurl = dataURLtoFile(png_dataurl, fileName);
      }

      if (options.callback) options.callback( png_dataurl );
      else debug("WARNING: no callback set, so nothing happens.");
    }
    
    svg_img.onerror = function() {
      console.log(
	"Can't export! Maybe your browser doesn't support " +
	  "SVG in img element or SVG input for Canvas drawImage?\n" +
	  "http://en.wikipedia.org/wiki/SVG#Native_support"
      );
    }

    // NOTE: will not return anything
  }

  function exportPNGcanvg() {
    var canvas = document.createElement("canvas");
    var ctx = canvas.getContext('2d');
    var svg_xml = XMLSerialize(_svg);

    // NOTE: canvg gets the SVG element dimensions incorrectly if not specified as attributes
    //debug("detected svg dimensions " + [_svg.clientWidth, _svg.clientHeight])
    //debug("canvas dimensions " + [canvas.width, canvas.height])

    var keepBB = options.keepOutsideViewport;
    if (keepBB) var bb = _svg.getBBox();

    // NOTE: this canvg call is synchronous and blocks
    canvg(canvas, svg_xml, { 
      ignoreMouse: true, ignoreAnimation: true,
      offsetX: keepBB ? -bb.x : undefined, 
      offsetY: keepBB ? -bb.y : undefined,
      scaleWidth: keepBB ? bb.width+bb.x : undefined,
      scaleHeight: keepBB ? bb.height+bb.y : undefined,
      renderCallback: function() {
	debug("exported image dimensions " + [canvas.width, canvas.height]);
	var png_dataurl = canvas.toDataURL();
	debug(type + " length: " + png_dataurl.length);
	
	if (options.callback) options.callback( png_dataurl );
      }
    });

    // NOTE: return in addition to callback
    return canvas.toDataURL();
  }

  // BEGIN MAIN

  if (!type) type = "image/svg+xml";
  if (!options) options = {};

  if (options.keepNonSafe) debug("NOTE: keepNonSafe is NOT supported and will be ignored!");
  if (options.keepOutsideViewport) debug("NOTE: keepOutsideViewport is only supported with canvg exporter.");
  
  switch (type) {
  case "image/svg+xml":
    return exportSVG();
    break;

  case "image/png":
  case "image/jpeg":
  case "File":

    var mimeType = type == "File" ? options.mimeType : type;

    if (!options.renderer) {
      if (window.canvg) options.renderer = "canvg";
      else options.renderer="native";
    }

    switch (options.renderer) {
    case "canvg":
      debug("using canvg renderer for png export");
      return exportPNGcanvg();
      break;

    case "native":
      debug("using native renderer for png export. THIS MIGHT FAIL.");
      return exportPNG(type, mimeType, options.fileName);
      break;

    default:
      debug("unknown png renderer given, doing noting (" + options.renderer + ")");
    }

    break;

  default:
    debug("Sorry! Exporting as '" + type + "' is not supported!")
  }
}
