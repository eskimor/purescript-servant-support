/* JsUtils exports */
"use strict";

// module JsUtils

exports.encodeUriComponent = encodeURIComponent;

exports.unsafeToString = function (obj) {
  return JSON.stringify(obj, null, 4)
}