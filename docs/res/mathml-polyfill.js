/*
This script is a derivative of
https://github.com/fred-wang/mathml-warning.js/blob/c89965e7c432f6c2d7b0904d23e21859d0cb183d/mpadded.js,
which is distributed under MPL v2 (http://mozilla.org/MPL/2.0/)
*/

"use strict";

var scriptId = "mathml-polyfill-script";
var warningId = "mathml-warning";

var rememberId = "remember-mathml-preference";

function closeWarning() {
  document.getElementById(scriptId).parentElement.removeChild(document.getElementById(warningId));
}

function submitChoice() {
  var choices = document.getElementsByName("choice");
  var selected = Array.from(choices).filter((item) => item.checked)[0];

  if (selected !== undefined) {
    var rememberCheckbox = document.getElementById(rememberId);
    var remember = rememberCheckbox === null || rememberCheckbox === undefined ? false : rememberCheckbox.checked;

    var choice = selected.value;
    if (handleChoice(choice) && remember) {
      document.cookie = "MathMLFallback=" + choice + ";path=/;max-age=" + 30 * 24 * 3600;
    }
  }

  closeWarning();
}

function handleChoice(choice) {
  var success = true;

  switch (choice) {
    case "css":
      var el = document.createElement("link");
      el.href = "https://fred-wang.github.io/mathml.css/mathml.css";
      el.rel = "stylesheet";
      document.head.appendChild(el);
      break;
    case "js":
      var el = document.createElement("script");
      el.src = "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=MML_HTMLorMML";
      document.head.appendChild(el);
      break;
    case "nothing":
      break;
    default:
      console.warn(`${choice} is not a valid choice`);
      success = false;
      break;
  }

  return success;
}

var box, div, namespaceURI;
namespaceURI = "http://www.w3.org/1998/Math/MathML";
// Create a div to test mpadded, using Kuma's "offscreen" CSS
document.body.insertAdjacentHTML(
  "afterbegin",
  "<div style='border: 0; clip: rect(0 0 0 0); height: 1px; margin: -1px; overflow: hidden; padding: 0; position: absolute; width: 1px;'>" +
  "<math xmlns='" + namespaceURI + "'>" +
  "<mpadded height='23px' width='77px'></mpadded>" +
  "</math>" +
  "</div>"
);

div = document.body.firstChild;
box = div.firstChild.firstChild.getBoundingClientRect();
document.body.removeChild(div);

if (Math.abs(box.height - 23) > 1 || Math.abs(box.width - 77) > 1) {
  if (document.cookie) {
    handleChoice(document.cookie.replace(/^MathMLFallback=([a-z]+)$/, "$1"));
  } else {
    var script = document.getElementById(scriptId);

    script.insertAdjacentHTML(
      "afterend",
      "<div id='" + warningId + "' style='display: flex; flex-direction: column; align-items: center; border: 2px solid orange; box-shadow: 0 0 1em gold; padding: 10px; margin: 0; top: 0; width: 95%; background: #fcf6d4;'>" +
      "<style scoped='scoped'>div { font-family: sans; } button { background: #ffd; }</style>" +
      "<p>This page uses <a href='https://developer.mozilla.org/en-US/docs/Web/MathML'>MathML</a>, which your browser doesn't fully support.</p>" +

      "<div>" +

      "<div>" +
      "<input type='radio' name='choice' value='js' id='mathml-preference-js' checked />" +
      "<label for='mathml-preference-js'>Load MathJax (<a href='https://www.mathjax.org/'>info</a>)</label>" +
      "</div>" +

      "<div>" +
      "<input type='radio' name='choice' value='css' id='mathml-preference-css' />" +
      "<label for='mathml-preference-css'>Apply mathml.css (<a href='https://github.com/fred-wang/mathml.css/'>info</a>)</label>" +
      "</div>" +

      "<div>" +
      "<input type='radio' name='choice' value='nothing' id='mathml-preference-nothing' />" +
      "<label for='mathml-preference-nothing'>Do nothing</label>" +
      "</div>" +

      "<div style='padding-top: 0.5em; padding-bottom: 0.5em;'>" +
      "<input type='checkbox' id='" + rememberId + "' />" +
      "<label for='" + rememberId + "'>Remember choice for 30 days</label>" +
      "</div>" +

      "<div style='text-align: center;'>" +
      "<input type='button' value='Ignore' onclick='closeWarning()' />" +
      "<input type='button' value='Save' style='margin-left: 1em;' onclick='submitChoice()' />" +
      "</div>" +

      "</div>"
    );
  }
}