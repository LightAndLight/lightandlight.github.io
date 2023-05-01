function navColor(bool) {
  var nav = document.getElementsByTagName("nav")
  if (bool) {
    nav[0].style.borderColor = "#4f86c6";
  } else {
    nav[0].style.borderColor = "#2f2f2f";
  }
}