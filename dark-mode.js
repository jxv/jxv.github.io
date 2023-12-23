function setDarkMode() {
  var element = document.body;
  var checkbox = document.getElementById("dark-mode-checkbox");
  element.classList.add('dark-mode', 'notransition');
  checkbox.checked = true;
}

function onToggleDarkMode() {
  var mode = localStorage.getItem('darkmode') || 'light';
  mode = mode == 'dark' ? 'light' : 'dark';
  localStorage.setItem('darkmode', mode);
  var element = document.body;
  element.classList.remove('notransition');
  element.classList.toggle('dark-mode');
  if (typeof onToggleDarkModeCallback !== 'undefined') {
    onToggleDarkModeCallback(mode);
  }
}


document.addEventListener("DOMContentLoaded", (event) => {
  var mode = localStorage.getItem('darkmode') || 'light';
  if (mode == 'dark') {
    setDarkMode();
    if (typeof onLoadDarkMode !== 'undefined') {
      onLoadDarkMode();
    }
  }
});

// window.onload = function() {
