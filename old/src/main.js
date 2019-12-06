const refocus = () => document.getElementById('currentInput').focus();
setInterval(refocus, 100);

require('./App.re').main(document.getElementById('terminal'));
