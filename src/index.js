import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var storedState = localStorage.getItem('elm:state');
var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: JSON.parse(storedState)
});
app.ports.setStorage.subscribe(function(state) {
    localStorage.setItem('elm:state', JSON.stringify(state));
});

registerServiceWorker();
