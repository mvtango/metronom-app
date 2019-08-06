import './main.css';
import '@fortawesome/fontawesome-free/js/all.js';
import { Howl, Howler } from 'howler/dist/howler.js'; 
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

// https://github.com/bholtbholt/step-sequencer/blob/master/src/elmPorts.js#L4
Howler.autosuspend = false;

const samples = {
      "strike": new Howl({ src: ['./samples/clap-strike.mp3'] }),
      "in":     new Howl({ src: ['./samples/kiss-in.mp3'] }),
      "out":    new Howl({ src: ['./samples/snap-out.mp3'] })
};



const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: { "todos" : [] }
});

app.ports.playSample.subscribe(function(sample) {
    // console.log("Sample: "+sample);
    samples[sample].play();

});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
