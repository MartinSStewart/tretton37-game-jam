import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

let app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.portOut.subscribe( ( message ) => {
    let messageType = message.Constructor;
    switch (messageType)
    {
        case "PlaySound":
            let messageBody = message.A1;
            var sound = new Audio("../" + messageBody.soundName);
            sound.currentTime = 0;
            sound.loop = messageBody.loop;
            sound.play();
            break;
        default:
            console.log("Message type \"" + messageType + "\" was not handled.");
            debugger;
            break;
    }
});

registerServiceWorker();
