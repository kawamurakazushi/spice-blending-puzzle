import { Elm } from "./Main";

const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: ""
});

app.ports.scrollToTop.subscribe(function() {
  window.scrollTo(0, 0);
});
