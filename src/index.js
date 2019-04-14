import { Elm } from "./Main";

Elm.Main.init({
  node: document.querySelector("main"),
  flags: process.env.API_KEY
});
