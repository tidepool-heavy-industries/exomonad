import { type Component } from "solid-js";
import { Router, Route } from "@solidjs/router";
import ChatView from "./views/ChatView";
import GraphView from "./views/GraphView";

const App: Component = () => {
  return (
    <Router>
      <Route path="/" component={ChatView} />
      <Route path="/graph" component={GraphView} />
    </Router>
  );
};

export default App;
