import React from 'react';
import ReactDOM from 'react-dom';

import Main from './output/Main';

function main() {
  ReactDOM.render(
    <div>
      <h1>Traced</h1>
      <Main.tracedReactComponent />
      <h1>Store</h1>
      <Main.storeReactComponent />
      <h1>Moore</h1>
      <Main.mooreReactComponent />
      <h1>Env</h1>
      <Main.envReactComponent />
    </div>,
    document.getElementById('app')
  );
}

if (module.hot) {
  module.hot.accept(function() {
    console.log('running main again');
    main();
  });
}

console.log('starting');
main();
