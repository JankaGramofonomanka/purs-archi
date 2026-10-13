# PureScript drawing demo

A small browser drawing demo written in PureScript. The `/svg`, `/canvas`, and
`/purescript-drawing` routes render the algorithm state on their drawing
surfaces. Use the left-side **Step algorithm** button to advance the algorithm;
its state is shared while switching between these routes. Their `/demo/...`
counterparts render the SVG, Canvas, and PureScript Drawing examples.

## Run locally

Requirements: Node.js 20 or newer and npm. The PureScript compiler, Spago, and
Vite are installed as project dependencies.

```sh
npm install
npm run dev
```

Open the local URL printed by Vite (by default, <http://127.0.0.1:5173/>).
The home page redirects to `/svg`.
The first build downloads the PureScript packages listed in `spago.yaml`.

To run the dev server and rebuild/reload the page when PureScript sources
change, run `npm run watch`.

To make a production build, run `npm run build`. To serve that build locally,
run `npm run preview`.
