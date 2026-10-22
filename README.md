# PureScript drawing demo

A small browser drawing demo written in PureScript. The `/algo/svg`, `/algo/canvas`,
and `/algo/purescript-drawing` routes render the algorithm state on their drawing
surfaces. Use the left-side **Step algorithm** button to advance the algorithm;
its state is shared while switching between these routes. Their `/demo/...`
counterparts render the SVG, Canvas, and PureScript Drawing examples.

The `/collonade/...` routes draw a stacked collonade centered on the surface,
with the lower diameter of the bottom row's columns 10px. Use the left-side bar
to choose the orders from bottom to top and the number of columns.

## Run locally

Requirements: Node.js 20 or newer and npm. The PureScript compiler, Spago, and
Vite are installed as project dependencies.

```sh
npm install
npm run dev
```

Open the local URL printed by Vite (by default, <http://127.0.0.1:5173/>).
The home page links to the `/algo`, `/demo`, and `/collonade` sections; each
of these opens its `/svg` page.
The first build downloads the PureScript packages listed in `spago.yaml`.

To run the dev server and rebuild/reload the page when PureScript sources
change, run `npm run watch`.

To make a production build, run `npm run build`. To serve that build locally,
run `npm run preview`.
