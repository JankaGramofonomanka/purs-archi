const updateRoute = onRouteChange => {
  const path = window.location.pathname.replace(/\/+$/, "") || "/";
  const routes = {
    "/svg": "SVG",
    "/canvas": "Canvas",
    "/purescript-drawing": "PureScript Drawing"
  };
  const route = routes[path] === undefined ? "/" : path;
  currentRoute = route;

  const homePage = document.getElementById("home-page");
  const demoPage = document.getElementById("demo-page");
  if (!(homePage instanceof HTMLElement) || !(demoPage instanceof HTMLElement)) {
    throw new Error("Missing page container");
  }
  homePage.hidden = route !== "/";
  demoPage.hidden = route === "/";

  for (const page of document.querySelectorAll("[data-demo-page]")) {
    if (!(page instanceof HTMLElement)) {
      throw new Error("Invalid demo page");
    }
    page.hidden = page.dataset.demoPage !== route;
  }

  resizeCanvasForRoute(route);

  const routeLinks = document.querySelectorAll(".route-link");
  routeLinks.forEach(link => {
    if (!(link instanceof HTMLAnchorElement)) {
      throw new Error("Invalid route link");
    }

    if (link.dataset.route === route) {
      link.setAttribute("aria-current", "page");
    } else {
      link.removeAttribute("aria-current");
    }
  });

  onRouteChange(route)();
};

let currentRoute = "/";

const resizeCanvasForRoute = route => {
  const canvasIds = {
    "/canvas": "canvas-target",
    "/purescript-drawing": "drawing-target"
  };
  const canvasId = canvasIds[route];
  if (canvasId === undefined) {
    return;
  }

  const canvas = document.getElementById(canvasId);
  if (!(canvas instanceof HTMLCanvasElement)) {
    throw new Error(`Missing #${canvasId} canvas`);
  }

  const bounds = canvas.getBoundingClientRect();
  const width = Math.round(bounds.width);
  const height = Math.round(bounds.height);
  if (width === 0 || height === 0) {
    throw new Error(`Canvas #${canvasId} has no visible drawing area`);
  }

  if (canvas.width !== width || canvas.height !== height) {
    canvas.width = width;
    canvas.height = height;
  }
};

export const setupRoutes = onRouteChange => () => {
  updateRoute(onRouteChange);
  window.addEventListener("popstate", () => updateRoute(onRouteChange));
  window.addEventListener("resize", () => {
    resizeCanvasForRoute(currentRoute);
    onRouteChange(currentRoute)();
  });

  const routeNav = document.querySelector(".route-nav");
  if (!(routeNav instanceof HTMLElement)) {
    throw new Error("Missing .route-nav element");
  }

  routeNav.addEventListener("click", event => {
    if (
      !(event.target instanceof Element) ||
      event.defaultPrevented ||
      event.button !== 0 ||
      event.metaKey ||
      event.ctrlKey ||
      event.shiftKey ||
      event.altKey
    ) {
      return;
    }

    const link = event.target.closest("a[data-route]");
    if (
      !(link instanceof HTMLAnchorElement) ||
      link.target !== "" ||
      link.hasAttribute("download") ||
      link.origin !== window.location.origin
    ) {
      return;
    }

    event.preventDefault();
    window.history.pushState({}, "", link.href);
    updateRoute(onRouteChange);
  });
};

export const setCount = count => () => {
  const output = document.getElementById("count");
  if (!(output instanceof HTMLElement)) {
    throw new Error("Missing #count element");
  }

  output.textContent = String(count);
};

export const onIncrement = handler => () => {
  const button = document.getElementById("increment");
  if (!(button instanceof HTMLButtonElement)) {
    throw new Error("Missing #increment button");
  }

  button.addEventListener("click", handler);
};
