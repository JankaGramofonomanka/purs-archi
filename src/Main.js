const updateRoute = onRouteChange => {
  const path = window.location.pathname.replace(/\/+$/, "") || "/";
  const routes = {
    "/svg": "SVG",
    "/demo/svg": "SVG Demo",
    "/canvas": "Canvas",
    "/demo/canvas": "Canvas Demo",
    "/purescript-drawing": "PureScript Drawing",
    "/demo/purescript-drawing": "PureScript Drawing Demo"
  };
  const route = Object.hasOwn(routes, path) ? path : "/svg";
  if (path !== route) {
    window.history.replaceState({}, "", route);
  }
  currentRoute = route;
  const demoPage = getDemoPage(route);

  for (const page of document.querySelectorAll("[data-demo-page]")) {
    if (!(page instanceof HTMLElement)) {
      throw new Error("Invalid demo page");
    }
    page.hidden = page.dataset.demoPage !== demoPage;
  }

  resizeCanvasForRoute(route);

  const routeLinks = document.querySelectorAll(".route-link");
  routeLinks.forEach(link => {
    if (!(link instanceof HTMLAnchorElement)) {
      throw new Error("Invalid route link");
    }

    const pageRoute = link.dataset.page;
    if (pageRoute === undefined) {
      throw new Error("Route link is missing its page");
    }
    const linkRoute = route.startsWith("/demo/") ? `/demo${pageRoute}` : pageRoute;
    link.dataset.route = linkRoute;
    link.href = linkRoute;

    if (pageRoute === demoPage) {
      link.setAttribute("aria-current", "page");
    } else {
      link.removeAttribute("aria-current");
    }
  });

  onRouteChange(route)();
};

let currentRoute = "/";

const getDemoPage = route => route.startsWith("/demo/") ? route.slice(5) : route;

const resizeCanvasForRoute = route => {
  const page = getDemoPage(route);
  const canvasIds = {
    "/canvas": "canvas-target",
    "/purescript-drawing": "drawing-target"
  };
  const canvasId = canvasIds[page];
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
